#' @title Full, direct and indirect network results for MSC
#' 
#' @description Compute all pairwise comparisons for the full network, all direct comparisons, or all indirect.
#'
#' @param ps A set of aggregated performance estimates, see \code{\link{aggregate_performance}}.
#' @param mtype Type of model (default "consistency", else "inconsistency"). It is sufficient to write \code{"c"} or \code{"i"}.
#' @param verbose If TRUE, results of each model will be printed (default FALSE)
#' @param ... Other arguments to be passed to \code{\link[metafor]{rma.mv}}. See also \code{\link{consistency}}.
#'
#' @return A list of class \code{msc}, with the following components:
#' \describe{
#'   \item{table}{A tibble containing the model results for each pair}
#'   \item{models}{A list with all the individual pairwise models results (\code{\link[metafor]{rma.mv}})}
#'   \item{type}{"Full", "indirect" or "direct"?}
#'   \item{performance}{Label of performance score, as given in, for example, \code{\link{compute_performance}}.}
#' } 
#'
#' @examples
#' dat <- msc_sample_data()
#' bssamp <- get_bs_samples(dat, id, cohort, outcome, n.samples = 10, a, b, c, d, e)
#' perf <- compute_performance(bssamp, fn = calibration_slope, lbl = "CS")
#' agg <- aggregate_performance(perf)
#' msc_indirect(agg, mtype = "inconsistency")
#' msc_direct(agg, mtype = "inconsistency")
#' full <- msc_full(agg, mtype = "inconsistency")
#' plot(full)
#' 
#' @export
#' @describeIn msc Compute all pairwise comparisons in full network of evidence, as well as direct and indirect comparisons
msc_full <- function(ps, mtype = c("consistency", "inconsistency")[1], verbose = FALSE, ...){
    mt <- match.arg(mtype, c("consistency", "inconsistency"))
    res.nw <- msc_network(ps, mtype = mt, ...)
    res.d <- msc_direct(ps, mtype = mt, verbose = verbose, ...)
    res.i <- msc_indirect(ps, mtype = mt, verbose = verbose, ...)
    outtab <- bind_rows(res.nw$table, res.d$table, res.i$table) %>%
        mutate(type = factor(type, c("network", "direct", "indirect"))) %>% 
        arrange(s1, s2, type) %>%
        mutate(measure = ps$lbl)
    modelresults <- c(res.nw$models, res.d$models, res.i$models)
    out <- list("table" = outtab,
                "models" = modelresults, 
                "type" = "full", 
                "performance" = ps$lbl)
    class(out) <- "msc"
    out
}

#' @describeIn msc Compute all pairwise comparisons in the full network of evidence
#' @export
msc_network <- function(ps, mtype = c("consistency", "inconsistency")[1], ...){
    scores <- ps$scores
    mt <- match.arg(mtype, c("consistency", "inconsistency"))
    modelfn <- switch(mt, 
                      consistency = consistency,
                      inconsistency = inconsistency)
    this.ss <- aggregate_performance(ps, reference = ps$scores[1])
    x <- modelfn(this.ss, ...)
    #print(coeftable(x))
    scorepairs <- expand.grid(x1 = scores, x2 = scores)
    scorepairs$x1 <- factor(scorepairs$x1, scores)
    scorepairs$x2 <- factor(scorepairs$x2, scores)
    to.include <- with(scorepairs, x1 != x2 & as.numeric(x2) > as.numeric(x1))
    scorepairs <- scorepairs[to.include, ]
    pairmat <- (model.matrix(~ scorepairs$x2) - model.matrix( ~ scorepairs$x1))[, -1]
    colnames(pairmat) <- scores[-1]
    rownames(pairmat) <- with(scorepairs, paste(x2, x1, sep = "-"))
    
    est <- as.vector(pairmat %*% x$beta)
    var.est <- pairmat %*% x$vb %*% t(pairmat)
    ci.lb <- as.vector(est - qnorm(1 - 0.05 / 2) * sqrt(diag(var.est)))
    ci.ub <- as.vector(est + qnorm(1 - 0.05 / 2) * sqrt(diag(var.est)))
    pval <- as.vector(pnorm(-abs(est / sqrt(diag(var.est)))) * 2)
    #list("estimate" = est, "ci.lb" = ci.lb, "ci.ub" = ci.ub, "var.estimate" = var.est)
    out <- tibble(s1 = scorepairs$x1,
                  s2 = scorepairs$x2,
                  model = x$model,
                  type = "network",
                  estimate = est,
                  ci.lb = ci.lb,
                  ci.ub = ci.ub,
                  pval = pval, measure = ps$lbl) %>%
        mutate(s1 = factor(s1, scores),
               s2 = factor(s2, scores))
    out <- list("table" = out,
                "models" = list("network" = x),
                "type" = "network", 
                "performance" = ps$lbl)
    class(out) <- "msc"
    out
}


#' @describeIn msc Compute all pairwise indirect comparisons
#' @export
msc_indirect <- function(ps, mtype = c("consistency", "inconsistency")[1], verbose = FALSE, ...){
    # node splitting approach to estimating direct and indirect differences
    # takes result of compute_performance as argument
    
    # for indirect comparisons (node-splitting approach):
    # if pair not in design --> leave it in as is
    # if pair == design --> remove entire study
    # if pair in design (but there are extra scores also) --> remove 2nd of pair
    scores <- ps$scores
    we <- ps$working.estimates
    
    mt <- match.arg(mtype, c("consistency", "inconsistency"))
    modelfn <- switch(mt, 
                      consistency = consistency,
                      inconsistency = inconsistency)
    
    # make list of pairwise comparisons
    listpairs <- combn(scores, 2)
    datout <- tibble(s1 = listpairs[1, ], 
                     s2 = listpairs[2, ],
                     model = mt,
                     type = "indirect",
                     estimate = NA,
                     ci.lb = NA,
                     ci.ub = NA,
                     pval = NA, 
                     measure = ps$lbl)
    modelresults <- vector("list", ncol(listpairs))
    names(modelresults) <- paste(apply(listpairs, 2, paste, collapse = "-"), "indirect")
    
    # for each comparison...
    for(i in 1:ncol(listpairs)){
        if(!verbose) cat(".")
        #https://edwinth.github.io/blog/dplyr-recipes/
        s1 <- rlang::sym(s1c <- listpairs[1, i])
        s2 <- rlang::sym(s2c <- listpairs[2, i])
        if(verbose) print(c(s1c, s2c))
        other.scores <- scores[!grepl(paste(c(s1c, s2c), collapse = "|"), scores)]
        # 1. keep only "working.estimates" with both s1 and s2, drop other scores% 
        this.designs <- we %>%
            filter(id == "Apparent") %>%
            select(-id, -type, -measure, -ref, -k) %>%
            group_by(cohort) %>%
            gather(scores, key = "score", value = "value") %>%
            mutate(score = factor(score, scores)) %>%
            arrange(cohort, score) %>%
            summarize(design = paste(score[!is.na(value)], collapse = "-"))
        this.we <- we %>%
            full_join(this.designs, by = "cohort") %>%
            mutate(has1 = grepl(s1c, design),
                   has2 = grepl(s2c, design),
                   has.others = grepl(paste(other.scores, collapse = "|"), design)) %>%
            # see, for example, https://dplyr.tidyverse.org/articles/programming.html for !!var := ...
            mutate(!!s1 := case_when(
                has1 & has2 & !has.others ~ NA_real_,
                has1 & has2 & has.others ~ !!s1,
                !(has1 & has2) ~ !!s1
            ),
            !!s2 := case_when(
                has1 & has2 & !has.others ~ NA_real_,
                has1 & has2 & has.others ~ NA_real_,
                !(has1 & has2) ~ !!s2
            ),
            k = ifelse(has1 & has2 & !has.others, 0, k)) %>%
            select(-has1, -has2, -has.others, -design)
        
        indirect_evidence <- (nrow(this.we) > 0) & 
            (mean(!is.na(this.we[, s1c])) > 0) & 
            (mean(!is.na(this.we[, s2c])) > 0)
        
        if(indirect_evidence){
            this.ps <- ps
            this.ps$working.estimates <- this.we
            this.ps$scores <- scores
            this.ss <- aggregate_performance(this.ps, reference = s1c)
            # 2. estimate model with [in]consistency()
            this.model <- try(modelfn(this.ss, ...), silent = TRUE)
            modelresults[[i]] <- this.model
            if(verbose) print(coeftable(this.model))
            # 3. save results
            if(any(class(this.model) == "try-error")){
                datout[i, 5:8] <- NA 
            } else {
                this.out <- with(this.model, cbind(beta, ci.lb, ci.ub, pval))
                names.out <- rownames(this.out)
                pick.names <- names.out[names.out %in% c(s2c, "mods")]
                datout[i, 5:8] <- this.out[pick.names, ]
            }
        } else {
            datout[i, 5:8] <- NA 
        }
        if(verbose) print(datout[i, ])
    }
    
    
    out <- list("table" = datout %>%
                    mutate(s1 = factor(s1, scores),
                           s2 = factor(s2, scores)),
                "models" = modelresults, 
                "type" = "indirect", 
                "performance" = ps$lbl)
    class(out) <- c("msc")
    out
}

#' @describeIn msc Compute all pairwise indirect comparisons
#' @export
msc_direct <- function(ps, mtype = c("consistency", "inconsistency")[1], verbose = FALSE, ...){
    # keep only if (pair %in% design)
    scores <- ps$scores
    we <- ps$working.estimates
    
    mt <- match.arg(mtype, c("consistency", "inconsistency"))
    modelfn <- switch(mt,
                      consistency = consistency,
                      inconsistency = inconsistency)
    
    # make list of pairwise comparisons
    listpairs <- combn(scores, 2)
    datout <- tibble(s1 = listpairs[1, ],
                     s2 = listpairs[2, ],
                     model = mt,
                     type = "direct",
                     estimate = NA,
                     ci.lb = NA,
                     ci.ub = NA,
                     pval = NA, 
                     measure = ps$lbl)
    modelresults <- vector("list", ncol(listpairs))
    names(modelresults) <- paste(apply(listpairs, 2, paste, collapse = "-"), "direct")
    
    # for each comparison...
    for(i in 1:ncol(listpairs)){
        if(!verbose) cat(".")
        #https://edwinth.github.io/blog/dplyr-recipes/
        s1 <- rlang::sym(s1c <- listpairs[1, i])
        s2 <- rlang::sym(s2c <- listpairs[2, i])
        if(verbose) print(c(s1c, s2c))
        other.scores <- scores[!grepl(paste(c(s1c, s2c), collapse = "|"), scores)]
        # 1. keep only "working.estimates" with both s1 and s2, drop other scores% 
        this.designs <- we %>%
            filter(id == "Apparent") %>%
            select(-id, -type, -measure, -ref, -k) %>%
            group_by(cohort) %>%
            gather(scores, key = "score", value = "value") %>%
            mutate(score = factor(score, scores)) %>%
            arrange(cohort, score) %>%
            summarize(design = paste(score[!is.na(value)], collapse = "-"))
        this.we <- we %>%
            full_join(this.designs, by = "cohort") %>%
            #filter(type == "apparent") %>%
            mutate(has1 = grepl(s1c, design),
                   has2 = grepl(s2c, design),
                   has.others = grepl(paste(other.scores, collapse = "|"), design)) %>%
            # see, for example, https://dplyr.tidyverse.org/articles/programming.html for !!var := ...
            mutate(k = has1 + has2,
                   ref = which(scores == s1c)) %>%
            mutate_at(other.scores, ~ NA) %>%
            select(-has1, -has2, -has.others, -design) %>%
            filter(k == 2)
        if(nrow(this.we) > 0){
            this.ps <- ps
            this.ps$working.estimates <- this.we
            this.ps$scores <- scores
            this.ss <- aggregate_performance(this.ps, reference = s1c)
            # 2. estimate model with [in]consistency()
            this.model <- try(modelfn(this.ss, ...), silent = TRUE)
            modelresults[[i]] <- this.model
            if(verbose) print(coeftable(this.model))
            # 3. save results
            if(any(class(this.model) == "try-error")){
                datout[i, 5:8] <- NA 
            } else {
                this.out <- with(this.model, c(beta, ci.lb, ci.ub, pval))
                datout[i, 5:8] <- this.out
            }
        } else {
            datout[i, 5:8] <- NA 
        }
        if(verbose) print(datout[i, ])
    }
    
    out <- list("table" = datout %>%
                    mutate(s1 = factor(s1, scores),
                           s2 = factor(s2, scores)),
                "models" = modelresults, 
                "type" = "direct", 
                "performance" = ps$lbl)
    class(out) <- "msc"
    out
}

#' @rdname msc
#' @title Print MSC results
#' @param ... In the print function, other options may be specified. These are passed to \code{\link[tibble]{print.tbl}}.
#' @export
print.msc <- function(x,  ...){
    print(x$table, ...)
}

#' @rdname msc
#' @title Simple plots for MSC network comparisons
#' @param compare_to If specified, only comparisons to \code{compare_to} are plotted.
#' @param newlabels A new vector of labels (character) for the scores can be used instead of the current vector of score names.
#' @export
plot.msc <- function(x, compare_to = NULL, newlabels = NULL){
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Package \"ggplot2\" needed for this function to work. Please install it.",
             call. = FALSE)
    }
    require(ggplot2)
    x <- x$table
    
    if(!is.null(compare_to)){
        check1 <- length(compare_to) == 1
        if(!check1) stop("compare_to should be a vector of length 1, for example: ", x$s1[1])
        check2 <- all((compare_to %in% levels(x$s1) | compare_to %in% levels(x$s2)))
        if(!check2) stop("compare_to should be one of the values found in s1 or s2, for example: ", x$s2[1])
        x1 <- x %>%
            filter(s1 == compare_to)
        x2 <- x %>%
            filter(s2 == compare_to)
        x2fix <- tibble(s1 = x2$s2,
                        s2 = x2$s1,
                        model = x2$model,
                        type = x2$type,
                        estimate = -x2$estimate,
                        ci.lb = -x2$ci.ub,
                        ci.ub = -x2$ci.lb,
                        pval = x2$pval,
                        measure = x2$measure)
        x <- bind_rows(x1, x2fix)
    } 
    if(!is.null(newlabels)){
        oldlabels <- levels(x$s1)
        k <- length(oldlabels)
        if(length(newlabels) != length(oldlabels)){
            warning("newlabels must be the same length as levels(x$s1), that is: ", k, ". We will keep the old levels of s1 and s2,")
        } else {
            message("Replacing old s1 and s2 levels (", 
                    paste(oldlabels, collapse = "; "), 
                    ") with newlabels (", 
                    paste(newlabels, collapse = "; "), ")")
            levels(x$s1) <- levels(x$s2) <- newlabels
        }
        
    }
    qplot(s2, estimate, ymin = ci.lb, ymax = ci.ub,
          data = x, color = type, geom = "blank") +
        geom_point(position = position_dodge(width = 0.2)) + 
        geom_linerange(position = position_dodge(width = 0.2)) + 
        facet_wrap( ~ s1) +
        guides(color = guide_legend("")) + 
        ggtitle(x$measure[1], subtitle = x$model[1])
}