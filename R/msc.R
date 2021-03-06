#' @title Full, direct and indirect network results for MSC
#' 
#' @importFrom stats model.matrix pnorm qnorm
#' @importFrom utils combn
#' @import ggplot2
#' 
#' @description Compute all pairwise comparisons for the full network, all direct comparisons, or all indirect.
#'
#' @param ps A set of raw performance estimates, see \code{\link{compute_performance}}. Since we have to compute a range of different aggregated performance measures, we start here with the raw performance estimates.
#' @param mods A vector of variable names that are moderators, that is, covariates which could affect the differences in score performance. See also \code{\link{aggregate_performance}}. The main model in an analysis should probably not include any moderators, but they may be interesting when examining transitivity.
#' @param mtype Type of model (default "consistency", else "inconsistency"). It is sufficient to write \code{"c"} or \code{"i"}.
#' @param ref Reference score, that is, the base score to which performance should be compared. In \code{msc_model}, the default reference score is the first one. In contrast, \code{msc_direct} and \code{msc_indirect} will print all pairwise comparisons unless a reference is specified.
#' @param verbose If TRUE, results of each model will be printed (default FALSE)
#' @param ... In the \code{msc} functions, any other arguments are passed to \code{\link[metafor]{rma.mv}}. See also \code{\link{consistency}}. In the print function, other options are passed to \code{\link{print.data.frame}}. Ignored in the plot function.
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
#' bssamp <- get_bs_samples(dat, id, study, outcome, n.samples = 10, 
#'                   scores = c("a", "b", "c", "d", "e", "f"), 
#'                   moderators = c("age", "female", "x1"))
#' perf <- compute_performance(bssamp, fn = calibration_slope, lbl = "CS")
#' msc_model(perf, mods = "age", mtype = "inconsistency", ref = "b")
#' plot(nw <- msc_network(perf, mods = NULL, mtype = "inconsistency", ref = "b"))
#' \dontrun{
#' msc_direct(perf, mods = "age", mtype = "inconsistency")
#' msc_indirect(perf, mtype = "inconsistency")
#' msc_direct(perf, mtype = "inconsistency")
#' full <- msc_full(perf, mtype = "inconsistency")
#' plot(full)
#' }
#' 
#' @export
#' @describeIn msc Compute all pairwise comparisons in full network of evidence, as well as direct and indirect comparisons
msc_full <- function(ps, mods = NULL, mtype = c("consistency", "inconsistency")[1], ref = NULL, verbose = FALSE, ...){
    if(class(ps) != "mscraw") warning("A set of *raw* performance estimates is needed (class(ps) == 'mscraw'), from compute_performance().")
    mt <- match.arg(mtype, c("consistency", "inconsistency"))
    if(verbose) cat("Calculating network estimates...\n")
    res.nw <- msc_network(ps, mods = mods, mtype = mt, ref = ref, ...)
    if(verbose) cat("Calculating direct estimates...\n")
    res.d <- msc_direct(ps, mods = mods, mtype = mt, ref = ref, verbose = verbose, ...)
    if(verbose) cat("Calculating indirect estimates...\n")
    res.i <- msc_indirect(ps, mods = mods, mtype = mt, ref = ref, verbose = verbose, ...)
    outtab <- rbind(res.nw$table, res.d$table, res.i$table)
    outtab$type <- factor(outtab$type, c("network", "direct", "indirect"))
    ord <- with(outtab, order(s1, s2, type))
    outtab <- outtab[ord, ]
    outtab$measure <- ps$lbl
    modelresults <- c(res.nw$models, res.d$models, res.i$models)
    out <- list("table" = outtab,
                "models" = modelresults, 
                "type" = "full", 
                "performance" = ps$lbl)
    class(out) <- "msc"
    out
}

#' @describeIn msc Compute an MSC model
#' @export
msc_model <- function(ps, mods = NULL, mtype = c("consistency", "inconsistency")[1], ref = NULL, ...){
  if(class(ps) != "mscraw") warning("A set of *raw* performance estimates is needed (class(ps) == 'mscraw'), from compute_performance().")
  scores <- ps$scores
  mt <- match.arg(mtype, c("consistency", "inconsistency"))
  modelfn <- switch(mt, 
                    consistency = consistency,
                    inconsistency = inconsistency)
  this.ref <- ref
  if(is.null(ref)){
    this.ref <- scores[1]
  }
  if(!this.ref %in% scores){
    this.ref <- scores[1]
    message("ref not in list of scores: ", this.ref, " used instead.")
  }
  this.ss <- aggregate_performance(ps, reference = this.ref)
  x <- modelfn(this.ss, mods = mods, ...)
  out <- data.frame(s1 = this.ref,
                    s2 = rownames(x$beta),
                    model = mt,
                    type = "model",
                    estimate = x$beta,
                    ci.lb = x$ci.lb,
                    ci.ub = x$ci.ub,
                    pval = x$pval, 
                    measure = ps$lbl,
                    mods = paste(mods, collapse = ", "))
  out <- list("table" = out,
              "models" = list("network" = x),
              "type" = "network", 
              "performance" = ps$lbl,
              "mods" = mods)
  class(out) <- "msc"
  out
}

#' @describeIn msc Compute all pairwise comparisons in the full network of evidence
#' @export
msc_network <- function(ps, mods = NULL, mtype = c("consistency", "inconsistency")[1], ref = NULL, ...){
    if(class(ps) != "mscraw") warning("A set of *raw* performance estimates is needed (class(ps) == 'mscraw'), from compute_performance().")
    scores <- ps$scores
    
    this.ref <- ref
    if(!is.null(this.ref) && !(this.ref %in% scores)){
      warning("ref not in list of scores, using ", scores[1], " instead")
      this.ref <- scores[1]
    }
    
    mt <- match.arg(mtype, c("consistency", "inconsistency"))
    modelfn <- switch(mt, 
                      consistency = consistency,
                      inconsistency = inconsistency)
    this.ss <- aggregate_performance(ps, reference = ps$scores[1])
    x <- modelfn(this.ss, mods = mods, ...)
    scorepairs <- expand.grid(x1 = scores, x2 = scores)
    scorepairs$x1 <- factor(scorepairs$x1, scores)
    scorepairs$x2 <- factor(scorepairs$x2, scores)
    if(!is.null(this.ref)){
     to.include <- with(scorepairs, x1 == this.ref & x1 != x2) 
    } else {
      to.include <- with(scorepairs, x1 != x2 & as.numeric(x2) > as.numeric(x1))
    }
    scorepairs <- scorepairs[to.include, ]
    pairmat <- (model.matrix(~ scorepairs$x2) - model.matrix( ~ scorepairs$x1))[, -1]
    colnames(pairmat) <- scores[-1]
    rownames(pairmat) <- with(scorepairs, paste(x2, x1, sep = "-"))
    
    pick.betas <- 1:(length(scores) - 1)
    est <- as.vector(pairmat %*% x$beta[pick.betas])
    var.est <- pairmat %*% x$vb[pick.betas, pick.betas] %*% t(pairmat)
    ci.lb <- as.vector(est - qnorm(1 - 0.05 / 2) * sqrt(diag(var.est)))
    ci.ub <- as.vector(est + qnorm(1 - 0.05 / 2) * sqrt(diag(var.est)))
    pval <- as.vector(pnorm(-abs(est / sqrt(diag(var.est)))) * 2)
    #list("estimate" = est, "ci.lb" = ci.lb, "ci.ub" = ci.ub, "var.estimate" = var.est)
    out <- data.frame(s1 = scorepairs$x1,
                  s2 = scorepairs$x2,
                  model = x$model,
                  type = "network",
                  estimate = est,
                  ci.lb = ci.lb,
                  ci.ub = ci.ub,
                  pval = pval, 
                  measure = ps$lbl,
                  mods = paste(mods, collapse = ", "))
    
    out$s1 = factor(out$s1, scores)
    out$s2 = factor(out$s2, scores)
    ord <- order(out$s1, out$s2)
    out <- out[ord, ]
    out <- list("table" = out,
                "models" = list("network" = x),
                "type" = "network", 
                "performance" = ps$lbl,
                "mods" = mods)
    class(out) <- "msc"
    out
}

#' @describeIn msc Compute all pairwise indirect comparisons
#' @export
msc_direct <- function(ps, mods = NULL, mtype = c("consistency", "inconsistency")[1], ref = NULL, verbose = FALSE, ...){
    if(class(ps) != "mscraw") warning("A set of *raw* performance estimates is needed (class(ps) == 'mscraw'), from compute_performance().")
    # keep only if (pair %in% design)
    scores <- ps$scores
    we <- ps$working.estimates
    
    mt <- match.arg(mtype, c("consistency", "inconsistency"))
    modelfn <- switch(mt,
                      consistency = consistency,
                      inconsistency = inconsistency)
    
    # make list of pairwise comparisons
    this.ref <- ref
    if(!is.null(this.ref) && !(this.ref %in% scores)){
      warning("ref not in list of scores, using ", scores[1], " instead")
      this.ref <- scores[1]
    }
    if(is.null(ref)){
      listpairs <- combn(scores, 2)
    } else {
      listpairs <- rbind(this.ref, scores[scores != this.ref])
    }
    if(verbose) print(listpairs)
    datout <- data.frame(s1 = listpairs[1, ],
                     s2 = listpairs[2, ],
                     model = mt,
                     type = "direct",
                     estimate = NA,
                     ci.lb = NA,
                     ci.ub = NA,
                     pval = NA, 
                     measure = ps$lbl,
                     mods = paste(mods, collapse = ", "))
    modelresults <- vector("list", ncol(listpairs))
    names(modelresults) <- paste(apply(listpairs, 2, paste, collapse = "-"), "direct")
    
    if(verbose) cat("Number of pairs to compare: ", ncol(listpairs))
    for(i in 1:ncol(listpairs)){
        if(verbose) cat(".")
        
        s1 <- listpairs[1, i]
        s2 <- listpairs[2, i]
        if(verbose) cat(s1, s2, "\n")
        this.we <- lapply(we, get_direct, s1, s2)
        if(all(sapply(this.we, nrow) == 0)){
            datout[i, 5:8] <- NA 
        } else {
        this.ps <- ps
        this.ps$working.estimates <- this.we
        this.ps$scores <- c(s1, s2)
        
        this.agg <- aggregate_performance(this.ps, reference = s1)
        this.model <- try(modelfn(this.agg, mods = mods, ...), silent = TRUE)
        modelresults[[i]] <- this.model
        if(verbose) print(this.model)
        if(any(class(this.model) == "try-error")){
                datout[i, 5:8] <- NA 
        } else {
                this.out <- with(this.model, cbind(beta, ci.lb, ci.ub, pval))
                datout[i, 5:8] <- this.out[1, ]
        }
        if(verbose) print(datout[i, ])
        
        }
        
    }
    
    datout$s1 <- factor(datout$s1, scores)
    datout$s2 <- factor(datout$s2, scores)
    ord <- order(datout$s1, datout$s2)
    datout <- datout[ord, ]
    out <- list("table" = datout,
                "models" = modelresults, 
                "type" = "direct", 
                "performance" = ps$lbl,
                "modeltype" = mt)
    class(out) <- "msc"
    out
}

#' @describeIn msc Compute all pairwise indirect comparisons
#' @export
msc_indirect <- function(ps, mods = NULL, 
                         mtype = c("consistency", "inconsistency")[1],
                         ref = NULL, 
                         verbose = FALSE, ...){
    if(class(ps) != "mscraw") warning("A set of *raw* performance estimates is needed (class(ps) == 'mscraw'), from compute_performance().")
    scores <- ps$scores
    we <- ps$working.estimates
    
    mt <- match.arg(mtype, c("consistency", "inconsistency"))
    modelfn <- switch(mt,
                      consistency = consistency,
                      inconsistency = inconsistency)
    
    # make list of pairwise comparisons
    this.ref <- ref
    if(!is.null(this.ref) && !(this.ref %in% scores)){
      warning("ref not in list of scores, using ", scores[1], " instead")
      this.ref <- scores[1]
    }
    if(is.null(ref)){
      listpairs <- combn(scores, 2)
    } else {
      listpairs <- rbind(this.ref, scores[scores != this.ref])
    }
    if(verbose) print(listpairs)
    datout <- data.frame(s1 = listpairs[1, ],
                     s2 = listpairs[2, ],
                     model = mt,
                     type = "indirect",
                     estimate = NA,
                     ci.lb = NA,
                     ci.ub = NA,
                     pval = NA, 
                     measure = ps$lbl,
                     mods = paste(mods, collapse = ", "))
    modelresults <- vector("list", ncol(listpairs))
    names(modelresults) <- paste(apply(listpairs, 2, paste, collapse = "-"), "indirect")
    
    if(verbose) cat("Number of pairs to compare: ", ncol(listpairs))
    for(i in 1:ncol(listpairs)){
        if(verbose) cat(".")
        
        s1 <- listpairs[1, i]
        s2 <- listpairs[2, i]
        if(verbose) cat(s1, s2, "\n")
        this.we <- lapply(we, get_indirect, s1, s2, sc = scores)
        
        c1 <- any(sapply(this.we, check_combn, to.check = s1)) 
        c2 <- any(sapply(this.we, check_combn, to.check = s2)) 
        
        if(!(c1 & c2)){
            datout[i, 5:8] <- NA 
        } else {
            this.ps <- ps
            this.ps$working.estimates <- this.we
            this.agg <- aggregate_performance(this.ps, reference = s1)
            this.model <- try(modelfn(this.agg, mods = mods, ...), silent = TRUE)
            modelresults[[i]] <- this.model
            if(verbose) print(this.model)
            if(any(class(this.model) == "try-error")){
                datout[i, 5:8] <- NA 
            } else {
                this.out <- with(this.model, cbind(beta, ci.lb, ci.ub, pval))
                if(verbose) print(coef(this.model))
                this.pick <- ifelse(length(coef(this.model)) == 1, 1, s2)
                if(verbose) print(this.pick)
                datout[i, 5:8] <- this.out[this.pick, ]
            }
            if(verbose) print(datout[i, ])
            
        }
        
    }
    
    datout$s1 <- factor(datout$s1, scores)
    datout$s2 <- factor(datout$s2, scores)
    ord <- order(datout$s1, datout$s2)
    datout <- datout[ord, ]
    out <- list("table" = datout,
                "models" = modelresults, 
                "type" = "indirect", 
                "performance" = ps$lbl)
    class(out) <- "msc"
    out
}

#' @rdname msc
#' @title Print MSC results
#' @param x An object of class \code{msc}, from any of \code{msc_{full, direct, indirect, network}}.
#' @export
print.msc <- function(x,  ...){
    print(x$table, ...)
}

#' @rdname msc
#' @title Simple plots for MSC network comparisons
#' @param compare_to Deprecated. Use the \code{ref} option in the \code{msc} function instead. (If specified, only comparisons to \code{compare_to} are plotted.)
#' @param newlabels A new vector of labels (character) for the scores can be used instead of the current vector of score names.
#' @method plot msc
#' @export
plot.msc <- function(x, compare_to = NULL, newlabels = NULL, ...){
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Package \"ggplot2\" needed for this function to work. Please install it.",
             call. = FALSE)
    }
    #require(ggplot2)
    x <- x$table
    
    if(!is.null(compare_to)){
      warning("Use the `ref` option in the msc_full, etc, function.")
        check1 <- length(compare_to) == 1
        if(!check1) stop("compare_to should be a vector of length 1, for example: ", x$s1[1])
        check2 <- all((compare_to %in% levels(x$s1) | compare_to %in% levels(x$s2)))
        if(!check2) stop("compare_to should be one of the values found in s1 or s2, for example: ", x$s2[1])
        x1 <- x[x$s1 == compare_to, ]
        x2 <- x[x$s2 == compare_to, ]
        if(nrow(x2) > 0){
        x2fix <- data.frame(s1 = x2$s2,
                        s2 = x2$s1,
                        model = x2$model,
                        type = x2$type,
                        estimate = -x2$estimate,
                        ci.lb = -x2$ci.ub,
                        ci.ub = -x2$ci.lb,
                        pval = x2$pval,
                        measure = x2$measure, 
                        mods = x1$mods[1])
        x <- rbind(x1, x2fix)
        } else {
          x <- x1
        }
    } 
    if(!is.null(newlabels)){
        oldlabels <- levels(x$s1)
        k <- length(oldlabels)
        if(length(newlabels) != length(oldlabels)){
            warning("newlabels must have length ", k, 
                    ". We will keep the old levels of s1 and s2,")
        } else {
            message("Replacing old s1 and s2 levels (", 
                    paste(oldlabels, collapse = "; "), 
                    ") with newlabels (", 
                    paste(newlabels, collapse = "; "), ")")
            levels(x$s1) <- levels(x$s2) <- newlabels
        }
        
    }
    ggplot(aes(.data$s2, .data$estimate, 
              ymin = .data$ci.lb, ymax = .data$ci.ub,
              color = .data$type), data = x) + 
        geom_linerange(position = position_dodge(width = 0.2)) + 
        geom_point(position = position_dodge(width = 0.2)) + 
        facet_wrap(vars(.data$s1)) +
        guides(color = guide_legend("")) + 
        labs(title = x$measure[1], 
             subtitle = paste(x$model[1], "model")) + 
        xlab("") + ylab(paste("difference in", x$measure[1]))
}
