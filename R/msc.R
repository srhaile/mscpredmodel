#' @title Full, direct and indirect network results for MSC
#' 
#' @importFrom stats model.matrix pnorm qnorm
#' @importFrom utils combn
#' @importFrom ggplot2 ggplot
#' @importFrom future.apply future_lapply
#' 
#' @description Compute comparisons of performance metrics for scores within a network.
#'
#' @param scores A vector of scores (i.e. variables in the dataset corresponding to calculated scores). The first score in the vector will be used as a comparator for all other scores.
#' @param cohort The name of the variable corresponding to cohort or study in the dataset.
#' @param outcome The name of the variable corresponding to the outcome.
#' @param subjid The name of the variable corresponding to the subject id.
#' @param fn A named list of performance metrics.
#' @param data Name of dataset containing all other variables, with individual patient data, that is, 1 row per subject.
#' @param mods A vector list of potential moderators in the dataset.
#' @param model Type of model (default "consistency", else "inconsistency"). It is sufficient to write \code{"c"} or \code{"i"}.
#' @param direct If a pair of scores are given, e.g. \code{c("B", "C")}, a direct estimate comparing those two scores is returned. (Default: NULL meaning full network estimates are computed.) In \code{msc_pairwise}, this should be given as TRUE or FALSE.
#' @param indirect If a pair of scores are given, e.g. \code{c("B", "C")}, a indirect estimate comparing those two scores is returned. (Default: NULL meaning full network estimates are computed.) If both \code{direct} and \code{indirect} are specified, only direct estimates are calculated. In \code{msc_pairwise}, this should be given as TRUE or FALSE.
#' @param ref Should all scores be compared to the \code{"first"} score, or \code{"all"} comparisons be calculated? The option \code{"all"} should be used if league tables are to be calculated afterwards. If code{ref = "all"}, one network model is run for each potential reference score, and direct / indirect models are run for all potential reference scores, rather than just all compared to the first score.
#' @param append_aggregate Name of structured dataset of aggregate data for which no IPD is available. Requires the following variables: cohort, contr, design, score.1, score.2,   yi, vi. (Default: NULL) TO DO
#' @param n.boot Number of bootstrap samples to be drawn. (Default: 250)
#' @param seed A random seed to make results reproducible (Default NULL)
#' @param max_missing If the proportion of missingness for any score within a cohort exceeds \code{max_missing}, the score will be counted as missing for the entire cohort. (Default 0.8)
#' @param ... In the \code{msc} functions, any other arguments are passed to \code{\link[metafor]{rma.mv}}.
#'
#' @return A list of class \code{msc}, with the following components:
#' \describe{
#'   \item{}{For each performance measure using the names from \code{fn}, a list containing \code{network}, \code{direct} if \code{direct = TRUE}, and \code{indirect} if \code{indirect = TRUE}. The coefficient tables from each \code{\link[metafor]{rma.mv}} model appear here}
#'   \item{data}{The dataset used in the network model.}
#'   \item{V}{The variance matrix used in the network model.}
#'   \item{rma.mv}{The full \code{\link[metafor]{rma.mv}} object for the network model.}
#' } 
#' 
#' 
#' The consistency model has random effects for contrast within cohort, while the inconsistency model has random effects for both contrast within cohort, and for contrast within design.
#' 
#' The main model in an analysis should probably not include any moderators, but they may be interesting when examining transitivity.
#'
#' @examples
#' dat <- msc_sample_data()
#' out1 <- msc(scores = c("a", "b", "c", "d", "e"),
#'          cohort = "study", outcome = "outcome", subjid = "id", 
#'          data = dat, direct = FALSE, indirect = FALSE,
#'          model = "inconsistency")
#' print(out1)
#' 
#' \dontrun{
#'  library(ggplot2)
#'  plot(out1)
#' }
#' 
 
#' @describeIn msc Compute MSC models for various performance measures, returning only the model results
#' @export
msc <- function(scores = c("A", "B", "C", "D"), cohort = "cohort", 
                outcome = "mort3", subjid = "id", 
                fn = list(AUC = c_statistic, 
                          `O/E` = oe_ratio), 
                mods = NULL, data = copddata, 
                model = c("consistency", "inconsistency")[1], 
                direct = TRUE, indirect = TRUE, 
                ref = c("first", "all")[1], n.boot = 250, 
                seed = NULL, max_missing = 0.8, 
                ...){
    
    # run basic checks before starting with the models
    if(is.null(scores)){
        stop("No scores provided to compare.")
    } else if(!is.null(scores)){
        missing_scores <- scores[!scores %in% names(data)]
    }
    if(length(missing_scores) == length(scores)){
        stop("Scores provided are not in data.")
    } else if(length(missing_scores) < length(scores) & length(missing_scores) != 0){
        scores <- scores[!scores %in% missing_scores]
        message("Some scores are not in data. Removing: ", paste(missing_scores, collapse = ", "), 
                ". New scores are: ", paste(scores, collapse = ", "), ".")
    }
    
    if(is.null(cohort)){
        stop("A variable of study cohorts must be specified, under the option `cohort`.") 
    } else if(!cohort %in% names(data)){
        stop("Variable specifying study cohorts (", cohort, ") is not in dataset.")
    }
    
    if(is.null(outcome)){
        stop("An outcome variable must be specified, under the option `outcome`.") 
    } else if(!outcome %in% names(data)){
        stop("The specified outcome variable (", outcome, ") is not in dataset.")
    }
    
    
    if(is.null(subjid)){
        stop("A subject ID must be specified, under the option `subjid`.") 
    } else if(!subjid %in% names(data)){
        stop("The specified subject ID (", subjid, ") is not in dataset.")
    }
    
    if(requireNamespace("future.apply", quietly = TRUE)) {
        require(future.apply)
        #plan(multisession, workers = 2)
        plan(sequential)
    }
    
    fnv <- as.vector(fn)
    
    my_fit <- function(f_fn, f_lbl, s = scores, c = cohort,
                       o = outcome, id = subjid,
                       m = mods, newdata = data, modeltype = model,
                       run_direct = direct, run_indirect = indirect,
                       which_ref = ref,
                       nb = n.boot, newseed = seed, 
                       pct = max_missing, ...){
        res <- NULL
        nw <- fit_msc(scores = s, cohort = c,
                      outcome = o, subjid = id,
                      perf_fn = f_fn, perf_lbl = f_lbl,
                      mods = m, data = newdata, model = modeltype,
                      direct = NULL, indirect = NULL,
                      n.boot = nb, seed = newseed, max_missing = pct, 
                      run_checks = FALSE, ...)
        nw_dat <- nw$aggrdat
        nw_model <- nw$rma.mv
        V <- nw$V
        this_res <- coeftab(nw_model)
        this_res$ref <- scores[1]
        this_res$evidence <- "network"
        res$models <-  this_res
        
        if(which_ref == "all"){
            for(ref_score in scores[-1]){
                new_fm <- paste(scores[scores != ref_score], collapse = " + ")
                new_fm <- paste("~", new_fm, "- 1")
                if(!is.null(m)){
                    fm_mods <- paste(m, collapse = " + ")
                    new_fm <- paste(new_fm, "+", fm_mods)
                }
                if(model == "consistency"){
                    new_mod <- rma.mv(yi, V, data = nw_dat, 
                                      mods = eval(as.formula(new_fm)),
                                      slab = cohort,
                                      random = list(~contr | cohort), 
                                      rho = 0.5, 
                                      control = list(tau2.init = 0.5,
                                                     optimizer="bobyqa"),
                                      ...)
                } else if(model == "inconsistency"){
                    new_mod <- rma.mv(yi, V, data = nw_dat, 
                                      mods = eval(as.formula(new_fm)),
                                      slab = cohort,
                                      random = list(~contr | cohort, 
                                                    ~contr | design), 
                                      rho = 0.5, phi = 0.5, 
                                      control = list(tau2.init = 0.5,
                                                     optimizer="bobyqa"),
                                      ...)
                }
                this_res <- coeftab(new_mod)
                this_res$ref <- ref_score
                this_res$evidence <- "network"
                res$models <- rbind(res$models, this_res)
            }
        }
        
        if(direct | indirect){
            combos <- combn(scores, m = 2)
            if(ref != "all"){
                choose_combos <- which(combos[1, ] == scores[1])
                combos <- combos[, choose_combos]
            }
            mc <- ncol(combos)
        }
        
        if(run_direct){
            message("obtaining direct estimates")
            tmp <- vector("list", mc)
            for(j in 1:mc){ #perf fun i + combo j
                this_combo <- combos[, j]
                this_mod <- fit_msc(scores = s, cohort = c,
                        outcome = o, subjid = id,
                        perf_fn = f_fn, perf_lbl = f_lbl,
                        mods = m, data = newdata, model = modeltype,
                        direct = this_combo, indirect = NULL,
                        n.boot = nb, seed = newseed, max_missing = pct,
                        run_checks = FALSE, ...)$rma.mv
                tmp[[j]] <- coeftab(this_mod$rma.mv)
                tmp[[j]]$ref <- this_combo[1]
                tmp[[j]]$term <- this_combo[2]
                tmp[[j]]$evidence <- "direct"
            }
            tmp <- do.call("rbind", tmp)
            res$models <- rbind(res$models, tmp)
        }

        if(run_indirect){
            message("obtaining indirect estimates")
            tmp <- vector("list", mc)
            for(j in 1:mc){ #perf fun i + combo j
                this_combo <- combos[, j]
                this_mod <- fit_msc(scores = s, cohort = c,
                                    outcome = o, subjid = id,
                                    perf_fn = f_fn, perf_lbl = f_lbl,
                                    mods = m, data = newdata, model = modeltype,
                                    direct = NULL, indirect = this_combo,
                                    n.boot = nb, seed = newseed, max_missing = pct,
                                    run_checks = FALSE, ...)
                tmp[[j]] <- coeftab(this_mod$rma.mv)
                tmp[[j]]$ref <- this_combo[1]
                tmp[[j]]$term <- this_combo[2]
                tmp[[j]]$evidence <- "indirect"
            }
            tmp <- do.call("rbind", tmp)
            res$models <- rbind(res$models, tmp)
        }
        
        res$data <- nw$aggrdat
        res$V <- nw$V
        res$rma.mv <- nw$rma.mv
        res
    }
    
    if (!requireNamespace("future.apply", quietly = TRUE)) {
        out <- lapply(fn, function(x) my_fit(f_fn = x, f_lbl = names(x)))
    } else {
        out <- future_lapply(fn, function(x) my_fit(f_fn = x, f_lbl = names(x)),
                             future.seed = NULL)
    }
    
    class(out) <- "msc"
    attr(out, "scores") <- scores
    attr(out, "mods") <- mods
    attr(out, "model") <- "consistency"
    return(out)
}

#' @details The consistency and inconsistency models are those found in \href{https://doi.org/10.1186/s12874-016-0184-5}{Law et al 2016}: 
#' 
#' Specifically, we fit the models described in Law et al 2016, which differ only in their random effects:
#' \describe{
#'   \item{consistency}{First item} random contrast within study
#'   \item{inconsistency}{Second item} random contrast within study, and random contrast within design
#' }
#' @seealso Law, M.; Jackson, D.; Turner, R.; Rhodes, K. & Viechtbauer, W. Two new methods to fit models for network meta-analysis with random inconsistency effects BMC Medical Research Methodology, 2016, 16, 87.#' 
#' 
#' @rdname msc
#' @title Print MSC results for one model
#' @param object An object of class \code{msc}, from \code{msc}.
#' @export
print.msc <- function(object){
    est <- lapply(object, function(x) x$models)
    est <- lapply(est, function(x){rownames(x) <- NULL; x})
    for(i in 1:length(est)){
        est[[i]]$perfmeasure <- names(est)[i]
        est[[i]] <- est[[i]][, c(11, 1:10)]
    }
    out <- do.call("rbind", est)
    rownames(out) <- NULL
    print(out)
}

#' @rdname msc
#' @title Plot MSC results for one model
#' @param object An object of class \code{msc}, from \code{msc}.
#' @export
plot.msc <- function(object){
    avail_scores <- attr(object, "scores")
    
    est <- lapply(object, function(x) x$models)
    est <- lapply(est, function(x){rownames(x) <- NULL; x})
    for(i in 1:length(est)){
        est[[i]]$perfmeasure <- names(est)[i]
        est[[i]] <- est[[i]][, c(11, 1:10)]
    }
    out <- do.call("rbind", est)
    rownames(out) <- NULL
    incl <- out$term %in% avail_scores
    out <- out[incl, ]
    
    out$contr <- with(out, paste(term, ref, sep = "-"))
    
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Package \"ggplot2\" needed for this function to work. Please install it.", call. = FALSE)
    }
    
    p <- ggplot(aes(contr, estimate, 
                    ymin = conf.low, ymax = conf.high,
                    color = evidence, shape = evidence), 
                data = out) +
        geom_point(aes(size = 1 / (std.error ^ 2)), 
                   position = position_dodge(width = 0.3)) + 
        geom_linerange(position = position_dodge(width = 0.3)) +
        facet_wrap(vars(perfmeasure), scales = "free_y") + 
        xlab(NULL) + ylab("difference in performance") + 
        guides(color = guide_legend(NULL),
               shape = guide_legend(NULL),
               size = "none") +
        theme(legend.position = "bottom")
    p
}

#' @describeIn msc Compute MSC models for various performance measures, returning only the model results
#' @export
fit_msc <- function(scores = c("A", "B", "C", "D"),
                    cohort = "cohort",
                    outcome = "mort3",
                    subjid = "id",
                    perf_fn = c_statistic,
                    perf_lbl = "c-statistic",
                    direct = NULL, indirect = c("B", "C"),
                    model = c("consistency", "inconsistency")[1],
                    mods = NULL,
                    data = this_dat,
                    append_aggregate = NULL,
                    n.boot = 250,
                    seed = NULL,
                    max_missing = 0.8,
                    run_checks = TRUE, ...){
    
    require(metafor)
    
    if(run_checks){
        
        if(is.null(scores)){
            stop("No scores provided to compare.")
        } else if(!is.null(scores)){
            missing_scores <- scores[!scores %in% names(data)]
        }
        if(length(missing_scores) == length(scores)){
            stop("Scores provided are not in data.")
        } else if(length(missing_scores) < length(scores) & length(missing_scores) != 0){
            scores <- scores[!scores %in% missing_scores]
            message("Some scores are not in data. Removing: ", paste(missing_scores, collapse = ", "), 
                    ". New scores are: ", paste(scores, collapse = ", "), ".")
        }
        
        if(is.null(cohort)){
            stop("A variable of study cohorts must be specified, under the option `cohort`.") 
        } else if(!cohort %in% names(data)){
            stop("Variable specifying study cohorts (", cohort, ") is not in dataset.")
        }
        
        if(is.null(outcome)){
            stop("An outcome variable must be specified, under the option `outcome`.") 
        } else if(!outcome %in% names(data)){
            stop("The specified outcome variable (", outcome, ") is not in dataset.")
        }
        
        
        if(is.null(subjid)){
            stop("A subject ID must be specified, under the option `subjid`.") 
        } else if(!subjid %in% names(data)){
            stop("The specified subject ID (", subjid, ") is not in dataset.")
        }
    }
    
    if(!is.null(direct)){
        check_length <- length(direct)
        if(check_length < 2){
            stop("Give 2 scores to compare directly.")
        } else if(check_length > 2){
            direct <- direct[1:2]
            message("More than 2 scores provided to compare directly, keeping only the first two: ", 
                    paste(direct, collapse = " and "), ".")
        } 
        present_direct <- direct[which(direct %in% scores)]
        absent_direct <- direct[which(!direct %in% scores)]
        dat_direct <- absent_direct[which(absent_direct %in% names(data) & !absent_direct %in% scores)]
        missing_direct <- absent_direct[which(!absent_direct %in% names(data))]
        
        if(length(missing_direct) > 0){
            message("Scores for direct comparison are not in the list of scores or in dataset.")
        } 
        if(length(dat_direct) > 0){
            message("Score for direct comparison ", paste(absent_direct, collapse = ", "), 
                    " added to list of scores.")
            scores <- c(dat_direct, scores)
        }
    }
    
    if(!is.null(indirect)){
        check_length <- length(indirect)
        if(check_length < 2){
            stop("Give 2 scores to compare indirectly.")
        } else if(check_length > 2){
            indirect <- indirect[1:2]
            message("More than 2 scores provided to compare indirectly, keeping only the first two: ", 
                    paste(indirect, collapse = " and "), ".")
        } 
        present_indirect <- indirect[which(indirect %in% scores)]
        absent_indirect <- indirect[which(!indirect %in% scores)]
        dat_indirect <- absent_indirect[which(absent_indirect %in% names(data) & !absent_indirect %in% scores)]
        missing_indirect <- absent_indirect[which(!absent_indirect %in% names(data))]
        
        if(length(missing_indirect) > 0){
            message("Scores for indirect comparison are not in the list of scores or in dataset.")
        } 
        if(length(dat_indirect) > 0){
            message("Score for indirect comparison ", paste(absent_indirect, collapse = ", "), 
                    " added to list of scores.")
            scores <- c(dat_indirect, scores)
        }
    }
    
    if(!is.null(mods)){
        mods <- mods[mods %in% names(data)]
    }
    
    this_dat <- data
    
    if(is.null(direct) & is.null(indirect)){
        
    } else if(!is.null(direct) & is.null(indirect)){
        
        has_both <- !is.na(this_dat[, direct[1]]) & !is.na(this_dat[, direct[2]])
        has_either <- xor(!is.na(this_dat[, direct[1]]), !is.na(this_dat[, direct[2]]))
        has_none <- is.na(this_dat[, direct[1]]) & is.na(this_dat[, direct[2]])
        scores <- direct
        this_dat <- this_dat[has_both, c(cohort, subjid, outcome, direct, mods)]
    } else if(is.null(direct) & !is.null(indirect)){
        has_both <- !is.na(this_dat[, indirect[1]]) & !is.na(this_dat[, indirect[2]])
        has_either <- xor(!is.na(this_dat[, indirect[1]]), !is.na(this_dat[, indirect[2]]))
        has_none <- is.na(this_dat[, indirect[1]]) & is.na(this_dat[, indirect[2]])
        
        score_other <- scores[!scores %in% indirect]
        scores <- c(indirect, score_other)
        
        this_dat <- this_dat[, c(cohort, subjid, outcome, scores, mods)]
        this_dat[has_both, indirect[1]] <- NA
        this_dat[has_both, indirect[2]] <- NA
        this_dat <- this_dat[!has_none, ]
        
    } else if(!is.null(direct) & !is.null(indirect)){
        message("Both direct and indirect comparisons requested. Only direct comparisons will be computed.")
        has_both <- !is.na(this_dat[, direct[1]]) & !is.na(this_dat[, direct[2]])
        has_either <- xor(!is.na(this_dat[, direct[1]]), !is.na(this_dat[, direct[2]]))
        has_none <- is.na(this_dat[, direct[1]]) & is.na(this_dat[, direct[2]])
        this_dat <- this_dat[has_both, c(cohort, subjid, outcome, direct, mods)]
    }
    
    print(nrow(this_dat))
    print(this_dat)
    
    if(nrow(this_dat) >= 1){
        
        g <- this_dat[, cohort]
        spl <- split(as.data.frame(this_dat), g)
        
        drop_missing <- function(x, maxmiss = max_missing){
            check_missing <- apply(x, 2, function(y) mean(is.na(y)))
            x[, check_missing > maxmiss] <- NA
            x
        }
        
        spl <- lapply(spl, drop_missing)
        
        k <- length(scores)
        
        out1 <- lapply(spl, compute_performance, 
                       k = length(scores), 
                       sc = scores,
                       fn = perf_fn,
                       observed = outcome, 
                       missing_threshold = max_missing)
        
        n.scores <- sapply(out1, length)
        n.contrasts <- ifelse(n.scores %in% 0:1, 0, n.scores - 1)
        cohorts <- rep(names(out1), n.contrasts)
        
        var_est <- lapply(spl, bootfn, R = n.boot, newseed = seed, k = k, 
                          sc = scores, pfn = perf_fn, 
                          missing_threshold = max_missing,
                          observed = outcome)
        
        V <- metafor::bldiag(var_est[n.contrasts > 0])
        
        cntr <- lapply(out1, get_contrasts)
        
        mm <- lapply(out1, get_mm)
        mm <- do.call("rbind", mm)
        
        designs <- lapply(out1, get_design)
        designs <- rep(unlist(designs), n.contrasts)
        
        perfdiff <- lapply(out1, get_perf_diff)
        perfdiff <- perfdiff[n.contrasts > 0]
        
        aggr_ipd <- data.frame(cohort = cohorts,
                               contr = unlist(cntr),
                               design = designs, 
                               score.1 = mm[, 1],
                               score.2 = mm[, 2],
                               yi = unlist(perfdiff),
                               vi = diag(V))
        
        if(!is.null(append_aggregate)){
            naa <- names(append_aggregate)
            nai <- names(aggr_ipd)
            if(length(naa) > length(nai)){
                message("append_aggregate does not have the right variables, it should contain:", paste(nai, collapse = ", "))
                append_aggregate <- NULL
            } else if(length(naa) > length(nai)){
                if(all(nai %in% naa)){
                    to_drop <- naa[!naa %in% nai]
                    append_aggregate <- subset(append_aggrege, drop = to_drop)
                } else {
                    to_add <- nai[!nai %in% nai]
                    message("append_aggregate is missing some variables, it should contain:", paste(to_add, collapse = ", "))
                    append_aggregate <- NULL
                }
            } else if(length(naa) == length(nai)){
                if(!all(naa == nai) & all(tolower(naa) == nai)){
                    names(append_aggregate) <- tolower(names(append_aggregate))
                } else if(!all(naa == nai) & all(naa %in% nai)){
                    append_aggregate <- append_aggregate[, nai]
                    aggr_ipd <- rbind(aggr_ipd, append_aggregate)
                    
                    aaV <- diag(append_aggregate$vi)
                    V <- metafor::bldiag(V, aaV)
                    
                } else if(all(naa == nai)){
                    aggr_ipd <- rbind(aggr_ipd, append_aggregate)
                    
                    aaV <- diag(append_aggregate$vi)
                    V <- metafor::bldiag(V, aaV)
                    
                } else {
                    message("append_aggregate seems to have mislabeled variables, it should contain:", paste(nai, collapse = ", "))
                    append_aggregate <- NULL
                }
            }
            
        }
        
        
        aggr_ipd <- metafor::contrmat(aggr_ipd, grp1 = "score.1", grp2 = "score.2")
        
        
        if(!is.null(mods)){
            summ_mods <- lapply(spl, summarize_moderators, m = mods)
            summ_mods <- do.call("rbind", summ_mods)
            summ_mods <- as.data.frame(summ_mods)
            summ_mods$cohort <- names(spl)
            aggr_ipd <- merge(aggr_ipd, summ_mods, by = "cohort", all.x = TRUE)
        }
        
        if(is.null(direct) & is.null(indirect)){
            this_fm <- paste(scores[-1], collapse = " + ")
            this_fm <- paste("~", this_fm, "- 1")
        } else if(!is.null(direct) & is.null(indirect)){
            this_fm <- paste(direct[-1], collapse = " + ")
            this_fm <- paste("~", this_fm, "- 1")
        } else if(is.null(direct) & !is.null(indirect)){
            this_fm <- paste(indirect[-1], collapse = " + ")
            this_fm <- paste("~", this_fm, "- 1")
        }
        
        if(!is.null(mods)){
            modsfm <- paste(mods, collapse = " + ")
            this_fm <- paste(this_fm, "+", modsfm)
        }
        
        if(!all(indirect %in% c(mm[, 1], mm[, 2]))){
            mod <- NULL
        } else
            if(model == "consistency"){
                mod <- try(rma.mv(yi, V, data = aggr_ipd, 
                              mods = as.formula(this_fm),
                              slab = cohort,
                              random = list(~contr | cohort), 
                              rho = 0.5, 
                              control = list(tau2.init = 0.5,
                                             optimizer="BFGS"),
                              ...),
                           silent = TRUE)
            } else if(model == "inconsistency"){
                mod <- try(rma.mv(yi, V, data = aggr_ipd, 
                              mods = as.formula(this_fm),
                              slab = cohort,
                              random = list(~contr | cohort, 
                                            ~contr | design), 
                              rho = 0.5, phi = 0.5, 
                              control = list(tau2.init = 0.5,
                                             optimizer="BFGS"),
                              ...),
                           silent = TRUE)
            }
        out <-  list(aggr_ipd, V, mod)
    }  else {
        out <- list(NULL, NULL, NULL)
    }
    names(out) <- c("aggrdat","V" , "rma.mv")
    class(out) <- "mscfit"
    out
    
}


