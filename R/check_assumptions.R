#' Check assumptions of transitivity, consistency and homogeneity
#' 
#' @examples
#' dat <- msc_sample_data()
#' mod <- msc(scores = c("a", "b", "c", "d",),
#'             cohort = "cohort", outcome = "mort3", subjid = "id",
#'             fn = list(`logit AUC` = c_statistic, 
#'             `calib slope` = calibration_slope), 
#'             model = "consistency", 
#'             direct = FALSE, indirect = FALSE,
#'             ref = "first", data = dat)
#' \dontrun{check_consistency(mod)
#' modm <- msc(scores = c("a", "b", "c", "d",),
#'             cohort = "cohort", outcome = "mort3", subjid = "id",
#'             fn = list(`logit AUC` = c_statistic, 
#'             `calib slope` = calibration_slope), 
#'             model = "consistency", 
#'             mods = c("age", "female", "x1"),
#'             direct = FALSE, indirect = FALSE,
#'             ref = "first", data = dat
#'             
#' check_transitivity(modm)
#' 
#' modnet <- msc(scores = c("a", "b", "c", "d",),
#'             cohort = "cohort", outcome = "mort3", subjid = "id",
#'             fn = list(`logit AUC` = c_statistic, 
#'             `calib slope` = calibration_slope), 
#'             model = "consistency", 
#'             direct = TRUE, indirect = TRUE,
#'             ref = "first", data = dat)
#'             
#' check_consistency(modnet)}

#' @describeIn check_assumptions Check assumption of transitivity
#' @param object An object from \code{msc}. For \code{check_transitivity}, it must contain moderators.
#' @param graph Should a graph of outcomes versus moderators be printed?
#' @return A nested list (performance measure, then moderator, than score contrast) containing linear regression model output of the effect size (outcome) against the cohort-specific average moderator value.
#' 
#' @importFrom stats model.matrix lm
#' @importFrom broom tidy
#' 
#' @export
check_transitivity <- function(object, graph = TRUE){
    
    if(is.null(attr(object, "mods"))){
        stop("No moderators included. Please add some with the `mods` argument.")
    }
    
    mods <- attr(object, "mods")
    
    this_dat <- lapply(object, function(x) x$data)
    this_dat <- lapply(this_dat, function(x){
        x$w <- 1 / x$vi
        x
    })
    
    measures <- names(this_dat)
    m <- length(mods)
    
    out <- vector("list", length(measures))
    names(out) <- measures
    
    for(meas in measures){
        contrlist <- unique(this_dat[[meas]]$contr)
        p <- length(contrlist)
        
        tmpm <- vector("list", m)
        names(tmpm) <- mods
        for(i in 1:m){
            tmpp <- vector("list", p)
            names(tmpp) <- contrlist
            for(j in 1:p){
                trfm <- paste("yi ~ ", mods[i])
                subdat <- subset(this_dat[[meas]], contr == contrlist[j])
                if(nrow(subdat) <= 1){
                    trmodel <- NULL   
                } else {
                    trmodel <- try(lm(as.formula(trfm),
                                      weights = w, 
                                      data = subdat), silent = TRUE)
                }
                tmpp[[j]] <- trmodel
            }
            tmpm[[i]] <- tmpp
        }
        
        out[[meas]] <- tmpm
    }
    
    if (graph){
        if (!requireNamespace("ggplot2", quietly = TRUE)) {
            stop("Package \"ggplot2\" needed for this function to work. Please install it.", 
                 call. = FALSE)
        }
        
        agw <- lapply(this_dat, function(x) reshape(x,
                                                    varying = mods, 
                                                    v.names = "value", 
                                                    timevar = "moderator", 
                                                    times = mods, 
                                                    idvar = "id", 
                                                    direction = "long"))
        for(i in 1:length(agw)){
            agw[[i]]$measure <- names(agw)[i]
        }
        agw <- do.call("rbind", agw)
        
        p <- ggplot(aes(value, yi), data = agw) + 
            geom_smooth(aes(linetype = contr, 
                            color = contr, weight = w),
                        method = "lm", formula = y ~ x,
                        alpha = 0.2) +
            geom_point(aes(shape = contr, color = contr, size = w)) + 
            xlab("Value of Moderator") + ylab("Difference in Performance") + 
            guides(weight = "none", size = "none",
                   color = guide_legend("Contrast"), 
                   linetype = "none", 
                   shape = guide_legend("Contrast")) +
            scale_linetype_discrete(drop = FALSE) +
            facet_grid(cols = vars(moderator), 
                       rows = vars(measure), scales = "free") +
            theme(legend.position = "bottom")
    }
    if(graph){
        return(p)
    } else {
        class(out) <- "msctr"
        return(out)
    }
}


#' @describeIn check_assumptions Check assumption of homogeneity
#' @param object An object from \code{msc}.
#' @param dig Number of digits to be printed, see \code{\link{format.pval}}.
#' @return A list containing datasets with tau-square, the Q likelihood ratio statistic, its degrees of freedom and its p-value.
#' 
#' @export
check_homogeneity <- function(object, dig = 3){
    if (!any(class(object) =="msc")) {
        stop("Input should be from `msc()` command!")
    }
    
    parms <- c("tau2", "QE", "k", "p", "QEp")
    newparms <- c("tau2", "QE", "df", "QEp")
    
    models <- lapply(object, function(x){
        y <- x$rma.mv
        y <- y[parms]
        y <- as.data.frame(y)
        y$df <- with(y, k - p)
        y <- y[, newparms]
        y
    })
    
    for(i in 1:length(models)){
        models[[i]]$measure <- names(models)[i]
        models[[i]] <- models[[i]][, c(5, 1:4)]
    }
    out <- do.call("rbind", models)
    rownames(out) <- NULL
    return(out)
}



#' @describeIn check_assumptions Check assumption of consistency
#' @param object An object from \code{msc}. For \code{check_consistency}, it must have been run with options \code{direct = TRUE, indirect = TRUE}
#' @param graph Should the results be displayed graphically? (default TRUE) If not, the coefficient table is returned.
#' @return A graph of the direct and indirect estimates for the various differences in score performance.
#' @export
#'
check_consistency <- function(object, graph = TRUE){
    if (!all(c("network", "direct", "indirect") %in% object[[1]]$models$evidence)) {
        stop("Please rerun the model with `direct = TRUE, indirect = TRUE`.")
    }
    
    avail_scores <- attr(object, "scores")
    k <- length(avail_scores)
    main_contr <- NULL
    for(i in 1:(k - 1)){
        main_contr <- c(main_contr, 
                        paste(avail_scores[(i + 1):k], avail_scores[i], sep = "-"))
    }
    
    est <- lapply(object, function(x) x["models"])
    est <- lapply(est, function(x) do.call("rbind", x))
    est <- lapply(est, function(x){rownames(x) <- NULL; x})
    for(i in 1:length(est)){
        est[[i]]$perfmeasure <- names(est)[i]
        est[[i]] <- est[[i]][, c(11, 1:10)]
        est[[i]]$contr <- with(est[[i]], paste(term, ref, sep = "-"))
        est[[i]] <- subset(est[[i]], contr %in% main_contr)
    }
    out <- do.call("rbind", est)
    out$contr <- with(out,  paste(term, ref, sep = "-"))
    ord <- with(out, order(perfmeasure, contr, evidence))
    out <- out[ord, ]
    out$evidence <- factor(out$evidence, c("network", "direct", "indirect"))
    rownames(out) <- NULL
    
    if (graph){
        if (!requireNamespace("ggplot2", quietly = TRUE)) {
            stop("Package \"ggplot2\" needed for this function to work. Please install it.", 
                 call. = FALSE)
        }
        
        p <- ggplot(aes(contr, estimate,
                        ymin = conf.low, ymax = conf.high), data = out) +
            geom_point(aes(color = evidence, shape = evidence, 
                           size = 1 / (std.error ^ 2)),
                       position  = position_dodge(width = 0.3)) + 
            geom_linerange(aes(color = evidence),
                           position  = position_dodge(width = 0.3)) + 
            facet_wrap(vars(perfmeasure), ncol = 1,
                       scales = "free_y") + 
            guides(size = "none") +
            xlab(NULL) + ylab("estimated difference in performance") + 
            theme(legend.position = "bottom")
        return(p)
    } else {
        return(out)
    }
    
}

