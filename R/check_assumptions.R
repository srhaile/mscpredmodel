#' Check assumptions of transitivity, consistency and homogeneity
#' 
#' @examples
#' dat <- msc_sample_data()
#' bssamp <- get_bs_samples(dat, id, study, outcome, n.samples = 8, 
#'                   scores = c("a", "b", "c", "d", "e", "f"), 
#'                   moderators = c("age", "female", "x1"))
#' perf <- compute_performance(bssamp, fn = calibration_slope, lbl = "CS")
#' agg <- aggregate_performance(perf)
#' check_transitivity(perf, graph = FALSE)
#' check_transitivity(agg, graph = FALSE)
#' check_homogeneity(consistency(agg))
#' full <- msc_full(perf)
#' check_homogeneity(full)
#' \dontrun{check_consistency(perf)}

#' @describeIn check_assumptions Check assumption of transitivity
#' @param x A set of computed (mscraw) or aggregated performance scores (mscagg)
#' @param graph Should a graph of outcomes versus moderators be printed?
#' @return A tibble containing results of linear regression models of the effect size (outcome) against the cohort-specific average moderator value.
#' 
#' @importFrom stats model.matrix lm
#' @importFrom broom tidy
#' 
#' @export
check_transitivity <- function(x, graph = FALSE){
    if(identical(class(x), "mscraw")){
        x <- aggregate_performance(x)
    }
    #if(!identical(class(x), "mscagg")) warning("x should be the results of aggregate_performance()")
    if(is.null(x$mods)) warning("x should contain some moderators. Add these when you run get_bs_samples().")
    x$wt <- solve(x$vi)
    
    subset_agg <- function(x, ctr){
        picks <- x$contr == ctr
        x2 <- x
        x2$cohort <- as.character(x$cohort[picks])
        x2$yi <- x2$yi[picks]
        x2$vi <- x2$vi[picks, picks]
        x2$wt <- as.matrix(x2$wt[picks, picks])
        x2$contr <- x$contr[picks]
        x2$design <- x2$design[picks]
        x2$design.matrix <- x2$design.matrix[picks, ]
        x2$moderators <- x2$moderators[picks, ]
        x2
    }
    
    transitivity_model <- function(contr, moderator){
        this.x <- subset_agg(x, contr)
        d1 <- data.frame("cohort" = this.x$cohort,
                         "yi" = this.x$yi, 
                         "contr" = this.x$contr, 
                         "design" = this.x$design)
        d1$wt <- diag(this.x$wt)
        d2 <- this.x$moderators
        dat.x <- merge(d1, d2, by = "cohort", all = TRUE)
        this.fm <- paste("yi ~", moderator)
        this.lm <- lm(as.formula(this.fm), weights = dat.x$wt, data = dat.x)
        out <- tidy(this.lm, conf.int = TRUE)
        out$contr <- contr
        out$moderator <- moderator
        out <- out[, c(8, 9, 1:7)]
        out
    }  
    parms <- expand.grid(moderator = x$mods, 
                contr = unique(x$contr)) 
    res <- do.call(rbind, mapply(transitivity_model, parms$contr, parms$moderator, SIMPLIFY = FALSE))
    res <- res[res$term != "(Intercept)", ]
    
    if(graph){
        if (!requireNamespace("ggplot2", quietly = TRUE)) {
            stop("Package \"ggplot2\" needed for this function to work. Please install it.",
                 call. = FALSE)
        }
        #require(ggplot2)
        
        
        
        dat1 <- data.frame(cohort = as.character(x$cohort), 
                                yi = x$yi, 
                                contr = x$contr, wt = diag(x$wt))
        dat2 <- x$moderators
        dat.ag <- merge(dat1, dat2, by = "cohort", all = TRUE) 
        dat.ag$id <- 1:nrow(dat.ag)
        
        agw <- reshape(dat.ag, varying = x$mods, v.names = "value",
                timevar = "moderator", times = x$mods, 
                idvar = "id", direction = "long")
        p <- ggplot(aes(.data$value, .data$yi, size = .data$wt, 
                        color = .data$contr, shape = .data$contr), 
               data = agw) + 
            geom_point() + 
            geom_smooth(method = "lm") + 
            xlab("Value of Moderator") + ylab("Difference in Performance") + 
            guides(size = FALSE, 
                   color = guide_legend("Contrast"), 
                   shape = guide_legend("Contrast")) + 
            facet_wrap( ~ .data$moderator, scales = "free_x")
        print(p)
    }
    
    ord <- with(res, order(moderator, contr))
    res[ord, ]
}

#' @describeIn check_assumptions Check assumption of homogeneity
#' @param object A \code{\link{consistency}} or \code{inconsistency} model, or the results of \code{msc_network}, \code{msc_full}, \code{msc_direct}, or \code{msc_indirect}.
#' @param dig Number of digits to be printed, see \code{\link{format.pval}}.
#' @return A vector containing tau-square, the Q likelihood ratio statistic, its degrees of freedom and its p-value.
#' 
#' @export
check_homogeneity <- function(object, dig = 3){
    if(!any(class(object) %in% c("rma.mv", "msc"))){
        stop("Input should be from `[in]consistency()` or any `msc` command!")
    }
    
    parms <- c("tau2", "QE", "k", "p", "QEp")
    newparms <- c("tau2", "QE", "df", "QEp")
    if(any(class(object) == "msc")){
        object <- object$models
        out <- lapply(lapply(object, `[`, parms), unlist)
        out <- as.data.frame(do.call(rbind, out))
    } else {
        out <- data.frame(object[parms])
    }
    out$df <- out$k - out$p
    out <- out[, newparms]
    out
    
}


#' @describeIn check_assumptions Check assumption of consistency
#' @param ps A set of raw performance estimates, from \code{\link{compute_performance}}
#' @param mtype Type of model (default "consistency", else "inconsistency"). It is sufficient to write \code{"c"} or \code{"i"}.
#' @return A graph of the direct and indirect estimates for the various differences in score performance.
#' @export
#'
check_consistency <- function(ps, mtype = c("consistency", "inconsistency")[1]){
    if(class(ps) != "mscraw") stop("ps should be the results of `compute_performance`!")
    fullres <- msc_full(ps, mtype = mtype)
    return(plot.msc(fullres))
}

