#' Check assumptions of transitivity, consistency and homogeneity
#' 
#' @examples
#' dat <- msc_sample_data()
#' bssamp <- get_bs_samples(dat, id, study, outcome, n.samples = 8, 
#'                   scores = c("a", "b", "c", "d", "e", "f"), 
#'                   moderators = c("age", "female", "x1"))
#' perf <- compute_performance(bssamp, fn = calibration_slope, lbl = "CS")
#' agg <- aggregate_performance(perf)
#' check_transitivity(agg, graph = FALSE)
#' check_homogeneity(consistency(agg))
#' \dontrun{check_consistency(perf)}

#' @describeIn check_assumptions Check assumption of transitivity
#' @param ag A set of aggregated performance scores
#' @param graph Should a graph of outcomes versus moderators be printed?
#' @return A tibble containing results of linear regression models of the effect size (outcome) against the cohort-specific average moderator value.
#' 
#' @importFrom magrittr %>%
#' @importFrom stats model.matrix lm
#' @import tibble
#' @import dplyr
#' @importFrom tidyr spread gather unite nest
#' @importFrom purrr map map2 possibly
#' @import broom
#' 
#' @export
check_transitivity <- function(ag, graph = FALSE){
    # print(class(ag))
    if(!identical(class(ag), "mscagg")) warning("ag should be the results of aggregate_performance()")
    if(is.null(ag$mods)) warning("ag should contain some moderators. Add these when you run get_bs_samples().")
    ag$wt <- solve(ag$vi)
    
    subset_agg <- function(x, ctr){
        picks <- x$contr == ctr
        x2 <- x
        x2$cohort <- as.character(x$cohort[picks])
        x2$yi <- x2$yi[picks]
        x2$vi <- x2$vi[picks, picks]
        x2$wt <- x2$wt[picks, picks]
        x2$contr <- x$contr[picks]
        x2$design <- x2$design[picks]
        x2$design.matrix <- x2$design.matrix[picks, ]
        x2$moderators <- x2$moderators[picks, ]
        x2
    }
    
    transitivity_model <- function(contr, moderator){
        this.ag <- subset_agg(ag, contr)
        dat.ag <- merge(data.frame("cohort" = this.ag$cohort,
                                   "yi" = this.ag$yi, 
                                   "contr" = this.ag$contr, 
                                   "design" = this.ag$design, 
                                   "wt" = diag(this.ag$wt)), 
                                   this.ag$moderators, 
                                    "by" = "cohort", all = TRUE)
        this.fm <- paste("yi ~", moderator)
        this.lm <- lm(as.formula(this.fm), weights = dat.ag$wt, data = dat.ag)
        tidy(this.lm, conf.int = TRUE)
    }  
    NA_tbl <- data.frame(term = NA, estimate = NA, std.error = NA, 
                     statistic = NA, p.value = NA, 
                     conf.low = NA, conf.high = NA)
    possibly_transitivity <- purrr::possibly(transitivity_model, NA_tbl)
    res <- crossing(contr = unique(ag$contr),
                    moderator = ag$mods) %>%
        mutate(fit = map2(.data$contr, .data$moderator, possibly_transitivity)) %>%
        unnest %>%
        filter(.data$term != "(Intercept)") %>%
        select(-.data$term)
    
    if(graph){
        if (!requireNamespace("ggplot2", quietly = TRUE)) {
            stop("Package \"ggplot2\" needed for this function to work. Please install it.",
                 call. = FALSE)
        }
        #require(ggplot2)
        
        dat1 <- data.frame(cohort = as.character(ag$cohort), 
                                yi = ag$yi, 
                                contr = ag$contr, wt = diag(ag$wt))
        dat2 <- ag$moderators
        dat.ag <- merge(dat1, dat2, by = "cohort", all = TRUE) %>%
        dat.ag$id = 1:nrow(dat.ag)
            tidyr::gather(ag$mods, key = "moderator", value = "value")
        p <- ggplot(aes(.data$value, .data$yi, size = .data$wt, 
                        color = .data$contr, shape = .data$contr), 
               data = dat.ag) + 
            geom_point() + 
            geom_smooth(method = "lm") + 
            xlab("Value of Moderator") + ylab("Difference in Performance") + 
            guides(size = FALSE, 
                   color = guide_legend("Contrast"), 
                   shape = guide_legend("Contrast")) + 
            facet_wrap( ~ .data$moderator, scales = "free_x")
        print(p)
    }
    
    res %>% 
        arrange(.data$moderator, .data$contr)
}

#' @describeIn check_assumptions Check assumption of homogeneity
#' @param mod A \code{\link{consistency}} or \code{inconsistency} model.
#' @param dig Number of digits to be printed, see \code{\link{format.pval}}.
#' @return A vector containing tau-square, the Q likelihood ratio statistic, its degrees of freedom and its p-value.
#' 
#' @export
check_homogeneity <- function(mod, dig = 3){
    if(!any(class(mod) == "rma.mv")){
        stop("Input should be from `[in]consistency()`!")
    }
    
    out <- c("tau2" = mod$tau2, "Q" = mod$QE, "df" = mod$k - mod$p, 
             "p-value" = mod$QEp)
    out.fmt <- rep(NA, 4)
    names(out.fmt) <- names(out)
    out.fmt[c(1, 2)] <- round(out[c(1, 2)], dig)
    out.fmt[3] <- out[3]
    out.fmt[4] <- format.pval(out[4], eps = 0.0001, digits = dig, scientific = 6)
    print(out.fmt)
    invisible(out)
}


#' @describeIn check_assumptions Check assumption of consistency
#' @param ps A set of raw performance estimates, from \code{\link{compute_performance}}
#' @param mtype Type of model (default "consistency", else "inconsistency"). It is sufficient to write \code{"c"} or \code{"i"}.
#' @return A graph of the direct and indirect estimates for the various differences in score performance.
#' @export
#'
check_consistency <- function(ps, mtype = c("consistency", "inconsistency")[1]){
    if(class(ps) != "mscraw") stop("ag should be the results of `compute_performance`!")
    fullres <- msc_full(ps, mtype = mtype)
    return(plot.msc(fullres))
}

