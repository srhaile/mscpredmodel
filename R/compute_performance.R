#' Computes performance measures for prognostic scores on a set of bootstrap samples
#'
#' @param bs.sample A set of cohort-stratified bootstrap samples, as generated with \code{\link{get_bs_samples}}
#' @param fn The name of a function to compute performance measures. For example, \code{\link{calibration_slope}}. See \strong{details}.
#' @param lbl A label to describe the performance measure computed. For example \code{"calibration slope"}. This label is used in subsequent plot functions, and appears in tables of regression coefficients of. Useful if you want to compute several performance measures, or the same performance measure on different subsets of data.
#'
#' @return A list with 3 elements, of class \code{msc_raw}:
#' \describe{
#'   \item{working.estimates}{A tibble containing a set of "working estimates", the raw performance measures, before they have been aggregated or contrasts have been computed.}
#'   \item{scores}{Names of the scores as given in \code{\link{get_bs_samples}}}
#'   \item{formulas}{vector of formulas)}
#'   \item{fn}{The function definition used to compute performance}
#'   \item{lbl}{The label given in the arguments.}
#' }
#' The results of \code{\link{compute_performance}} have suitable \code{print} and \code{summary} methods.
#'
#' @details The function to compute performance measures, \code{fn} requires two arguments:
#' \describe{ 
#' \item{bss}{The name of the bootstrap sample. The full bootstrap data is called within the function as \code{analysis(bss)}. See  \code{\link[rsample]{bootstraps}} for more details.}
#'   \item{fn}{The formula that will be called by the model, of the form \code{outcome ~ score} (character).}
#' }
#' and outputs a single numeric value. Using \code{\link{possibly}}, \code{\link{compute_performance}} assigns a value of \code{NA} if there is an error.
#'
#' @export
#' 
#' @importFrom magrittr %>%
#' @importFrom stats model.matrix binomial median quantile sd
#' @import dplyr
#' @importFrom tidyr spread gather unite nest
#' @importFrom purrr map map2 map2_dbl map_dbl possibly partial
#' @importFrom utils head data
#' @importFrom rsample analysis
#'
#' @examples
#' dat <- msc_sample_data()
#' bssamp <- get_bs_samples(dat, id, study, outcome, n.samples = 5, 
#'                   scores = c("a", "b", "c", "d", "e", "f"), 
#'                   moderators = c("age", "female", "x1"))
#' perf <- compute_performance(bssamp, fn = calibration_slope, lbl = "CS")
#' print(perf)
#' summary(perf)
#' points(perf)
#' lines(perf)
compute_performance <- function(bs.sample,
                                fn = calibration_slope,
                                lbl = NULL){
    scores <- bs.sample$scores
    mods <- bs.sample$mods
    formulas <- bs.sample$formulas
    orig.sample <- bs.sample$orig.sample
    bs.sample <- bs.sample$bs.sample
    
    if (!requireNamespace("rsample", quietly = TRUE)) {
      stop("Package \"rsample\" needed for this function to work. Please install it.",
           call. = FALSE)
    }
    
    wrap_fn <- function(dd){
        out <- NULL
        bss <- lapply(dd$splits, analysis)
        try_fn <- function(...){
            this.out <- try(fn(...), silent = TRUE)
            if(any(class(this.out) == "try-error")) this.out <- NA
            this.out
            }
        for(i in formulas){
            out <- cbind(out, sapply(bss, try_fn, fm = i))
        }
        colnames(out) <- scores
        rownames(out) <- NULL
        data.frame("id" = dd$id, out)
        
    }

    ps <- lapply(bs.sample, wrap_fn)
    
    get_mods <- function(x){
        as.data.frame(x[, c("cohort", mods)])
    }
    
    dat.mods <- lapply(orig.sample, get_mods)
    
    out <- list("working.estimates" = ps,
                moderators = dat.mods,
                scores = scores, mods = mods, 
                formulas = formulas, fn = fn, lbl = lbl)
    class(out) <- "mscraw"
    out
}

#' @describeIn compute_performance Print raw performance estimates
#' @param x Set of performance estimates calculated with \code{\link{compute_performance}}
#' @param ... Other arguments to be passed to \code{\link{print}}. Ignored by summary, points and lines.
#' @export
print.mscraw <- function(x, ...){
    x.apparent <- lapply(x$working.estimates, function(x) x[x$id == "Apparent", ])
    cohorts <- names(x.apparent)
    x.apparent <- do.call(rbind, x.apparent)
    x.apparent <- data.frame(cohort = cohorts, x.apparent[, -1])
  print(x.apparent, ...)
}

#' #' @describeIn compute_performance Print summary of raw performance estimates
#' #' @param object Set of performance estimates calculated with \code{\link{compute_performance}}
#' #' @param nonpar  Should nonparametric summary statistics (median [IQR]) be reported? (TRUE)
#' #' @param NArm Should NAs be removed before calculated summary statistics? (TRUE)
#' #' @export
#' summary.mscraw <- function(object, nonpar = TRUE, NArm = TRUE, ...){
#'   sc <- object$scores
#'   object.apparent <- object$working.estimates %>%
#'     filter(id == "Apparent") %>%
#'     select(.data$cohort, sc)
#'   q1 <- partial(quantile, probs = 0.25, na.rm = NArm)
#'   q3 <- partial(quantile, probs = 0.75, na.rm = NArm)
#'   nonmiss <- function(object) sum(!is.na(object))
#'   if(nonpar){
#'     fns <- list("nonmiss" = nonmiss, "median" = partial(median, na.rm = NArm), 
#'                 "q1" = q1, "q3" = q3)
#'   } else {
#'     fns <- list("nonmiss" = nonmiss, "mean" = partial(mean, na.rm = NArm), 
#'                 "sd" = partial(sd, na.rm = NArm))
#'   }
#'   object.apparent  %>%
#'     gather(sc, key = "score", value = "value") %>%
#'     group_by(.data$score) %>%
#'     summarize_at("value", fns) %>%
#'     mutate(performance = object$lbl) %>%
#'     select(.data$score, .data$performance, everything())
#' }

#' @describeIn compute_performance Plot variability of raw performance estimates across bootstrap samples using points
#' @inheritParams print.mscraw
#' @export
points.mscraw <- function(x, ...){
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  working.estimates <- x$working.estimates
  cohorts <- names(working.estimates)
  we <- mapply(function(x, coh) data.frame(cohort = coh, x), 
         working.estimates, cohorts,
         SIMPLIFY = FALSE)
  we <- do.call(rbind, we)
  scores <- x$scores
  lbl <- x$lbl
  
  wel <- reshape(we, varying = scores,
                 times = scores,
                 v.names = "performance",
                 idvar = c("cohort", "id"), 
                 timevar = "score",
                 direction = "long")
  
  bs <- subset(wel, id != "Apparent")
  ap <- subset(wel, id == "Apparent")
  
  
  ggplot(aes(.data$cohort, .data$performance), data = bs) +
    geom_jitter(color = "gray", alpha = 0.5) +
    geom_point(data = ap) +
    facet_wrap(vars(.data$score)) +
    coord_flip() +
    ylim(-2, 5) + xlab(lbl)
}

#' @describeIn compute_performance Plot variability of raw performance estimates across bootstrap samples using lines (density plots)
#' @inheritParams print.mscraw
#' @export
lines.mscraw <- function(x, ...) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop(
            "Package \"ggplot2\" needed for this function to work. Please install it.",
            call. = FALSE
        )
    }
    working.estimates <- x$working.estimates
    cohorts <- names(working.estimates)
    we <- mapply(function(x, coh) data.frame(cohort = coh, x), 
                 working.estimates, cohorts,
                 SIMPLIFY = FALSE)
    we <- do.call(rbind, we)
    scores <- x$scores
    lbl <- x$lbl
    
    wel <- reshape(we, varying = scores,
                   times = scores,
                   v.names = "performance",
                   idvar = c("cohort", "id"), 
                   timevar = "score",
                   direction = "long")
    bs <- subset(wel, id != "Apparent")
    
    ggplot(aes(.data$performance, group = .data$cohort), data = bs) +
        geom_density() +
        xlab(lbl) +
        facet_wrap(vars(.data$score))
}

