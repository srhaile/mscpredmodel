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
#' \item{bss}{The name of the bootstrap sample. The full bootstrap data is called within the function as \code{analysis(bss)}. See \code{\link{?rsample::bootstraps}} for more details.}
#'   \item{fn}{The formula that will be called by the model, of the form \code{outcome ~ score} (character).}
#' }
#' and outputs a single numeric value. Using \code{\link{possibly}}, \code{\link{compute_performance}} assigns a value of \code{NA} if there is an error.
#'
#' @export
#'
#' @examples
#' dat <- msc_sample_data()
#' bssamp <- get_bs_samples(dat, id, cohort, outcome, n.samples = 10, a, b, c, d, e)
#' perf <- compute_performance(bssamp, fn = calibration_slope, lbl = "CS")
#' print(perf)
#' summary(perf)
#' points(perf)
#' lines(perf)
compute_performance <- function(bs.sample,
                                fn = calibration_slope,
                                lbl = NULL){
    scores <- bs.sample$scores
    formulas <- bs.sample$formulas
    bs.sample <- bs.sample$bs.sample
    if (!requireNamespace("rsample", quietly = TRUE)) {
      stop("Package \"rsample\" needed for this function to work. Please install it.",
           call. = FALSE)
    }

    if(is.null(lbl)) lbl <- paste(head(fn), collapse = "")
    # data steps
    fm.dat <- expand.grid(cohort = unique(bs.sample$cohort),
                          id = unique(bs.sample$id),
                          fm = formulas) %>%
        mutate_all(as.character)
    bs.sample <- bs.sample %>%
      mutate(cohort = as.character(cohort))
    working.data <- full_join(bs.sample, fm.dat, by = c("cohort", "id")) %>%
        mutate(fm = as.character(fm))
    working.estimates <- working.data %>%
        mutate(type = ifelse(id == "Apparent", "apparent", "bootstrap"),
               est = map2_dbl(splits, fm, possibly(fn, otherwise = NA_real_)),
               scores = rep_len(scores, length.out = nrow(working.data)),
               measure = lbl) %>%
        select(-splits, -fm) %>%
        spread(scores, est)  %>%
        # next line seems redundant, but otherwise, the scores go in alphabetical order!
        select(cohort, id, type, measure, scores) %>%
        unite(scores, col = est.all, sep = ":", remove = FALSE) %>%
        mutate(ref = map_dbl(est.all, get_ref),
               k = map_dbl(est.all, get_k)) %>%
        select(-est.all)

    out <- list("working.estimates" = working.estimates,
                scores = scores, formulas = formulas, fn = fn, lbl = lbl)
    class(out) <- "mscraw"
    out
}

#' @describeIn compute_performance Print raw performance estimates
#' @param x Set of performance estimates calculated with \code{\link{copute_performance}}
#' @export
print.mscraw <- function(x, ...){
  x.apparent <- x$working.estimates %>%
    filter(id == "Apparent")
  print(x.apparent, ...)
}

#' @describeIn compute_performance Print summary of raw performance estimates
#' @param nonpar  Should nonparametric summary statistics (median [IQR]) be reported? (TRUE)
#' @param NArm Should NAs be removed before calculated summary statistics? (TRUE)
#' @export
summary.mscraw <- function(x, nonpar = TRUE, NArm = TRUE, ...){
  sc <- x$scores
  x.apparent <- x$working.estimates %>%
    filter(id == "Apparent") %>%
    select(cohort, sc)
  q1 <- partial(quantile, probs = 0.25)
  q3 <- partial(quantile, probs = 0.75)
  nonmiss <- function(x, na.rm = TRUE) sum(!is.na(x))
  if(nonpar){
    fns <- vars(nonmiss, median, q1, q3)
  } else {
    fns <- vars(nonmiss, mean, sd)
  }
  x.apparent  %>%
    gather(sc, key = "score", value = "value") %>%
    group_by(score) %>%
    summarize_at("value", fns, na.rm = NArm) %>%
    mutate(performance = x$lbl) %>%
    select(score, performance, everything())
}

#' @describeIn compute_performance Plot variability of raw performance estimates across bootstrap samples using points
#' @inheritParams print.mscraw
#' @export
points.mscraw <- function(perf.estimates){
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  working.estimates <- perf.estimates$working.estimates
  scores <- perf.estimates$scores
  lbl <- perf.estimates$lbl
  
  we <- working.estimates %>%
    select(cohort, id, scores) %>%
    gather(scores, key = "score", value = "performance") %>%
    mutate(id = ifelse(id == "Apparent", "Apparent", "Bootstrap"))
  
  bs <- we %>%
    filter(id == "Bootstrap")
  ap <- we %>%
    filter(id == "Apparent")
  
  
  ggplot(aes(cohort, performance), data = bs) +
    geom_jitter(color = "gray", alpha = 0.5) +
    geom_point(data = ap) +
    facet_wrap( ~ score) +
    coord_flip() +
    ylim(-2, 5)
}

#' @describeIn compute_performance Plot variability of raw performance estimates across bootstrap samples using lines (density plots)
#' @inheritParams print.mscraw
#' @export
lines.mscraw <- function(perf.estimates) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package \"ggplot2\" needed for this function to work. Please install it.",
      call. = FALSE
    )
  }
  working.estimates <- perf.estimates$working.estimates
  scores <- perf.estimates$scores
  lbl <- perf.estimates$lbl
  
  working.estimates %>%
    select(-id, -measure, -ref, -k) %>%
    gather(scores, key = "score", value = "value") %>%
    filter(type != "apparent") %>%
    ggplot(aes(value, group = cohort)) +
    geom_density() +
    xlab(lbl) +
    facet_wrap(~ score)
}

