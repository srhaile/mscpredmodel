#' Some graphical explorations of performance estimates
#'
#' After \code{\link{compute_performance}}, you may want to explore the performance
#' within each cohort before proceeding any further. For example, how different are
#' the cohorts? How variable are the bootstrap samples? Is the distribution of
#' bootstrap samples approximately normally distributed? Here, we provide two functions
#' to visualize the estimates. \code{\link[=points.mscraw]{points}} plots each of the
#' bootstrap sample estimates by cohort and score. \code{\link[=lines.mscraw]{lines}}
#' plots the density of the bootstrap sample estimates for each cohort, stratified by
#' score.
#'
#' @param perf.estimates
#'
#' @return A plot is printed.
#' @export
#'
#' @details Requires \code{\link[ggplot2]{ggplot}}.
#'
#' @examples
#' dat <- msc_sample_data()
#' bssamp <- get_bs_samples(dat, id, cohort, outcome, n.samples = 10, a, b, d, e, f)
#' perf <- compute_performance(bssamp, fn = calibration_slope, lbl = "CS")
#' points(perf)
#' lines(perf)
#' 
NULL