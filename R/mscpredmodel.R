#' @title mscpredmodel: Multiple Score Comparison of Prediction Models
#'
#' @description The mscpredmodel package provides a number of functions to facilitate
#' multiple score comparison (MSC), a network meta-analytic approach to 
#' comparing multiple prediction models or prognostic scores using 
#' individual patient data (IPD). 
#' 
#' @section Main Functions:
#' The main mscpredmodel functions are as follows, in approximately the order they might be used in a data analysis:
#' \describe{
#'   \item{\code{\link{msc_sample_data}}}{Produces a simulated dataset in order to try out the package}
#'   \item{\code{\link{get_bs_samples}}}{Generate bootstrap samples, stratified by cohort}
#'   \item{\code{\link{compute_performance}}}{Compute a performance measure for each of the scores, stratified by cohort, in each of the bootstrap samples. One such performance measure would be, for example, \code{\link{calibration_slope}}}
#'   \item{\code{\link{aggregate_performance}}}{Produces the aggregated performance and its empirically estimated variance-covariance matrix for each cohort, to be used in the (in)consistency models.}
#' }
#'
#' @docType package
#' @name mscpredmodel
#' 
NULL