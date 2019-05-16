#' MSC network meta-analysis models
#'
#' @param x A set of aggregated performance score data, as computed with the \code{\link{aggregate_performance}} function.
#' @param mods Which moderators should be included in the model? These can be any of the same moderators included in \code{\link{get_bs_samples}}, all others will be ignored.
#' @param ... Any other arguments will be passed directly to \code{\link[metafor]{rma.mv}}
#'
#' @return The results of an \code{\link[metafor]{rma.mv}} multivariate/multilevel Linear (mixed-effects) model (class "rma.mv"), with the following additional components, which can be passed along to other functions:
#' #' \describe{
#'   \item{reference}{name of reference score}
#'   \item{scores}{vector of scores analysed}
#'   \item{model}{If \code{\link{consistency}} was used, "consistency", else "inconsistency"}
#'   \item{performance}{Label of performance, as used in previous functions}
#' }
#' 
#' @details The consistency and inconsistency models are those found in \href{doi://10.1186/s12874-016-0184-5}{Law et al 2016}: 
#' 
#' Law, M.; Jackson, D.; Turner, R.; Rhodes, K. & Viechtbauer, W. Two new methods to fit models for network meta-analysis with random inconsistency effects BMC Medical Research Methodology, 2016, 16, 87.
#' 
#' Specifically, we fit one of two models using "Jackson's Model", as described in the paper, which differ only in their random effects:
#' \describe{
#'   \item{consistency}{First item} random contrast within study
#'   \item{inconsistency}{Second item} random contrast within study, and random contrast within design
#' }
#' @import dplyr
#' @import metafor
#' @export
#'
#' @examples
#' dat <- msc_sample_data()
#' bssamp <- get_bs_samples(dat, id, study, outcome, n.samples = 10, 
#'                   scores = c("a", "b", "c", "d", "e", "f"), 
#'                   moderators = c("age", "female", "x1"))
#' perf <- compute_performance(bssamp, fn = calibration_slope, lbl = "CS")
#' agg <- aggregate_performance(perf)
#' consistency(agg)
#' inconsistency(agg)
#' agg.c <- aggregate_performance(perf, "c")
#' consistency(agg.c)
consistency <- function(x, mods = NULL, ...){
    if(class(x) != "mscagg") stop("Requires aggregated data of class(x) = 'mscagg' (created using the aggregate_performance function)")
    if (!requireNamespace("metafor", quietly = TRUE)) {
        stop("Package \"metafor\" needed for this function to work. Please install it.",
             call. = FALSE)
    }
    if(!is.null(mods)){
        mods.to.keep <- mods[mods %in% x$mods]
        mods.to.drop <- mods[!mods %in% x$mods]
        mods <- mods.to.keep
        if(length(mods.to.drop) > 0) warning("Dropping moderators because not in aggregated dataset: ", mods.to.drop)
        x$dm <- cbind(x$design.matrix, as.matrix(x$moderators[, mods.to.keep]))
        if(colnames(x$dm)[1] == "") colnames(x$dm)[1] <- "intrcpt"
    } else {
        x$dm <- x$design.matrix
    }
    
    if(length(x$yi) > 1){
        modC <- metafor::rma.mv(yi, vi, mods =  dm, 
                                slab = cohort, intercept = FALSE, 
                                data = x,
                               random = ~ contr | cohort, rho = 0.5, ...)
    } else if(length(x$yi) == 1){
        modC <-  metafor::rma(x$yi, x$vi, mods = x$dm, slab = x$cohort, intercept = FALSE,  ...)
    }
    modC$reference <- x$ref
    modC$scores <- x$scores
    modC$mods <- mods
    modC$model <- "consistency"
    modC$performance <- x$lbl
    return(modC)
}

#' @describeIn consistency Estimate differences in score performance using inconsistency model
#' @export
inconsistency <- function(x, mods = NULL, ...){
    if(class(x) != "mscagg") stop("Requires aggregated data of class(x) = 'mscagg' (created using the aggregate_performance function)")
    if (!requireNamespace("metafor", quietly = TRUE)) {
        stop("Package \"metafor\" needed for this function to work. Please install it.",
             call. = FALSE)
    }
    if(!is.null(mods)){
        mods.to.keep <- mods[mods %in% x$mods]
        mods.to.drop <- mods[!mods %in% x$mods]
        if(length(mods.to.drop) > 0) warning("Dropping moderators because not in aggregated dataset: ", mods.to.drop)
        x$dm <- cbind(x$design.matrix, as.matrix(x$moderators[, mods.to.keep]))
        if(colnames(x$dm)[1] == "") colnames(x$dm)[1] <- "intrcpt"
    } else {
        x$dm <- x$design.matrix
    }
    if(length(x$yi) > 1){
        modI <- metafor::rma.mv(x$yi, x$vi, mods = x$dm, slab = x$cohort, 
                               intercept = FALSE, 
                               random = list(~ x$contr | x$cohort, 
                                             ~ x$contr | x$design), 
                               rho = 0.5, phi = 0.5, ...)
    } else if(length(x$yi) == 1){
        modI <-  metafor::rma(x$yi, x$vi, mods = x$dm, slab = x$cohort, intercept = FALSE, ...)
    }
    modI$reference <- x$ref
    modI$scores <- x$scores
    modI$mods <- mods
    modI$model <- "inconsistency"
    modI$performance <- x$lbl
    return(modI)
}

