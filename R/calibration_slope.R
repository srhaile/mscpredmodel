#' Compute performance in terms of various typical calibration and discrimination measures.
#' 
#' Here we provide several functions to compute some typical performance measures 
#' for calibration and discrimination. This is however not intended to be an 
#' exhaustive set of performance measures.
#' 
#' Please note that \code{\link{compute_performance}} will use \code{try}, and return \code{NA} if the function has an error. Of the potentially large number of cohort, score and bootstrap sample, this was a straightforward way to prevent \code{\link{compute_performance}} from returning an error for the whole dataset. However, if the function to compute performance generally does not work (for example, if the package \code{pROC} is required but not loaded), this behavior also prevents warning message for, say, unloaded R packages from being printed. See the example for how to redefine the provided functions with a transformation. 
#' 
#' @importFrom stats model.matrix pnorm qnorm glm qlogis offset update na.omit as.formula coef
#' @importFrom pROC roc auc
#' 
#' @param dd A dataset. 
#' @param fm The formula that will be called by the model, of the form \code{outcome ~ score} (character).
#'
#' @return A single performance measure (numeric). 
#' @examples
#' n <- 100
#' x <- rnorm(n)
#' y <- as.numeric(rnorm(n, x) > 1)
#' dat <- data.frame(x, y)
#' 
#' # log calibration slope
#' log_cs <- function(dd, fm){
#'     log(calibration_slope(dd, fm))
#' }
#'
#' calibration_slope(dat, "y ~ x")
#' log_cs(dat, "y ~ x")
#'
#' @describeIn calibration_slope Estimate calibration slope
#' @export
calibration_slope <- function(dd, fm){
    fm <- as.formula(fm)
    fm <- update(fm, . ~ qlogis(.))
    m <- glm(fm, dd, family = binomial, na.action = na.omit)
    coef(m)[2]
}

#' @describeIn calibration_slope Estimate calibration-in-the-large
#' @export
calibration_large <- function(dd, fm){
    fm <- as.formula(fm)
    fm <- update(fm, . ~ offset(qlogis(.)))
    m <- glm(fm, dd, family = binomial, na.action = na.omit)
    coef(m)
}

#' @describeIn calibration_slope Estimate c-Statistics / Area under the ROC curve
#' @export
c_statistic <- function(dd, fm){
    if (!requireNamespace("pROC", quietly = TRUE)) {
        stop("Package \"pROC\" needed for this function to work. Please install it.",
             call. = FALSE)
    }
    fm <- as.formula(fm)
    m <- roc(fm, dd)
    auc(m)
}

#' @describeIn calibration_slope Estimate ratio of observed to expected number of events
#' @export
oe_ratio <- function(dd, fm){
    if(is.character(fm)){
        fm <- trimws(strsplit(fm, "~")[[1]])
    } else {
        fm <- as.character(fm)
        fm <- fm[fm != "~"]
    }
    this.dd <- dd[, fm] 
    this.dd <- na.omit(this.dd)
    obs <- this.dd[, fm[1]]
    pred <- this.dd[, fm[2]]
    sum(obs) / sum(pred)
}

#' @describeIn calibration_slope Estimate Brier score
#' @export
brier_score <- function(dd, fm){
    if(is.character(fm)){
        fm <- trimws(strsplit(fm, "~")[[1]])
    } else {
        fm <- as.character(fm)
        fm <- fm[fm != "~"]
    }
    obs <- dd[, fm[1]]
    pred <- dd[, fm[2]]
    brier.score <- (obs - pred) ^ 2
    mean(brier.score, na.rm = TRUE)
}
