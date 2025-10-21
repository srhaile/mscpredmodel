#' Compute performance in terms of various typical calibration and discrimination measures.
#' 
#' Here we provide several functions to compute some typical performance measures for calibration and discrimination (calibration slope, calibration-in-the-large, c-Statistic / AUC, O:E ratio, Brier score, integrated calibration index, and related E50, E90, and Emax). This is not intended to be an exhaustive set of performance measures.
#' 
#' In some cases, it may be better to perform network meta-analysis on transformed performance measures, for example log calibration slope or logit AUC, as these may be closer to normally distributed. The examples below show how these could be computed.
#' 
#' 
#' @seealso Austin, PC, Steyerberg, EW. The Integrated Calibration Index (ICI) and related metrics for quantifying the calibration of logistic regression models. Statistics in Medicine. 2019; 1– 15. https://doi.org/10.1002/sim.8281
#' 
#' @seealso Debray, T. P., Damen, J. A., Riley, R. D., Snell, K., Reitsma, J. B., Hooft, L., … Moons, K. G. (2018). A framework for meta-analysis of prediction model studies with binary and time-to-event outcomes. Statistical Methods in Medical Research. https://doi.org/10.1177/0962280218785504

#' @seealso Snell KI, Ensor J, Debray TP, et al (2017). Meta-analysis of prediction model performance across multiple studies: which scale helps ensure between-study normality for the C -statistic and calibration measures? Statistical Methods in Medical Research. https://doi.org/10.1177/0962280217705678
#' 
#' @importFrom stats model.matrix pnorm qnorm glm qlogis offset update na.omit as.formula coef predict quantile loess
#' @importFrom pROC roc auc
#' 
#' @param obs A vector of observed scores 
#' @param pred A vector of observed outcomes
#'
#' @return A single performance measure (numeric). 
#' @examples
#' n <- 100
#' predscore <- runif(n)
#' outcome <- as.numeric(rnorm(n, predscore) > 1)
#' 
#' # using a built-in function
#' calibration_slope(outcome, predscore)
#' 
#' 
#' # adapt above to compute log calibration slope
#' log_cs <- function(obs, pred){
#'     log(calibration_slope(obs, pred))
#' }
#' log_cs(outcome, predscore)
#' 
#' # or adapt c_statistic to get logit AUC
#' logit_auc <- function(obs, pred){
#' qlogis(c_statistic(obs, pred))
#' } 
#' logit_auc(outcome, predscore)
#' @describeIn calibration_slope Estimate calibration slope
#' @export
calibration_slope <- function(obs, pred){
    m <- glm(obs ~ qlogis(pred), 
             family = binomial, na.action = na.omit)
    coef(m)[2]
}

#' @describeIn calibration_slope Estimate calibration-in-the-large
#' @export
calibration_large <- function(obs, pred){
    m <- glm(obs ~ offset(qlogis(pred)), 
             family = binomial, na.action = na.omit)
    coef(m)
}

#' @describeIn calibration_slope Estimate c-Statistic / Area under the ROC curve
#' @export

c_statistic <- function(obs, pred){
    if (!requireNamespace("pROC", quietly = TRUE)) {
        stop("Package \"pROC\" needed for this function to work. Please install it.", call. = FALSE)
    }
    out <- pROC::roc(response = obs, 
               predictor = pred, 
               na.rm = TRUE, quiet = TRUE)
    as.numeric(out$auc)
} 

#' @describeIn calibration_slope Estimate ratio of observed to expected number of events
#' @export
oe_ratio <- function(obs, pred){
    cc <- complete.cases(obs, pred)
    sum(obs[cc]) / sum(pred[cc])
}

#' @describeIn calibration_slope Estimate Brier score
#' @export
brier_score <- function(obs, pred){
    mean((obs - pred) ^ 2, na.rm = TRUE)
}

#' @describeIn calibration_slope Estimate Integrated Calibration Index (ICI) (mean)
#' @aliases ICI
#' @export
int_calib_index <- function(obs, pred){
    loess.calibrate <- loess(obs ~ pred)
    P.calibrate <- predict(loess.calibrate, newdata = pred)
    p.diff <- abs(P.calibrate - pred)
    mean(p.diff, na.rm = TRUE)
}

#' @describeIn calibration_slope Estimate Integrated Calibration Index (ICI) (median)
#' @export
E50 <- function(obs, pred){
    loess.calibrate <- loess(obs ~ pred)
    P.calibrate <- predict(loess.calibrate, newdata = pred)
    p.diff <- abs(P.calibrate - pred)
    median(p.diff, na.rm = TRUE)
}

#' @describeIn calibration_slope Estimate Integrated Calibration Index (ICI) (90th percentile)
#' @export
E90 <- function(obs, pred){
    loess.calibrate <- loess(obs ~ pred)
    P.calibrate <- predict(loess.calibrate, newdata = pred)
    p.diff <- abs(P.calibrate - pred)
    quantile(p.diff, probs = 0.9, na.rm = TRUE)
}

#' @describeIn calibration_slope Estimate Integrated Calibration Index (ICI) (maximum)
#' @export
Emax <- function(obs, pred){
    loess.calibrate <- loess(obs ~ pred)
    P.calibrate <- predict(loess.calibrate, newdata = pred)
    p.diff <- abs(P.calibrate - pred)
    max(p.diff, na.rm = TRUE)
}

