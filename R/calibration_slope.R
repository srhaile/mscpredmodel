#' Compute performance in terms of various typical calibration and discrimination measures.
#' 
#' Here we provide several functions to compute some typical performance measures 
#' for calibration and discrimination. This is however not intended to be an 
#' exhaustive set of performance measures.
#' 
#' Please note that \code{\link{compute_performance}} will use \code{\link[purrr]{possibly}} to return \code{NA_real_} if the function has an error. Of the potentially large number of cohort, score and bootstrap sample, this was a straightforward way to prevent \code{\link{compute_performance}} from returning an error for the whole dataset. However, if the function to compute performance generally does not work (for example, if the package \code{pROC} is required but not loaded), this behavior also prevents warning message for, say, unloaded R packages from being printed.
#' 
#' It may be that you 1) prefer some transformation of these performance measures (e.g. log, or logit), or 2) would like to use another performance measure entirely. In the case of using a transformation, consider using \code{\link[purrr]{compose}}. For example, to obtain logit AUC, use the function \code{compose(qlogis, c_statistic)}, or for log O:E ratio, try \code{compose(log, oe_ratio)}. You may of course also define your own function, using these 5 as a template.
#'
#' @param bss A set of bootstrap samples, stratified by cohort, as computed by \code{\link{get_bs_samples}}. The full bootstrap data is called within the function as \code{analysis(bss)}. See \code{\link[rsample]{bootstraps}} for more details.
#' @param fm The formula that will be called by the model, of the form \code{outcome ~ score} (character).
#'
#' @return A single performance measure (numeric). 
#' @export
#'
#' @describeIn calibration_slope Estimate calibration slope
#' @export
calibration_slope <- function(bss, fm){
    fm <- as.formula(fm)
    fm <- update(fm, . ~ qlogis(.))
    m <- glm(fm, analysis(bss), family = binomial, na.action = na.omit)
    coef(m)[2]
}

#' @describeIn calibration_slope Estimate calibration-in-the-large
#' @export
calibration_large <- function(bss, fm){
    fm <- as.formula(fm)
    fm <- update(fm, . ~ offset(qlogis(.)))
    m <- glm(fm, analysis(bss), family = binomial, na.action = na.omit)
    coef(m)
}

#' @describeIn calibration_slope Estimate c-Statistics / Area under the ROC curve
#' @export
c_statistic <- function(bss, fm){
    if (!requireNamespace("pROC", quietly = TRUE)) {
        stop("Package \"pROC\" needed for this function to work. Please install it.",
             call. = FALSE)
    }
    fm <- as.formula(fm)
    m <- roc(fm, analysis(bss))
    auc(m)
}

#' @describeIn calibration_slope Estimate ratio of observed to expected number of events
#' @export
oe_ratio <- function(bss, fm){
    if(is.character(fm)){
        fm <- trimws(strsplit(fm, "~")[[1]])
    } else {
        fm <- as.character(fm)
        fm <- fm[fm != "~"]
    }
    this.bss <- analysis(bss) %>%
        select(fm) %>% 
        drop_na()
    obs <- this.bss %>% pull(fm[1])
    pred <- this.bss %>% pull(fm[2])
    sum(obs) / sum(pred)
}

#' @describeIn calibration_slope Estimate Brier score
#' @export
brier_score <- function(bss, fm){
    if(is.character(fm)){
        fm <- trimws(strsplit(fm, "~")[[1]])
    } else {
        fm <- as.character(fm)
        fm <- fm[fm != "~"]
    }
    obs <- analysis(bss)[, fm[1]]
    pred <- analysis(bss)[, fm[2]]
    brier.score <- (obs - pred) ^ 2
    mean(brier.score, na.rm = TRUE)
}
