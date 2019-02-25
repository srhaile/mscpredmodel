#' Compute performance in terms of various typical calibration and discrimination measures.
#' 
#' Here we provide several functions to compute some typical performance measures 
#' for calibration and discrimination. This is however not intended to be an 
#' exhaustive set of performance measures.
#'
#' @param bss A set of bootstrap samples, stratified by cohort, as computed by \code{\link{get_bs_samples}}. The full bootstrap data is called within the function as \code{analysis(bss)}. See \code{\link{?rsample::bootstraps}} for more details.
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
    m <- glm(fm, analysis(bss), family = binomial)
    coef(m)[2]
}

#' @describeIn calibration_slope Estimate calibration-in-the-large
#' @export
calibration_large <- function(bss, fm){
    fm <- as.formula(fm)
    fm <- update(fm, . ~ offset(qlogis(.)))
    m <- glm(fm, analysis(bss), family = binomial)
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
