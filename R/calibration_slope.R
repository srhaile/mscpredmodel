#' Compute calibration slope
#'
#' @param bss A set of bootstrap samples, stratified by cohort, as computed by \code{\link{get_bs_samples}}. The full bootstrap data is called within the function as \code{analysis(bss)}. See \code{\link{?rsample::bootstraps}} for more details.
#' @param fm The formula that will be called by the model, of the form \code{outcome ~ score} (character).
#'
#' @return A single calibration slope (numeric)
#' @export
#'
calibration_slope <- function(bss, fm){
    fm <- as.formula(fm)
    fm <- update(fm, . ~ qlogis(.))
    m <- glm(fm, analysis(bss), family = binomial)
    coef(m)[2]
}

calibration.large <- function(bss, fm){
    fm <- as.formula(fm)
    fm <- update(fm, . ~ offset(qlogis(.)))
    m <- glm(fm, analysis(bss), family = binomial)
    coef(m)
}

c.statistic <- function(bss, fm){
    require(pROC)
    fm <- as.formula(fm)
    m <- roc(fm, analysis(bss))
    auc(m)
}

oe.ratio <- function(bss, fm){
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

brier.score <- function(bss, fm){
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
