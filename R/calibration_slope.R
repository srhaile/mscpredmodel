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
