#' Generates bootstrap samples
#'
#' @param data Dataset containing subject identifiers, names of cohorts, outcome, and each of the scores
#' @param id name of variable with subject identifiers
#' @param cohorts name of variable containing cohorts (the stratification factor, can be a factor, character or numeric)
#' @param outcome name of outcome variable (binomial)
#' @param n.samples Number of bootstrap samples to be generated within each cohort (default 1000)
#' @param ... Any other arguments are taken to be the names of the variables containinng the scores. The order the scores are here is used in all other functions.
#'
#' @return A list with 3 elements:
#' \describe{
#'   \item{bs.sample}{A tibble containing the stratified bootstrap samples}
#'   \item{scores}{Names of the scores as given in \code{...} in the function call}
#'   \item{formulas}{vector of formulas that can be used in any function which calculates performance measures (e.g. \code{\link{calibration_slope}})}
#' }
#' @export
#'
#' @examples
#' dat <- msc_sample_data()
#' get_bs_samples(dat, id, cohort, outcome, n.samples = 10, a, b, d, e, f)
get_bs_samples <- function(data, id, cohorts, outcome, n.samples = 1000, ...){
    # for some of these tricks: https://edwinth.github.io/blog/dplyr-recipes/
    if (!requireNamespace("rsample", quietly = TRUE)) {
        stop("Package \"rsample\" needed for this function to work. Please install it.",
             call. = FALSE)
    }
    id_enq <- enquo(id)
    cohorts_enq <- enquo(cohorts)
    outcome_enq <- enquo(outcome)
    scores <- quos(...)
    bs.sample <- data %>%
        select(!!id_enq, !!cohorts_enq, !!outcome_enq, !!!scores) %>%
        rename(id = !!id_enq,
               cohort = !!cohorts_enq,
               outcome = !!outcome_enq) %>%
        group_by(cohort)
    scores <- names(bs.sample)
    scores <- scores[!scores %in% c("id", "cohort", "outcome")]
    bs.sample2  <- bs.sample %>%
        nest() %>%
        mutate(bs = map(data, bootstraps, times = n.samples, apparent = TRUE)) %>%
        select(-data) %>%
        unnest
    list(bs.sample = bs.sample2,
         scores = scores,
         formulas = paste("outcome ~", scores))
}
