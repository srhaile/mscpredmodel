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
#' get_bs_samples(dat, id, study, outcome, n.samples = 10, scores = c("a", "b", "c", "d", "e", "f"), moderators = c("age", "female", "x1"))
get_bs_samples <- function(data, id, cohort, outcome, n.samples = 1000, 
                           scores = NULL, moderators = NULL){
    # for some of these tricks: https://edwinth.github.io/blog/dplyr-recipes/
    if (!requireNamespace("rsample", quietly = TRUE)) {
        stop("Package \"rsample\" needed for this function to work. Please install it.",
             call. = FALSE)
    }
    id_enq <- enquo(id)
    cohort_enq <- enquo(cohort)
    outcome_enq <- enquo(outcome)
    #scores <- quos(...)
    
    if(is.null(scores)){
        stop("Please specify some scores to be compared.")
    }
    scores.to.keep <- scores[scores %in% names(data)]
    scores.to.drop <- scores[!scores %in% names(data)]
    mods.to.keep <- moderators[moderators %in% names(data)]
    mods.to.drop <- moderators[!moderators %in% names(data)]
    if(!all(scores %in% names(data))){
        warning("Some or all scores are not in the dataset!\nKeeping:", scores.to.keep,
                "\nDropping:", scores.to.drop)
    }
    if(!all(moderators %in% names(data)) & !is.null(moderators)){
        warning("Some or all moderators are not in the dataset!\nKeeping:", 
                mods.to.keep, "\nDropping:", mods.to.drop)
    }
    
    bs.sample <- data %>%
        select(!!id_enq, !!cohort_enq, !!outcome_enq, scores, moderators) %>%
        rename(id = !!id_enq,
               cohort = !!cohort_enq,
               outcome = !!outcome_enq) %>%
        group_by(cohort)
   # scores <- names(bs.sample)
    scores <- scores.to.keep
    mods <- mods.to.keep
    bs.sample2  <- bs.sample %>%
        nest() %>%
        mutate(bs = map(data, rsample::bootstraps, times = n.samples, apparent = TRUE)) %>%
        select(-data) %>%
        unnest
    list(bs.sample = bs.sample2,
         scores = scores,
         mods = mods,
         formulas = paste("outcome ~", scores))
}
