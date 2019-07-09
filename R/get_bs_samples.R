#' Generates bootstrap samples
#'
#' @param data Dataset containing subject identifiers, names of cohorts, outcome, and each of the scores
#' @param id name of variable with subject identifiers
#' @param cohort name of variable containing cohorts (the stratification factor, can be a factor, character or numeric)
#' @param outcome name of outcome variable (binomial)
#' @param n.samples Number of bootstrap samples to be generated within each cohort (default 1000)
#' @param scores A vector of variable names that are scores (or their predicted probabilities). The order the scores are here is used in all other functions.
#' @param moderators A vector of variable names that are moderators, that is, covariates which could affect the differences in score performance. We save these variables here, to be aggregated later in \code{\link{aggregate_performance}}. They can then be entered as covariates in \code{\link{msc_direct}}, \code{\link{consistency}}, or any other models.
#'
#' @return A list with 3 elements:
#' \describe{
#'   \item{bs.sample}{A data.frame containing the stratified bootstrap samples, split by cohort}
#'   \item{orig.sample}{A data.frame containing the orignal data, split by cohort}
#'   \item{scores}{Unique names of the scores which are also in the dataset}
#'   \item{mods}{A vector of moderators which may be included in subsequent models}
#'   \item{formulas}{vector of formulas that can be used in any function which calculates performance measures (e.g. \code{\link{calibration_slope}})}
#' }
#' 
#' @importFrom rsample bootstraps
#' @export
#'
#' @examples
#' dat <- msc_sample_data()
#' set.seed(12345)
#' get_bs_samples(dat, id, study, outcome, scores = letters[1:4], moderators = "age")

get_bs_samples <- function(data, id, cohort, outcome, n.samples = 1000, 
                           scores = NULL, moderators = NULL){
    mf <- match.call()
    id      <- as.character(mf[[match("id", names(mf))]])
    cohort     <- as.character(mf[[match("cohort", names(mf))]])
    outcome <- as.character(mf[[match("outcome", names(mf))]])
    
    if(is.null(scores)){
        stop("Please specify some scores to be compared.")
    }
    scores.to.keep <- unique(scores[scores %in% names(data)])
    scores.to.drop <- c(scores[!scores %in% names(data)], scores[scores %in% c("id", "cohort")])
    mods.to.keep <- unique(moderators[moderators %in% names(data)])
    mods.to.drop <- moderators[!moderators %in% names(data)]
    if(!all(scores %in% names(data))){
        warning("Some scores are not in the dataset!\nKeeping: ", 
                paste(scores.to.keep, collapse = ", "),
                "\nDropping: ", paste(scores.to.drop, collapse = ", "))
    }
    if(!all(moderators %in% names(data)) & !is.null(moderators)){
        warning("Some moderators are not in the dataset!\nKeeping:", 
                paste(mods.to.keep, collapse = ", "), 
                "\nDropping: ", paste(mods.to.drop, collapse = ", "))
    }
    scores <- scores.to.keep
    mods <- mods.to.keep
    
    sm <- data[, c(id, cohort, outcome, scores, mods)]
    names(sm)[1:3] <- c("id", "cohort", "outcome")
    
    fm <- paste("outcome ~", scores)
    
    spl <- split(sm, sm$cohort)
    
    if (!requireNamespace("rsample", quietly = TRUE)) {
        stop("Package \"rsample\" needed for this function to work. Please install it.",
             call. = FALSE)
    }
    bss <- lapply(spl, bootstraps, times = n.samples, apparent = TRUE)
    
    list(bs.sample = bss,
         orig.sample = spl,
         scores = scores,
         mods = mods,
         formulas = fm)
} 


