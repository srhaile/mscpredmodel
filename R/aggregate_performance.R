#' Aggregate performance measures by cohort
#'
#' @importFrom magrittr %>%
#' @importFrom stats model.matrix
#' @import dplyr
#' @importFrom tidyr spread gather unite nest
#' @importFrom purrr map map2 map2_dbl map_int map2_chr possibly
#' 
#' @param perf.estimates A set of performance estimates of class \code{mscraw}, as computed by \code{\link{compute_performance}}
#' @param reference The name of the reference score (default NULL, the first score is the reference). This will be the reference level of the scores in any later models.
#' @param design.levels A character vector of alternate short names for the scores, to be used in naming the designs. Default is \code{\link{LETTERS}}, so that a possible design would be \code{AB} or \code{ACD}, instead of \code{score1score2} or \code{score1score3score4}. The design variable is used in the definition of the random effects. See \code{\link{consistency}}.
#' @param fn.mods A function used to summarize the moderators within cohorts. If NULL, the default mean(x, na.rm = TRUE) is used.
#' @param x An object of class \code{mscagg}, returned from \code{\link{aggregate_performance}}.
#' @param ... Other arguments to be passed to \code{\link{print}}.
#'
#' @return A list with the following elements, of class \code{mscagg}
#' \describe{
#'   \item{cohorts}{List of cohorts}
#'   \item{yi}{Differences in score performance}
#'   \item{vi}{Variance-covariance matrix for yi}
#'   \item{contr}{Score contrasts}
#'   \item{design}{"Design" of the study (that is, which scores could be calculated)}
#'   \item{design.matrix}{Design matrix, corresponding to \code{contr}}
#'   \item{lbl}{A label for the analysis, which shows up in plots and output datasets}
#'   \item{fn}{The function used to compute performance, for example \code{\link{calibration_slope}}}
#'   \item{scores}{A vector of the scores used}
#'   \item{ref}{The reference score}
#'   \item{mods}{A vector of names of the moderators}
#'   \item{moderators}{A dataframe containing aggregated moderators}
#' }
#' @export
#'
#' @examples
#' dat <- msc_sample_data()
#' bssamp <- get_bs_samples(dat, id, study, outcome, n.samples = 10, 
#'                   scores = c("a", "b", "c", "d", "e", "f"), 
#'                   moderators = c("age", "female", "x1"))
#' perf <- compute_performance(bssamp, fn = calibration_slope, lbl = "CS")
#' agg <- aggregate_performance(perf)
#' agg
aggregate_performance <- function(perf.estimates, reference = NULL, 
                                  design.levels = LETTERS,
                                  fn.mods = NULL){
    scores <- perf.estimates$scores
    mods <- perf.estimates$mods
    moderators <- perf.estimates$moderators
    #formulas <- perf.estimates$formulas
    fn <- perf.estimates$fn
    lbl <- perf.estimates$lbl
    working.estimates <- perf.estimates$working.estimates
    
    if(is.null(reference)){
        reference <- scores[1]
    } else if (!reference %in% scores){
        warning("reference score was not one of the scores. The reference has been changed to", scores[1])
        reference <- scores[1]
    }
    
    if(is.null(fn.mods) & !is.null(mods)){
        #message("Since fn.mods was not specified, moderators will be aggregated with mean(x, na.rm = TRUE).")
        fn.mods <- partial(mean, na.rm = TRUE)
    } else if(!length(fn.mods) %in% c(1, length(mods))){
        warning("There should be", length(mods), "or 1 function to aggregate the moderators defined. Reverting to mean(x, na.rm = TRUE)")
        fn.mods <- partial(mean, na.rm = TRUE)
    }
    
    tmp <- working.estimates %>% 
        group_by(cohort, .data$type) %>%
        nest() %>%
        mutate(data = map(data, get_diff)) %>% 
        unnest %>% 
        filter(.data$k > 1) %>%
        group_by(cohort, .data$type, .data$ref) %>%
        nest(scores) %>%
        mutate(yi = map(data, ~ apply(., 2, mean, na.rm = TRUE)),
               vi = map(data, ~ var(., use = "pairwise"))) %>%
        mutate(scores.eval = map(yi, ~ which(!is.na(.)))) %>%
        mutate(num.scores = map_int(.data$scores.eval, length)) %>%
        filter(.data$num.scores > 0) %>%
        select(-.data$num.scores) %>%
        mutate(yi = map2(yi, .data$scores.eval, function(x, k) x[k]),
               vi = map2(vi, .data$scores.eval, function(x, k) x[k, k]))
    
    yi <- tmp %>% 
        filter(.data$type == "apparent") %>% 
        select(cohort, yi, .data$scores.eval, .data$ref) %>% 
        unnest %>%
        mutate(score = .data$scores.eval) %>%
        select(-.data$scores.eval)
    vis <- tmp %>% 
        filter(.data$type != "apparent") %>% 
        select(vi)
    vi <- metafor::bldiag(lapply(vis$vi, as.matrix))
    
    y <- yi$yi
    v <- vi
    cohort <- factor(yi$cohort)
    cohortd <- tibble("cohort" = as.character(cohort))
    
    if(!is.null(mods)){
    modagg <- moderators %>%
        group_by(cohort) %>%
        summarize_at(mods, fn.mods) 
    modagg <- left_join(cohortd, modagg, by = "cohort")
    } else {
        modagg <- NULL
    }
    
    dmat <- contrmat(scores[yi$ref], scores[yi$score], reference, scores)
    contr <- yi %>% 
        mutate(ref = scores[.data$ref], 
               score = scores[.data$score], 
               contr = paste(.data$score, .data$ref, sep = "-")) %>%
        select(contr) %>%
        unlist
    design <- yi %>%
        group_by(cohort) %>%
        mutate(design = map2_chr(.data$ref, .data$score, 
                    ~ paste(design.levels[unique(sort(c(.data$ref, .data$score)))], 
                                                     collapse = ""))) %>%
        mutate(design = gsub("slope", "", design)) %>%
        ungroup() %>%
        select(design) %>%
        unlist
    
    out <- list(cohort, y, v, contr, design, dmat, lbl, fn, scores, reference, mods, modagg)
    names(out) <- c("cohort", "yi", "vi", "contr", "design", 
                    "design.matrix", "lbl", "fn", "scores", "ref",
                    "mods", "moderators")
    class(out) <- "mscagg"
    return(out)
}

#' @describeIn aggregate_performance Print basic aggregated performance measures
#' @export
print.mscagg <- function(x, ...){
    aggdat <- with(x, tibble(cohort, yi, vi = diag(vi), 
            contr, design, lbl)) %>% 
        mutate(cohort = as.character(.data$cohort)) %>%
        full_join(x$moderators, by = "cohort")
    print(aggdat, ...)
}

