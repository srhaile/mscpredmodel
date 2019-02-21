#' Aggregate performance measures by cohort
#'
#' @param perf.estimates A set of performance estimates of class \code{mscraw}, as computed by \code{\link{compute_performance}}
#' @param reference The name of the reference score (default NULL, the first score is the reference). This will be the reference level of the scores in any later models.
#' @param design.levels A character vector of alternate short names for the scores, to be used in naming the designs. Default is \code{\link{LETTERS}}, so that a possible design would be \code{AB} or \code{ACD}, instead of \code{score1score2} or \code{score1score3score4}. The design variable is used in the definition of the random effects. See \code{\link{consistency}}.
#'
#' @return A list with the following elements, of class \code{mscagg}
#' \describe{
#'   \item{cohorts}{First item}
#'   \item{yi}{Second item}
#'   \item{vi}{First item}
#'   \item{contr}{Second item}
#'   \item{design}{First item}
#'   \item{design.matrix}{First item}
#'   \item{lbl}{Second item}
#'   \item{fn}{First item}
#'   \item{scores}{Second item}
#'   \item{ref}{The reference score}
#' }
#' @export
#'
#' @examples
#' dat <- msc_sample_data()
#' bssamp <- get_bs_samples(dat, id, cohort, outcome, n.samples = 10, a, b, c, d, e)
#' perf <- compute_performance(bssamp, fn = calibration_slope, lbl = "CS")
#' agg <- aggregate_performance(perf)
#' agg
aggregate_performance <- function(perf.estimates, reference = NULL, 
                                  design.levels = LETTERS){
    scores <- perf.estimates$scores
    formulas <- perf.estimates$formulas
    fn <- perf.estimates$fn
    lbl <- perf.estimates$lbl
    working.estimates <- perf.estimates$working.estimates
    
    if(is.null(reference)){
        reference <- scores[1]
    } else if (!reference %in% scores){
        warning("reference score was not one of the scores. The reference has been changed to", scores[1])
        reference <- scores[1]
    }
    
    tmp <- working.estimates %>% 
        group_by(cohort, type) %>%
        nest() %>%
        mutate(data = map(data, get_diff)) %>% 
        unnest %>% 
        filter(k > 1) %>%
        group_by(cohort, type, ref) %>%
        nest(scores) %>%
        mutate(yi = map(data, ~ apply(., 2, mean, na.rm = TRUE)),
               vi = map(data, ~ var(., use = "pairwise"))) %>%
        mutate(scores.eval = map(yi, ~ which(!is.na(.)))) %>%
        mutate(num.scores = map_int(scores.eval, length)) %>%
        filter(num.scores > 0) %>%
        select(-num.scores) %>%
        mutate(yi = map2(yi, scores.eval, function(x, k) x[k]),
               vi = map2(vi, scores.eval, function(x, k) x[k, k]))
    
    yi <- tmp %>% 
        filter(type == "apparent") %>% 
        select(cohort, yi, scores.eval, ref) %>% 
        unnest %>%
        mutate(score = scores.eval) %>%
        select(-scores.eval)
    vis <- tmp %>% 
        filter(type != "apparent") %>% 
        select(vi)
    vi <- bldiag(lapply(vis$vi, as.matrix))
    
    y <- yi$yi
    v <- vi
    cohort <- factor(yi$cohort)
    dmat <- contrmat(scores[yi$ref], scores[yi$score], reference, scores)
    contr <- yi %>% 
        mutate(ref = scores[ref], 
               score = scores[score], 
               contr = paste(score, ref, sep = "-")) %>%
        select(contr) %>%
        unlist
    design <- yi %>%
        group_by(cohort) %>%
        mutate(design = map2_chr(ref, score, ~ paste(design.levels[unique(sort(c(ref, score)))], 
                                                     collapse = ""))) %>%
        mutate(design = gsub("slope", "", design)) %>%
        ungroup() %>%
        select(design) %>%
        unlist
    
    out <- list(cohort, y, v, contr, design, dmat, lbl, fn, scores, reference)
    names(out) <- c("cohorts", "yi", "vi", "contr", "design", 
                    "design.matrix", "lbl", "fn", "scores", "ref")
    class(out) <- "mscagg"
    return(out)
}

#' @describeIn aggregate_performance Print basic aggregated performance measures
#' @export
print.mscagg <- function(x, ...){
    aggdat <- with(x, tibble(cohorts, yi, vi = diag(vi), contr, design, lbl))
    print(aggdat, ...)
}