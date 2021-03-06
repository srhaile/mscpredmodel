#' Aggregate performance measures by cohort
#'
#' @importFrom stats model.matrix
#' @importFrom metafor bldiag
#' 
#' @param perf.estimates A set of performance estimates of class \code{mscraw}, as computed by \code{\link{compute_performance}}
#' @param reference The name of the reference score (default NULL, the first score is the reference). This will be the reference level of the scores in any later models.
#' @param design.levels A character vector of alternate short names for the scores, to be used in naming the designs. Default is \code{\link{LETTERS}}, so that a possible design would be \code{AB} or \code{ACD}, instead of \code{score1score2} or \code{score1score3score4}. The design variable is used in the definition of the random effects. See \code{\link{consistency}}.
#' @param fn.mods A function used to summarize the moderators within cohorts. Moderators should be numeric as the mean is computed by default. Factor variables should converted to numeric prior to use in \code{\link{get_bs_samples}}. For example, if variable x2 has levels A, B, C and D, convert to x2f = 1 if x2 = {A, B} and 0, otherwise. If NULL, the default mean(x, na.rm = TRUE) is used.
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
#' 
#' @details The aggregation of moderators here is different than what the function \code{\link{casemix}} is doing. \code{\link{casemix}} shows the actual levels of the moderators, either as median and IQR (or boxplots) for continuous variables, or tables (or barplots) for factor variables. In \code{aggregate_performance} we assume a single number is needed to describe the overall value of the moderator, which is most commonly the mean. Factor variables with two levels (e.g. A and B) should be converted to 0/1 variables. Unordered factor variables with more than 2 levels (e.g. A, B, C, and D) should be collapsed to 0/1 variables (for example 0 = {A, B}, 1 = {C, D}). Ordered factor variables (e.g. I, II, III, IV) should be converted to numeric variables (e.g. 1, 2, 3, 4). Any factor variables will be dropped in \code{aggregate_performance}.
#' 
#' @export
#'
#' @examples
#' dat <- msc_sample_data()
#' bssamp <- get_bs_samples(dat, id, study, outcome, n.samples = 5, 
#'                   scores = c("a", "b", "c", "d", "e", "f"), 
#'                   moderators = c("age", "female", "x1", "sex"))
#' perf <- compute_performance(bssamp, fn = calibration_slope, lbl = "CS")
#' agg <- aggregate_performance(perf)
#' head(agg)
aggregate_performance <- function(perf.estimates, reference = NULL,
                                  design.levels = LETTERS,
                                  fn.mods = NULL){
    scores <- perf.estimates$scores
    mods <- perf.estimates$mods
    moderators <- perf.estimates$moderators
    fn <- perf.estimates$fn
    lbl <- perf.estimates$lbl
    working.estimates <- perf.estimates$working.estimates
    
    if(is.null(reference)){
        reference <- scores[1]
    } else if (!reference %in% scores){
        warning("reference score was not one of the scores. The reference has been changed to ", scores[1])
        reference <- scores[1]
    }
    
    classmods <- sapply(moderators[[1]], class)
    check.classmods <- classmods[mods] != "factor"
    
    if(any(!check.classmods)){
      message("Removing moderators classified as factors: ", paste(names(check.classmods)[!check.classmods], collapse = ", "), 
              "; consider recoding these to numeric")
      mods <- names(check.classmods)[check.classmods]
    }
    
    
    if(is.null(fn.mods) & !is.null(mods)){
        #message("Since fn.mods was not specified, moderators will be aggregated with mean(x, na.rm = TRUE).")
        fn.mods <- function(x) mean(x, na.rm = TRUE)
    } else if(!length(fn.mods) %in% c(1, length(mods))){
        warning("There should be", length(mods), "or 1 function to aggregate the moderators defined. Reverting to mean(x, na.rm = TRUE)")
        fn.mods <- function(x) mean(x, na.rm = TRUE)
    }
    
    refs <- sapply(working.estimates, get_ref, s = scores, ref = reference)
    sc <- sapply(working.estimates, get_scores, s = scores)
    designs <- sapply(sc, get_design, dl = design.levels, s = scores)
    
    sc <- mapply(function(x, y) x[!x %in% y], sc, refs)
    we <- lapply(working.estimates, get_diff, s = scores, ref = reference)

    yi <- mapply(get_est, we, sc)
    vi <- mapply(get_var, we, sc)
    
    k <- sapply(yi, length)
    cohort <- factor(rep(names(yi), k))
    design <- rep(designs, k)
    yi <- unlist(yi)
    sc <- unlist(sc)
    refs <- unlist(rep(refs, k))
    contr <- paste(sc, refs, sep = "-")
    designmat <- contrmat(refs, sc, ref = reference, sc = scores)
    to.keep <- lapply(vi, length) > 0
    vi <- metafor::bldiag(lapply(vi[to.keep], as.matrix))

    
    if(!is.null(mods)){
        outmods <- data.frame("cohort" = names(moderators))
        for(i in mods){
            tmp <- lapply(moderators, function(x) as.vector(x[, i]))
            outmods[, i] <- sapply(tmp, fn.mods)
        }
    # modagg <- moderators %>%
    #     group_by(cohort) %>%
    #     summarize_at(mods, fn.mods) 
    # modagg <- left_join(cohortd, modagg, by = "cohort")
    } else {
        outmods <- NULL
    }
    
    outmods <- merge(data.frame(cohort = cohort), outmods, sort = FALSE)
    
    out <- list(cohort, yi, vi, contr, design, designmat, sc, refs,
                lbl, fn, scores, reference, mods, outmods)
    names(out) <- c("cohort", "yi", "vi", "contr", "design", 
                    "design.matrix", "s1", "s2" ,"lbl", "fn", "scores", "ref",
                    "mods", "moderators")
    class(out) <- "mscagg"
    return(out)
}

#' @describeIn aggregate_performance Print basic aggregated performance measures
#' @export
print.mscagg <- function(x, ...){
    aggdat <- with(x, data.frame(cohort, yi, vi = diag(vi), 
            contr, design, lbl))
    aggdat$cohort <- as.character(aggdat$cohort)
    aggdat <- merge(aggdat, x$moderators, by = "cohort")
    print(aggdat, ...)
}

