#' Some graphical explorations of performance estimates
#'
#' After \code{\link{compute_performance}}, you may want to explore the performance
#' within each cohort before proceeding any further. For example, how different are
#' the cohorts? How variable are the bootstrap samples? Is the distribution of
#' bootstrap samples approximately normally distributed? Here, we provide two functions
#' to visualize the estimates. \code{\link[=points.msc_raw]{points}} plots each of the
#' bootstrap sample estimates by cohort and score. \code{\link[=lines.msc_raw]{lines}}
#' plots the density of the bootstrap sample estimates for each cohort, stratified by
#' score.
#'
#' @param perf.estimates
#'
#' @return A plot is printed.
#' @export
#'
#' @details Requires \code{\link[ggplot2]{ggplot}}.
#'
#' @examples
#' dat <- msc_sample_data()
#' bssamp <- get_bs_samples(dat, id, cohort, outcome, n.samples = 10, a, b, d, e, f)
#' perf <- compute_performance(bssamp, fn = calibration_slope, lbl = "CS")
#' points(perf)
#' lines(perf)
points.msc_raw <- function(perf.estimates){
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Package \"ggplot2\" needed for this function to work. Please install it.",
             call. = FALSE)
    }
    working.estimates <- perf.estimates$working.estimates
    scores <- perf.estimates$scores
    lbl <- perf.estimates$lbl

    we <- working.estimates %>%
        select(cohort, id, scores) %>%
        gather(scores, key = "score", value = "performance") %>%
        mutate(id = ifelse(id == "Apparent", "Apparent", "Bootstrap"))

    bs <- we %>%
        filter(id == "Bootstrap")
    ap <- we %>%
        filter(id == "Apparent")


    ggplot(aes(cohort, performance), data = bs) +
        geom_jitter(color = "gray", alpha = 0.5) +
        geom_point(data = ap) +
        facet_wrap( ~ score) +
        coord_flip() +
        ylim(-2, 5)
}

#' @rdname points.msc_raw

lines.msc_raw <- function(perf.estimates) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop(
            "Package \"ggplot2\" needed for this function to work. Please install it.",
            call. = FALSE
        )
    }
    working.estimates <- perf.estimates$working.estimates
    scores <- perf.estimates$scores
    lbl <- perf.estimates$lbl

    working.estimates %>%
        select(-id, -measure, -ref, -k) %>%
        gather(scores, key = "score", value = "value") %>%
        filter(type != "apparent") %>%
        ggplot(aes(value, group = cohort)) +
        geom_density() +
        xlab(lbl) +
        facet_wrap(~ score)
}
