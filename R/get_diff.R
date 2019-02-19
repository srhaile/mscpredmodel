#' Calculate differences between scores
#'
#' @param d A structured dataset, as calculated with \code{\link{aggregate_performance}}
#'
#' @return A new dataset with differences calculated
#' @export
#'
get_diff <- function(d){
    # see character to name section : https://edwinth.github.io/blog/dplyr-recipes/
    nams <- names(d)
    nams <- nams[!nams %in% c("ref", "k", "id", "measure")]
    this.ref <- max(c(d$ref[d$ref > 0], 0))
    if(this.ref > 0){
        ref.name <- nams[this.ref]
        ref.var <- rlang::sym(ref.name)
    } else {
        ref.name <- ref.var <- NA
    }
    scores.eval <- d %>%
        select(nams) %>%
        summarize_all( function(x) mean(!is.na(x)))
    this.grp <- paste(names(scores.eval)[scores.eval > 0.8], collapse = "-")
    # have to be evaluated at least 80% of the time
    most_common_number_scores <- as.numeric(names(sort(table(d$k), decreasing = TRUE))[1])
    if(most_common_number_scores > 1){
        d <- d %>%
            mutate(ref.score = !! ref.var) %>%
            gather(nams, key = "score", value = "value", -ref.score) %>%
            mutate(value = value - ref.score) %>%
            mutate(value = ifelse(score == ref.name, NA, value)) %>%
            spread(score, value) %>%
            mutate(grp = this.grp,
                   ref = this.ref) %>%
            select(-ref.score) %>%
            select(id, measure, ref, k, nams, grp)
    } else if(most_common_number_scores == 1){
        d <- d %>%
            select(id, measure, ref, k, nams) %>%
            mutate_at(ref.name, function(x) ifelse(!is.na(x), 0, NA)) %>%
            mutate(grp = ref.name)
    } else {
        d <- d %>%
            select(id, measure, ref, k, nams) %>%
            mutate(grp = "")
    }

    d
}
