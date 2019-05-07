#' @title Various utility functions
#'
#' @describeIn utils Make design matrix
#' @param trt1 vector of the 1st treatment / score in the contrast
#' @param trt2 vector of the 2nd treatment / score in the contrast
#' @param ref The name of the reference treatment / score (character)
#' @param sc A vector containing the full list of scores considered, so that the scores are not put into alphabetical order. This keeps the order of the scores in later network meta-analysis models the same as in other places.
#'
#' @details  This function has been adapted slightly from the supplementary material of  \href{doi://10.1186/s12874-016-0184-5}{Law et al 2016}: 
#' 
#' Law, M.; Jackson, D.; Turner, R.; Rhodes, K. & Viechtbauer, W. Two new methods to fit models for network meta-analysis with random inconsistency effects BMC Medical Research Methodology, 2016, 16, 87.
#' 
#'
#' @return A design matrix
#' 
#' @importFrom magrittr %>%
#' @importFrom stats model.matrix
#' @import dplyr
#' @importFrom tidyr spread gather unite
#' @examples
#' contrmat(letters[c(1, 1, 2, 2, 3)], letters[c(2, 3, 4, 5, 6)], "a", sc = letters[1:6])
#' contrmat(letters[c(1, 1, 2, 2, 3)], letters[c(2, 3, 4, 5, 6)], "a", sc = letters[6:1])
#' contrmat(letters[c(1, 1, 2, 2, 3)], letters[c(2, 3, 4, 5, 6)], "c", sc = letters[6:1])
contrmat <- function(trt1, trt2, ref, sc = NULL){
    all.lvls <- unique(c(levels(factor(trt1)), levels(factor(trt2))))
    if(is.null(sc)) sc <- all.lvls
    all.lvls <- factor(all.lvls, sc)
    all.lvls <- sort(all.lvls)
    trt1 <- factor(trt1, levels=all.lvls)
    trt2 <- factor(trt2, levels=all.lvls)
    X <- model.matrix(~ trt2 - 1) - model.matrix(~ trt1 - 1)
    colnames(X) <- all.lvls
    if (missing(ref))
        ref <- all.lvls[1]
    X[, colnames(X) != ref]
}

#' @describeIn utils Calculate differences between performance measures
#' @param d A structured dataset, as calculated with \code{\link{aggregate_performance}}
#' @return A new dataset with differences calculated
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

#' @describeIn utils Get number of scores
#' @param x A string of scores, pasted together with \code{:}.
#' @return Number of non-missing scores
#'
get_k <- function(x){
    x <- strsplit(x, split = ":")[[1]]
    x[x == "NA"] <- NA
    x <- as.numeric(x)
    if(!all(is.na(x))){
        return(length(which(!is.na(x))))
    } else {
        return(0)
    }
}

#' @describeIn utils Get reference score
#' @return The number of the reference score
#'
#'@examples
#' x <- paste(c(0.025, 0.05, NA, 0.1), sep = ":")
#' y <- paste(c(NA, 0.01, 0.02, 0.05), sep = ":")
#' get_ref(x)
#' get_ref(y)

get_ref <- function(x){
    x <- strsplit(x, split = ":")[[1]]
    x[x == "NA"] <- NA
    x <- as.numeric(x)
    if(!all(is.na(x))){
        return(first(which(!is.na(x))))
    } else {
        return(0)
    }
}



