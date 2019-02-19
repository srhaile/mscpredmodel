#' Construct contrast matrix
#'
#' Constructs a contrast matrix.
#'
#' @param trt1 vector of the 1st treatment / score in the contrast
#' @param trt2 vector of the 2nd treatment / score in the contrast
#' @param ref The name of the reference treatment / score (character)
#' @param sc A vector containing the full list of scores considered, so that the scores are not put into alphabetical order. This keeps the order of the scores in later network meta-analysis models the same as in other places.
#'
#' @references  This function has been adapted slightly from the supplementary material of Law et al 2016: Law, M.; Jackson, D.; Turner, R.; Rhodes, K. & Viechtbauer, W. Two new methods to fit models for network meta-analysis with random inconsistency effects BMC Medical Research Methodology, 2016, 16, 87. \href{doi://10.1186/s12874-016-0184-5}
#'
#' @return A design matrix
#' @export
#'
#' @examples
#' contrmat(letters[c(1, 1, 2, 2, 3)], letters[c(2, 3, 4, 5, 6)], "a", sc = letters[1:6])
#' contrmat(letters[c(1, 1, 2, 2, 3)], letters[c(2, 3, 4, 5, 6)], "a", sc = letters[6:1])
#' contrmat(letters[c(1, 1, 2, 2, 3)], letters[c(2, 3, 4, 5, 6)], "c", sc = letters[6:1])
contrmat <- function(trt1, trt2, ref, sc = NULL) {
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
