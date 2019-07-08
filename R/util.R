#' @title Various utility functions
#'
#' @describeIn util Make design matrix
#' @param trt1 vector of the 1st treatment / score in the contrast
#' @param trt2 vector of the 2nd treatment / score in the contrast
#' @param s1 name of the 1st score in the contrast
#' @param s2 name of the 2nd score in the contrast
#' @param s vector of scores to be kept
#' @param ref The name of the reference treatment / score (character)
#' @param sc A vector containing the full list of scores considered, so that the scores are not put into alphabetical order. This keeps the order of the scores in later network meta-analysis models the same as in other places.
#' @param dl design.levels
#' @param x A variety of possible inputs, depends on the function
#' @param to.check Name of score to be checked
#'
#' @details  This function has been adapted slightly from the supplementary material of  \href{https://doi.org/10.1186/s12874-016-0184-5}{Law et al 2016}: 
#' 
#' @seealso Law, M.; Jackson, D.; Turner, R.; Rhodes, K. & Viechtbauer, W. Two new methods to fit models for network meta-analysis with random inconsistency effects BMC Medical Research Methodology, 2016, 16, 87.
#' 
#' @return A design matrix
#' 
#' @importFrom stats model.matrix var
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

#' @describeIn util Calculate differences between performance measures
#' @return A new dataset with differences calculated
#' 
get_diff <- function(x, s, ref){
    refs <- get_ref(x, s, ref)
    if(refs != ""){
        x$ref <- x[, refs]
        for(i in s) x[, i] <- x[, i] - x$ref
        x[, refs] <- NA
        x <- x[, !names(x) %in% "ref"]
        x
    } else {
        NULL
    }
}

#' @describeIn util Get reference score
#' @return The number of the reference score
#'
get_ref <- function(x, s, ref){
    pick <- x$id == "Apparent"
    sc <- x[pick, s]
    which.nonmiss <- which(!is.na(sc))
    if(length(which.nonmiss) >= 1){
        this.ref <- ref
        if(is.na(sc[this.ref])) this.ref <- names(sc)[which.nonmiss[1]]
    } else {
        this.ref <- ""
    }
    this.ref
}

#' @describeIn util Get non-missing scores
#' @return The names of the observed scores
#'
#'
get_scores <- function(x, s){
    pick <- x$id == "Apparent"
    sc <- x[pick, s]
    names(sc)[!is.na(sc)]
}

#' @describeIn util Get design of the cohort
#' @return A string of the design (combination of scores) of the cohort
#'
get_design <- function(x, dl, s){
    new.labels <- dl[1:length(s)]
    relbl <- factor(x, s, labels = new.labels)
    paste(relbl, collapse = "")
}

#' @describeIn util Get estimated performance
#' @return a vector of performance estimates
#'
get_est <- function(x, s){
    pick <- x$id == "Apparent"
    out <- as.numeric(x[pick, s])
    names(out) <- s
    if(length(out) == 0) out <- numeric(0)
    out
}

#' @describeIn util Get variance estimated performance
#' @return a variance matrix
#'
get_var <- function(x, s){
    pick <- x$id != "Apparent"
    if(any(dim(x[pick, s]) == 0)){
        out <- matrix(nrow = 0, ncol = 0)
    } else {
        out <- var(x[pick, s], na.rm = TRUE, 
                   use = "pairwise.complete.obs")
    }
    if(length(out) == 1)  out <- as.matrix(out)
    out
}

#' @describeIn util Find direct (head-to-head) comparisons
#' @return a data.frame
#'
get_direct <- function(x, s1, s2){
    out <- x[, c("id", s1, s2)]
    to.keep <- !is.na(out[, s1]) & !is.na(out[, s2])
    out[to.keep, ]
}

#' @describeIn util Find indirect comparisons
#' @return a data.frame
# for indirect comparisons (node-splitting approach):
# if pair not in design --> leave it in as is
# if pair == design --> remove entire study
# if pair in design (but there are extra scores also) --> remove 2nd of pair

get_indirect <- function(x, s1, s2, sc){
    ap <- x[x$id == "Apparent", sc]
    nms <- names(ap)[!is.na(ap)]
    has_s1 <- s1 %in% nms
    has_s2 <- s2 %in% nms
    others <- nms[!nms %in% c(s1, s2)]
    has_others <- length(others) > 0
    
    if(has_s1 & has_s1){
        if(has_others){
            out <- x
            out[, s2] <- NA
        } else if (!has_others){
            out <- x
            out[, c(s1, s2)] <- NA
        }
    } else {
        out <- x
    }
    out
}

#' @describeIn util Check if score is still in any of the cohorts?
#' @return TRUE/FALSE
check_combn <- function(x, to.check){
    tmp <- as.numeric(x[x$id == "Apparent", to.check])
    ((length(tmp) > 0) && !is.na(tmp))
}



