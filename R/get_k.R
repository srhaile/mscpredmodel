#' Find number of non-missing scores
#'
#' Calculates the number of non-missing scores row-wise. Not intended to be
#' called by the user.
#'
#' @param x A string of scores, pasted together with \code{:}.
#'
#' @return Number of non-missing scores
#' @export
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
