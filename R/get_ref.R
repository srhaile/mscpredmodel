#' Find reference score
#'
#' Finds the first non-missing scores, and selects it as the "reference" score,
#' for use in defining design matrices. Note intended to be called by the user.
#'
#' @param x A set of scores, pasted together.
#'
#' @return The number of the reference score
#' @export
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
