#' Create league table
#'
#' @param object A model from \code{msc()}.
#' @param low Should the evidence in the lower triangle come from \code{"direct"} (default), \code{"indirect"}, or \code{"network"} evidence. It is sufficient to write the first letter.
#' @param upper Should the evidence in the upper triangle come from \code{"direct"}, \code{"indirect"} (default), or \code{"network"} evidence. It is sufficient to write the first letter.
#' @param dig Number of significant digits to format the results.
#' @param mid Confidence intervals are written as \code{brackets[1]} [lower bound] \code{mid} [upper bound] \code{brackets[2]}. (Default \code{" to "})
#' @param brackets Confidence intervals are written as \code{brackets[1]} [lower bound] \code{mid} [upper bound] \code{brackets[2]}. (Default \code{c("(", ")")})
#'
#' @returns A matrix of performance comparisons, by pair of scores
#' @export
#'
#' @examples 
#' dat <- msc_sample_data()
#' mod <- msc(scores = c("a", "b", "c", "d", "e"),
#'          cohort = "study", outcome = "outcome", subjid = "id", 
#'          data = dat, direct = TRUE, indirect = TRUE,
#'          ref = "all", model = "inconsistency")
#' 
#' 
league_table <- function(object, 
                         low = c('direct', 'indirect', 'network')[1], 
                         upp = c('direct', 'indirect', 'network')[2], 
                         dig = 2, mid = " to ", brackets = c("(", ")")){
    
    l <- match.arg(low, c('direct', 'indirect', 'network'))
    u <- match.arg(upp, c('direct', 'indirect', 'network'))
    
    fmt <- function(x) format(x, nsmall = dig, digits = dig, sci = 1)
    
    est <- lapply(object, function(x) x$models)
    est <- lapply(est, function(x){rownames(x) <- NULL; x})
    for(i in 1:length(est)){
        est[[i]]$perfmeasure <- names(est)[i]
        est[[i]] <- est[[i]][, c(11, 1:10)]
        tmp <- with(est[[i]], paste(fmt(estimate), " ", brackets[1], 
                                    fmt(conf.low), mid, 
                                    fmt(conf.high), brackets[2], sep = ""))
        tmp[is.na(est[[i]]$estimate)] <- NA
        est[[i]]$est_ci <- tmp
    }
    
    avail_scores <- attr(object, "scores")
    k <- length(avail_scores)
    res <- vector("list", length(est))
    
    for(i in 1:length(est)){
        lt <- matrix(NA, nrow = k, ncol = k)
        diag(lt) <- avail_scores
        rownames(lt) <- colnames(lt) <- avail_scores
        
        this_est <- subset(est[[i]], term %in% avail_scores)
        this_lower <- subset(this_est, evidence == l)
        this_upper <- subset(this_est, evidence == u)
        
        for(j in 1:nrow(this_lower)){
            lt[this_lower$term[j], this_lower$ref[j]] <- this_lower$est_ci[j]
            lt[this_upper$ref[j], this_upper$term[j]] <- this_upper$est_ci[j]
        }
        rownames(lt) <- colnames(lt) <- NULL
        res[[i]] <- lt
    }
    names(res) <- names(est)
    res
    
}