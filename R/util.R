#' @title Various internal utility functions
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
#' @return A new dataset with performance measures
#' 

compute_performance <- function(x, k = k, sc = scores, fn = perffn, 
                                observed = outcome,
                                missing_threshold = 0.8){
    out <- rep(NA, k)
    names(out) <- sc
    for(i in 1:k){
        this_score <- sc[i]
        obs <- as.vector(x[, observed])
        if(this_score %in% names(x)){
            pred <- as.vector(x[, this_score])
            pct_miss <- mean(is.na(pred))
            if(pct_miss > missing_threshold){
                out[i] <- NA
            } else if(all(is.na(pred)) | 
                      all(is.na(obs) | 
                          all(obs == 0) | all(obs == 1) |
                          all(obs[!is.na(pred)] == 0))){
                out[i] <- NA
            } else if(length(table(pred)) < 2 | 
                      length(table(obs)) < 2){
                out[i] <- NA
            } else {
                out[i] <- fn(obs, pred)
            }
        } else {
            out[i] <- NA
        }
        
    }
    out <- out[!is.na(out)]
    out
}

#' @describeIn util Calculate variance matrix of differences in performance measures, after bootstrap sampling
#' @return A variance matrix
bootfn <- function(x, R = n.boot, newseed = NULL, k = k, 
                   sc = scores, pfn = perf_fn,
                   missing_threshold = 0.8, 
                   observed = outcome){
    out <- matrix(NA, nrow = R, ncol = k)
    colnames(out) <- sc
    n.obs <- nrow(x)
    if(!is.null(newseed)){
        set.seed(newseed)
    }
    
    for(b in 1:R){
        this_sample <- sample(1:n.obs, n.obs, replace = TRUE)
        for(i in 1:k){
            this_score <- sc[i]
            obs <- x[, observed][this_sample]
            pred <- as.vector(x[, this_score][this_sample])
            pct_miss <- mean(is.na(pred))
            if(pct_miss > missing_threshold){
                out[b, i] <- NA
            } else if(all(is.na(pred)) | all(is.na(obs))){
                out[b, i] <- NA
            } else if(all(obs[!is.na(pred)] == 0) | all(obs[!is.na(pred)] == 1)){
                out[b, i] <- NA
            } else if(length(table(pred)) < 2 | 
                      length(table(obs)) < 2  ){
                
                out[b, i] <- NA
            }  else {
                out[b, i] <- pfn(obs, pred)
            }
        }
    }
    
    out[out == Inf | out == -Inf] <- NA
    
    any_values <- apply(out, 2, function(x) any(!is.na(x)))
    
    if(sum(any_values) >= 2){
        out <- out[, any_values]
        out_diff <- out[, -1] - out[, 1]
        VE <- var(out_diff, use = "pairwise.complete.obs")
        if(length(VE) > 1){
            colnames(VE) <- paste(colnames(out)[-1], colnames(out)[1], sep = "-")
        } else {
            names(VE) <- paste(colnames(out)[-1], colnames(out)[1], sep = "-")
        }
    } else {
        VE <- NA
    }
    VE
}

#' @describeIn util Get vector of which scores are compared
#' @return A vector
get_contrasts <- function(x){
    p <- length(x)
    if(p %in% 0:1){
        return(NULL)
    } else {
        nms <- names(x)
        paste(nms[-1], nms[1], sep = "-")
    }
}

#' @describeIn util Calculate difference in performance measure compared to first score
#' @return A vector
get_perf_diff <- function(x){
    p <- length(x)
    if(p %in% 0:1){
        out <- NULL
    } else {
        nms <- names(x)
        nms <- paste(nms[-1], nms[1], sep = "-")
        out <- x[-1] - x[1]
        names(out) <- nms
    }
    return(out)
}

#' @describeIn util Get design of study
#' @return A matrix
get_design <- function(x){
    p <- length(x)
    if(p %in% 0:1){
        return("")
    } else {
        nms <- names(x)
        paste(nms, collapse = "-")
    }
}

#' @describeIn util Get model matrix
#' @return A matrix
get_mm <- function(x){
    p <- length(x)
    if(p %in% 0:1){
        out <- NULL
    } else {
        nms <- names(x)
        out <- data.frame(score.1 = nms[-1],
                      score.2 = nms[1])
    }
    out
}

