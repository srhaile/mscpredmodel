#' Get performance statistics
#'
#' @param scores A vector of scores (i.e. variables in the dataset corresponding to calculated scores). The first score in the vector will be used as a comparator for all other scores.
#' @param cohort The name of the variable corresponding to cohort or study in the dataset.
#' @param outcome The name of the variable corresponding to the outcome.
#' @param fn A named list of performance metrics.
#' @param data  Name of dataset containing all other variables, with individual patient data, that is, 1 row per subject.
#'
#' @returns
#' @export
#'
#' @examples
get_performance_statistics <- function(scores,
                                       cohort,
                                       outcome,         
                                       data,
                                       fn = list("AUC" = c_statistic,
                                                 "O/E" = oe_ratio)){
    
    if(!cohort %in% names(data)){
        stop("Variable specifying study cohorts (", cohort, ") is not in dataset.")
    }
    
    if(!outcome %in% names(data)){
        stop("The specified outcome variable (", outcome, ") is not in dataset.")
    }
    
    this_dat <- data
    
    if(is.null(scores)){
        stop("No scores provided to compare.")
    } else if(!is.null(scores)){
        missing_scores <- scores[!scores %in% names(data)]
    }
    if(length(missing_scores) == length(scores)){
        stop("Scores provided are not in data.")
    } else if(length(missing_scores) < length(scores) & length(missing_scores) != 0){
        scores <- scores[!scores %in% missing_scores]
        message("Some scores are not in data. Removing: ", paste(missing_scores, collapse = ", "), 
                ". New scores are: ", paste(ss, collapse = ", "), ".")
    }
    k <- length(scores)
    g <- this_dat[, cohort]
    spl <- split(as.data.frame(this_dat), g)
    
    f <- length(fn)
    out <- vector("list", f)
    
    for(ff in 1:f){
        this_out <- matrix(NA, nrow = length(spl), ncol = length(scores))
        colnames(this_out) <- scores
        rownames(this_out) <- names(spl)
        this_fn <- fn[[ff]]
        
        for(j in 1:length(spl)){
            this_spl <- spl[[j]]
            for(i in 1:k){
                this_score <- scores[i]
                obs <- this_spl[, outcome]
                pred <- as.vector(this_spl[, this_score])
                if(all(is.na(pred)) | all(is.na(obs))){
                    this_out[j, i] <- NA
                } else {
                    this_out[j, i] <- this_fn(obs, pred)
                }
                
            }
            
        }
        
        out[[ff]] <- this_out
    }
    
    names(out) <- names(fn)
    class(out) <- "perfsumm"
    
    return(out)
    
}

#' @describeIn get_performance_statistics  Compute summary statistics
#' @export
basic_summ <- function(x, dig = 2){
    fmt <- function(x) format(x, nsmall = dig, digits = dig)
    q2 <- median(x, na.rm = TRUE)
    q1 <- as.numeric(quantile(x, prob = 0.25, na.rm = TRUE))
    q3 <- as.numeric(quantile(x, prob = 0.75, na.rm = TRUE))
    paste(fmt(q2), " [", fmt(q1), ", ", fmt(q3), "]", sep = "")
}

#' @describeIn get_performance_statistics  Summarize performance statistics
#' @export
summary.perfsumm <- function(x, fun = basic_summ, ...){
    n <- sapply(x, function(s) apply(s, 2, function(sc){sum(!is.na(sc))} ))
    n <- n[, 1]
    x2 <- lapply(x, function(s) apply(s, 2, fun, ...))
    out <- t(do.call("rbind", x2))
    out <- cbind(n, out)
    return(out)
}