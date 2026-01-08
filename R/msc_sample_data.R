#' Make MSC sample data set
#'
#' Creates a sample data set to test out the \code{\link{mscpredmodel}} package.
#' As the dataset is randomly generated, and it may be desired to make other
#' sample datasets (for example, with more missing observations, or different
#' characteristics), the code given here could be adapted to other situations.
#'
#' @importFrom stats model.matrix runif rnorm plogis rbinom runif rexp
#'
#' @param n.cohorts Number of cohorts in the data set (default 30)
#'
#' @return A sample data set (data.frame)
#' @export
#'
#' @examples
#' dat <- msc_sample_data(n.cohorts = 30)
msc_sample_data <- function(seed = NULL){
    n.cohorts <- 5
    set.seed(seed)
    
    sample_data <- data.frame(n = round(runif(n.cohorts, 100, 1000)),
                              mean.age = round(runif(n.cohorts, 50, 70)),
                              sd.age = round(runif(n.cohorts, 5, 15)),
                              pct.female = round(runif(n.cohorts), 2),
                              mean.x1 = round(rnorm(n.cohorts), 1),
                              sd.x1 = round(rexp(n.cohorts, 1), 2),
                              mean.x2 = round(rnorm(n.cohorts), 1),
                              sd.x2 = round(rexp(n.cohorts, 1), 2),
                              mean.z = round(rnorm(n.cohorts), 1),
                              sd.z = round(rexp(n.cohorts, 1), 2)) 
    
    betas <- rnorm(6)
    
    gen_mods <- function(i){
        this_cohort <- with(sample_data, 
                            data.frame(cohort = i,
                                       age = rnorm(n[i], mean.age[i], sd.age[i]),
                                       sex = rbinom(n[i], 1, pct.female[i]),
                                       x1 = rnorm(n[i], mean.x1[i], sd.x1[i]),
                                       x2 = rnorm(n[i], mean.x2[i], sd.x2[i]),
                                       z = rnorm(n[i], mean.z[i], sd.z[i])))
        
        
        mm <- as.matrix(this_cohort[, -1])
        mm[, "age"] <- mm[, "age"] - 55
        mm <- cbind(1, mm)
        pred <- betas %*% t(mm) + rnorm(sample_data$n[i], 0, 3)
        this_cohort$y <- as.numeric(plogis(pred) > 0.5)
        
        this_cohort$sex <- factor(this_cohort$sex, 0:1, c("M", "F"))
        return(this_cohort)
    }
    
    spl <- lapply(1:n.cohorts, FUN = gen_mods)
    
    mod1 <- glm(y ~ age + sex, data = spl[[1]], 
                family = binomial)
    mod2 <- glm(y ~ age + x1, data = spl[[2]], 
                family = binomial)
    mod3 <- glm(y ~ sex + x2, data = spl[[3]], 
                family = binomial)
    mod4 <- glm(y ~ age + x2, data = spl[[4]], 
                family = binomial)
    mod5 <- glm(y ~ sex + age + x1 + x2, data = spl[[5]], 
                family = binomial)
    
    spl[[1]]$x1 <- round(spl[[1]]$x1, 1)
    spl[[2]]$x2 <- round(spl[[2]]$x2, 1)
    spl[[3]]$x1 <- round(spl[[3]]$x1, 1)
    spl[[3]]$x2 <- round(spl[[3]]$x2, 1)
    spl[[4]]$x1 <- round(spl[[4]]$x1, 2)
    spl[[4]]$x2 <- round(spl[[4]]$x2, 1)
    spl[[5]]$x1 <- round(spl[[5]]$x1, 3)
    spl[[5]]$x2 <- round(spl[[5]]$x2, 3)
    spl <- lapply(spl, function(x){x[, -6]}) # remove z 
    spl <- lapply(spl, function(x){
        newpred <- predict(mod1, newdata = x, type = "link")
        x$pred1 <- newpred
        x
    }) 
    spl <- lapply(spl, function(x){
        newpred <- predict(mod2, newdata = x, type = "response")
        x$pred2 <- newpred
        x
    }) 
    spl <- lapply(spl, function(x){
        newpred <- predict(mod3, newdata = x, type = "link")
        x$pred3 <- newpred
        x
    }) 
    spl <- lapply(spl, function(x){
        newpred <- predict(mod4, newdata = x, type = "link")
        x$pred4 <- newpred
        x
    }) 
    spl <- lapply(spl, function(x){
        newpred <- predict(mod5, newdata = x, type = "response")
        x$pred5 <- newpred
        x
    }) 

    # structural missings
    spl[[1]]$pred3 <- NA
    spl[[2]]$pred5 <- NA
    spl[[4]]$pred4 <- NA
    
  # ... and some random missings
 randmiss <- function(d, s, p = 0.2){
    make_missing <- rbinom(nrow(d), 1, prob = p)
     d[make_missing == 1, s] <- NA 
     d
 }
 
 datmiss <- lapply(spl, randmiss, s = "pred1", p = 0.05)
 datmiss <- lapply(datmiss, randmiss, s = "pred2", p = 0.1)
 datmiss <- lapply(datmiss, randmiss, s = "pred3", p = 0.2)
 datmiss <- lapply(datmiss, randmiss, s = "pred4", p = 0.3)
 datmiss <- lapply(datmiss, randmiss, s = "pred5", p = 0.2)
 
 sample_data <- do.call(rbind, datmiss)
 names(sample_data)[1] <- "study"
 sample_data
}


