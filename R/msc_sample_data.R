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
msc_sample_data <- function(n.cohorts = 5, seed = NULL){
  set.seed(seed)

  scores_setup <- data.frame(score = letters[1:9],
                         intercept = c(0, 0.5, -0.5, 0, 0, 0, 0, 0.5, -0.5),
                         slope = c(1, 1, 1, 0.5, 1.5, 0.8, 1.2, 0.5, 1.5))

  make_data <- function(n, p, p.width = 0.1){
    p.min <- max(c(0.001, p - p.width))
    p.max <-  min(c(p + p.width, 0.999))
    ids <- data.frame(id = 1:n,
                p.true = runif(n, p.min, p.max),
                uncertainty = rnorm(n, mean = 0, sd = 0.75)) 
    id_score <- expand.grid("id" = ids$id, "score" = scores_setup$score)
    d <- merge(ids, id_score, by = "id", all = TRUE, sort = TRUE)
    d <- merge(d, scores_setup, by = "score", all = TRUE, sort = TRUE)
    
    d$logit.p.true <- qlogis(d$p.true)
    d$logit.p.pred <- (d$logit.p.true - d$intercept) / d$slope
    d$logit.p.outcome <- d$logit.p.true + d$uncertainty
    d$outcome <- as.numeric(d$logit.p.outcome > 0)
    d$p.pred <- plogis(d$logit.p.pred)
    d <- d[, c("id", "outcome", "score", "p.pred")]
    dwide <- reshape(d, direction = "wide",
            idvar = c("id", "outcome"),
            timevar = "score")
    names(dwide) <- c("id", "outcome", as.character(scores_setup$score))
    rownames(dwide) <- NULL
    ord <- order(dwide$id)
    dwide[ord, ]
  }
  sim_moderators <- function(n, mean.age, sd.age, pct.female, 
                             mean.x1, sd.x1){
      d <- data.frame(id = 1:n,
                  age = rnorm(n, mean.age, sd.age), 
                  female = rbinom(n, 1, pct.female),
                  x1 = rnorm(n, mean.x1, sd.x1))
      rownames(d) <- NULL
      d
  }
  

  ## TO DO...
  sample_data <- data.frame(cohort = sample(1:n.cohorts),
                        n = round(runif(n.cohorts, 100, 1000)),
                        p.outcome = runif(n.cohorts, 0.2, 0.6),
                        mean.age = round(runif(n.cohorts, 40, 60)),
                        sd.age = round(runif(n.cohorts, 5, 15)),
                        pct.female = round(runif(n.cohorts), 2),
                        mean.x1 = round(rnorm(n.cohorts), 1),
                        sd.x1 = round(rexp(n.cohorts, 1), 2)) 
  
  dat <- mapply(make_data, sample_data$n, sample_data$p.outcome, SIMPLIFY = FALSE)
  datm <- mapply(sim_moderators, sample_data$n, sample_data$mean.age, 
                 sample_data$sd.age, sample_data$pct.female, 
                 sample_data$mean.x1, sample_data$sd.x1, SIMPLIFY = FALSE)
  names(dat) <- names(datm) <- sample_data$cohort
  add_cohort <- function(x, nms){
      varnames <- names(x)
      x$cohort <- nms
      x <- x[, c("cohort", varnames)]
      x
  }
  dat <- mapply(add_cohort, dat, names(dat), SIMPLIFY = FALSE)
  dat <- do.call(rbind, dat)
  datm <- mapply(add_cohort, datm, names(datm), SIMPLIFY = FALSE)
  datm <- do.call(rbind, datm)
  dat2 <- merge(dat, datm, by = c("cohort", "id"))
  sample_data <- split(dat2, dat2$cohort)
  #sample_data <- mapply(add_cohort, sample_data, names(sample_data), SIMPLIFY = FALSE)

  # add in structural missing (by cohort)
 lvls <- names(sample_data)
 b.missing <- sample(lvls, round(n.cohorts * 0.2))
 c.missing <- sample(lvls, round(n.cohorts * 0.2))
 d.missing <- sample(lvls, round(n.cohorts * 0.2))
 e.missing <- sample(lvls, round(n.cohorts * 0.4))
 f.missing <- sample(lvls, round(n.cohorts * 0.4))
 g.missing <- sample(lvls, round(n.cohorts * 0.4))
 h.missing <- sample(lvls, round(n.cohorts * 0.6))
 i.missing <- sample(lvls, round(n.cohorts * 0.6))
 
 strucmiss <- function(d, s, coh){
     if(d$cohort[1] %in% coh){
         d[, s] <- NA
     } 
     d
 }
 
 datmiss <- lapply(sample_data, strucmiss, s = "b", coh = b.missing)
 datmiss <- lapply(datmiss, strucmiss, s = "c", coh = c.missing)
 datmiss <- lapply(datmiss, strucmiss, s = "d", coh = d.missing)
 datmiss <- lapply(datmiss, strucmiss, s = "e", coh = e.missing)
 datmiss <- lapply(datmiss, strucmiss, s = "f", coh = f.missing)
 datmiss <- lapply(datmiss, strucmiss, s = "g", coh = g.missing)
 datmiss <- lapply(datmiss, strucmiss, s = "h", coh = h.missing)
 datmiss <- lapply(datmiss, strucmiss, s = "i", coh = i.missing)

  # ... and some random missings
 randmiss <- function(d, s, p = 0.2){
    make_missing <- rbinom(nrow(d), 1, prob = p)
     d[make_missing == 1, s] <- NA 
     d
 }
 
 datmiss <- lapply(datmiss, randmiss, s = "a", p = 0.05)
 datmiss <- lapply(datmiss, randmiss, s = "b", p = 0.1)
 datmiss <- lapply(datmiss, randmiss, s = "c", p = 0.2)
 datmiss <- lapply(datmiss, randmiss, s = "d", p = 0.3)
 datmiss <- lapply(datmiss, randmiss, s = "e", p = 0.2)
 datmiss <- lapply(datmiss, randmiss, s = "f", p = 0.1)
 datmiss <- lapply(datmiss, randmiss, s = "g", p = 0.2)
 datmiss <- lapply(datmiss, randmiss, s = "h", p = 0.1)
 datmiss <- lapply(datmiss, randmiss, s = "i", p = 0.3)
 
 sample_data <- do.call(rbind, datmiss)
 names(sample_data)[1] <- "study"
 sample_data$sex <- factor(sample_data$female, 0:1, c("male", "female"))
 sample_data
}


