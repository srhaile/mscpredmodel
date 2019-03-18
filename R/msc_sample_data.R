#' Make MSC sample data set
#'
#' Creates a sample data set to test out the \code{\link{mscpredmodel}} package.
#' As the dataset is randomly generated, and it may be desired to make other
#' sample datasets (for example, with more missing observations, or different
# characteristics), the code given here could be adapted to other situations.
#'
#' @param n.cohorts Number of cohorts in the data set (default 30)
#'
#' @return A sample data set (tibble)
#' @export
#'
#' @examples
#' dat <- msc_sample_data(n.cohorts = 30)
msc_sample_data <- function(n.cohorts = 30){
  set.seed(20190218)

  scores_setup <- tibble(score = letters[1:9],
                         intercept = c(0, 0.5, -0.5, 0, 0, 0, 0, 0.5, -0.5),
                         slope = c(1, 1, 1, 0.5, 1.5, 0.8, 1.2, 0.5, 1.5))

  make_data <- function(n, p, p.width = 0.1){
    p.min <- max(c(0.001, p - p.width))
    p.max <-  min(c(p + p.width, 0.999))
    d <- tibble(id = 1:n,
                p.true = runif(n, p.min, p.max),
                uncertainty = rnorm(n, mean = 0, sd = 0.75)) %>%
      crossing("score" = scores_setup$score) %>%
      full_join(scores_setup, by = "score") %>%
      mutate(logit.p.true = qlogis(p.true)) %>%
      mutate(logit.p.pred = (logit.p.true - intercept) / slope,
             logit.p.outcome = logit.p.true + uncertainty) %>%
      mutate(outcome = as.numeric(logit.p.outcome > 0),
             p.pred = plogis(logit.p.pred)) %>%
      select(id, outcome, score, p.pred) %>%
      spread(score, p.pred)
    d
  }
  
  sim_moderators <- function(n, mean.age, sd.age, pct.female, 
                             mean.x1, sd.x1){
      d <- tibble(id = 1:n,
                  age = rnorm(n, mean.age, sd.age), 
                  female = rbinom(n, 1, pct.female),
                  x1 = rnorm(n, mean.x1, sd.x1))
      d
  }
  

  sample_data <- tibble(cohort = 1:n.cohorts,
                        n.subjects = round(runif(n.cohorts, 100, 1000)),
                        p.outcome = runif(n.cohorts, 0.2, 0.6),
                        mean.age = round(runif(n.cohorts, 40, 60)),
                        sd.age = round(runif(n.cohorts, 5, 15)),
                        pct.female = round(runif(n.cohorts), 2),
                        mean.x1 = round(rnorm(n.cohorts), 1),
                        sd.x1 = round(rexp(n.cohorts, 1), 2)) %>%
    mutate(dat = map2(n.subjects, p.outcome, make_data),
           datm = pmap(list(n.subjects, mean.age, sd.age,
                pct.female, mean.x1, sd.x1), sim_moderators)) %>%
    unnest() %>%
    select(-p.outcome, -n.subjects, -mean.age, -sd.age, -pct.female,
           -mean.x1, -sd.x1, -id1)

  # add in structural missing (by cohort)

  sample_data <- sample_data %>%
      mutate(b = ifelse(cohort %% 2 == 0, b, NA),
             c = ifelse(cohort %% 3 == 0, NA, c),
             d = ifelse(cohort %% 4 == 0, d, NA),
             e = ifelse(cohort %% 5 == 0, NA, e),
             f = ifelse(cohort %% 6 == 0, f, NA),
             g = ifelse(cohort %% 7 == 0, NA, g),
             h = ifelse(cohort %% 8 == 0, h, NA),
             i = ifelse(cohort %% 9 == 0, NA, i))

  # ... and some random missings
  random_missing <- function(x, p = 0.2){
      is.missing <- rbinom(length(x), 1, p = p)
      ifelse(is.missing == 1, NA, x)
  }
  msc_sample_data  <- sample_data %>%
      mutate(b = random_missing(b, 0.1),
             c = random_missing(c, 0.2),
             d = random_missing(d, 0.3),
             e = random_missing(e, 0.4),
             f = random_missing(f, 0.3),
             g = random_missing(g, 0.2),
             h = random_missing(h, 0.1),
             i = random_missing(i, 0.3))

  msc_sample_data %>% 
      rename(study = cohort)
}

