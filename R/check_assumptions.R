dat <- msc_sample_data()
bssamp <- get_bs_samples(dat, id, cohort, outcome, n.samples = 10, a, b, c, d, e)
perf <- compute_performance(bssamp, fn = calibration_slope, lbl = "CS")
agg <- aggregate_performance(perf)

moderators <- dat %>%
    group_by(cohort) %>%
    summarize(age = mean(age, na.rm = TRUE),
              female = mean(female, na.rm = TRUE),
              x1 = mean(x1, na.rm = TRUE)) %>%
    rename(cohorts = cohort)

wt <- solve(agg$vi)

aggm <- with(agg, tibble(cohorts, yi, 
                         vi = diag(vi), wt = diag(wt), contr)) %>%
    left_join(moderators %>% mutate(cohorts = factor(cohorts)), 
              by = "cohorts") %>%
    arrange(contr, cohorts)

# aggmg <- aggm %>% 
#     group_by(contr) %>% 
#     nest()

transitivity_model <- function(contr, moderator){
    this.fm <- paste("yi ~", moderator)
    this.lm <- lm(as.formula(this.fm), data = aggm, weights = wt)
    this.lm
}



x <- c("age", "female", "x1")
res <- crossing(contr = unique(aggm$contr),
                moderator = x) %>%
    mutate(fit = map2(contr, moderator, transitivity_model))
