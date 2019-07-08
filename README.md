
<!-- README.md is generated from README.Rmd. Please edit that file -->
mscpredmodel
============

The goal of mscpredmodel is to make external validation and comparison of prediction models via the network meta-analytic approach multiple score comparison (MSC) straightforward for researchers involved in systematic reviews and (network) meta-analysis of prediction models.

Installation
------------

You can install the released version of mscpredmodel from [github](https://github.com/) with:

``` r
devtools::install_github("srhaile/mscpredmodel")
```

Example
-------

This is a basic example which shows you a typical analysis for a dataset having individual patient data for 30 cohorts, each with some combination of scores a, b, c, e, and g. The example here has only 100 bootstrap samples to speed up the runtime, but an actual analysis should use more.

``` r
library(mscpredmodel)
library(ggplot2)
theme_set(theme_bw())
dat <- msc_sample_data()
head(dat)
#> # A tibble: 6 x 15
#>   study    id outcome     a     b      c     d      e     f     g     h
#>   <int> <int>   <dbl> <dbl> <dbl>  <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>
#> 1     1     1       0 0.245    NA  0.348    NA  0.320    NA 0.281    NA
#> 2     1     2       0 0.300    NA  0.414    NA  0.362    NA 0.331    NA
#> 3     1     3       0 0.317    NA  0.433    NA NA        NA 0.345    NA
#> 4     1     4       0 0.269    NA  0.377    NA NA        NA 0.303    NA
#> 5     1     5       1 0.354    NA  0.475    NA NA        NA 0.377    NA
#> 6     1     6       0 0.359    NA NA        NA NA        NA 0.382    NA
#> # … with 4 more variables: i <dbl>, age <dbl>, female <int>, x1 <dbl>
M <- 100
bs.example <- get_bs_samples(data = dat, id = id, cohort = study, 
                             outcome = outcome, n.samples = M, 
                             scores = c("a", "b", "c", "e", "g"), 
                             moderators = c("age", "female"))
ps <- compute_performance(bs.example, fn = calibration_large, lbl = "calibration-in-the-large")
summary(ps)
#> # A tibble: 5 x 6
#>   score performance              nonmiss  median     q1     q3
#>   <chr> <chr>                      <int>   <dbl>  <dbl>  <dbl>
#> 1 a     calibration-in-the-large      15 -0.420  -0.746  0.115
#> 2 b     calibration-in-the-large       7 -0.0887 -0.362  0.717
#> 3 c     calibration-in-the-large      10 -1.01   -1.36  -0.388
#> 4 e     calibration-in-the-large      12 -0.604  -0.820  0.223
#> 5 g     calibration-in-the-large      13 -0.540  -0.859  0.180

agg <- aggregate_performance(ps, reference = "b")
check_transitivity(agg, graph = TRUE)
```

<img src="man/figures/README-example-1.png" width="60%" />

    #> # A tibble: 8 x 8
    #>   contr moderator  estimate std.error statistic p.value  conf.low conf.high
    #>   <chr> <chr>         <dbl>     <dbl>     <dbl>   <dbl>     <dbl>     <dbl>
    #> 1 b-a   age        0.00429    0.00169     2.54   0.0521  -5.75e-5   0.00864
    #> 2 c-a   age       -0.000713   0.00190    -0.375  0.718   -5.10e-3   0.00368
    #> 3 e-a   age        0.00945    0.00587     1.61   0.139   -3.63e-3   0.0225 
    #> 4 g-a   age        0.00204    0.00342     0.596  0.563   -5.49e-3   0.00957
    #> 5 b-a   female     0.0454     0.0556      0.817  0.451   -9.75e-2   0.188  
    #> 6 c-a   female    -0.0950     0.0505     -1.88   0.0971  -2.12e-1   0.0216 
    #> 7 e-a   female     0.267      0.128       2.09   0.0632  -1.77e-2   0.551  
    #> 8 g-a   female     0.121      0.0667      1.82   0.0961  -2.54e-2   0.268

    modc <- consistency(agg)
    modi <- inconsistency(agg)
    modi
    #> 
    #> Multivariate Meta-Analysis Model (k = 42; method: REML)
    #> 
    #> Variance Components: 
    #> 
    #> outer factor: cohort (nlvls = 15)
    #> inner factor: contr  (nlvls = 4)
    #> 
    #>             estim    sqrt  fixed
    #> tau^2      0.0028  0.0533     no
    #> rho        0.5000            yes
    #> 
    #> outer factor: design (nlvls = 9)
    #> inner factor: contr  (nlvls = 4)
    #> 
    #>             estim    sqrt  fixed
    #> gamma^2    0.0011  0.0333     no
    #> phi        0.5000            yes
    #> 
    #> Test for Residual Heterogeneity: 
    #> QE(df = 38) = 88.4727, p-val < .0001
    #> 
    #> Test of Moderators (coefficient(s) 1:4): 
    #> QM(df = 4) = 743.8152, p-val < .0001
    #> 
    #> Model Results:
    #> 
    #>    estimate      se      zval    pval    ci.lb    ci.ub     
    #> a   -0.4571  0.0308  -14.8198  <.0001  -0.5175  -0.3966  ***
    #> c   -0.9796  0.0368  -26.6531  <.0001  -1.0517  -0.9076  ***
    #> e   -0.5556  0.0393  -14.1273  <.0001  -0.6327  -0.4785  ***
    #> g   -0.5373  0.0345  -15.5757  <.0001  -0.6049  -0.4697  ***
    #> 
    #> ---
    #> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    check_homogeneity(modi)
    #>      tau2         Q        df   p-value 
    #>   "0.003"  "88.473"      "38" "<0.0001"
    check_consistency(ps)
    #> Warning: Removed 4 rows containing missing values (geom_point).
    #> Warning: Removed 4 rows containing missing values (geom_linerange).

<img src="man/figures/README-example-2.png" width="60%" />

``` r

fullres <- msc_full(ps)
plot(fullres, compare_to = "c")
#> Warning: Removed 1 rows containing missing values (geom_point).
#> Warning: Removed 1 rows containing missing values (geom_linerange).
```

<img src="man/figures/README-example-3.png" width="60%" />
