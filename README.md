
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
#> Loading required package: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
#> Loading required package: forcats
#> Loading required package: purrr
#> Loading required package: rsample
#> Loading required package: tidyr
#> Loading required package: tibble
dat <- msc_sample_data()
dat
#> # A tibble: 17,765 x 12
#>    cohort    id outcome     a     b      c     d      e     f      g     h
#>     <int> <int>   <dbl> <dbl> <dbl>  <dbl> <dbl>  <dbl> <dbl>  <dbl> <dbl>
#>  1      1     1       0 0.186    NA NA        NA  0.272    NA  0.226    NA
#>  2      1     2       0 0.298    NA  0.412    NA NA        NA  0.329    NA
#>  3      1     3       0 0.119    NA  0.182    NA  0.208    NA  0.158    NA
#>  4      1     4       0 0.163    NA NA        NA NA        NA  0.203    NA
#>  5      1     5       0 0.184    NA  0.272    NA NA        NA  0.225    NA
#>  6      1     6       0 0.132    NA  0.200    NA  0.221    NA  0.172    NA
#>  7      1     7       0 0.196    NA NA        NA  0.280    NA NA        NA
#>  8      1     8       0 0.245    NA  0.349    NA  0.321    NA  0.281    NA
#>  9      1     9       0 0.120    NA NA        NA  0.209    NA  0.159    NA
#> 10      1    10       0 0.211    NA NA        NA  0.293    NA  0.250    NA
#> # … with 17,755 more rows, and 1 more variable: i <dbl>
M <- 100
bs.example <- get_bs_samples(data = dat, id = id, cohorts = cohort, 
                             outcome = outcome, n.samples = M, 
                             a, b, c, e, g)
ps <- compute_performance(bs.example, fn = calibration_large, lbl = "calibration-in-the-large")
agg <- aggregate_performance(ps, reference = "a", design.levels = c("a", "b", "c", "e", "g"))
# modc <- consistency(agg)
# modi <- inconsistency(agg)
# modi
```
