
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

This is a basic example which shows you a typical analysis for a dataset having individual patient data for 30 cohorts, each with some combination of scores a, b, c, e, and g. The example here has only 25 bootstrap samples to speed up the runtime, but an actual analysis should use more.

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
M <- 25
bs.example <- get_bs_samples(data = dat, id = id, cohorts = cohort, 
                             outcome = outcome, n.samples = M, 
                             a, b, c, e, g)
ps <- compute_performance(bs.example, fn = calibration_large, lbl = "calibration-in-the-large")
agg <- aggregate_performance(ps, reference = "b")
modc <- consistency(agg)
modi <- inconsistency(agg)
modi
#> 
#> Multivariate Meta-Analysis Model (k = 85; method: REML)
#> 
#> Variance Components: 
#> 
#> outer factor: cohorts (nlvls = 30)
#> inner factor: contr   (nlvls = 4)
#> 
#>             estim    sqrt  fixed
#> tau^2      0.0073  0.0853     no
#> rho        0.5000            yes
#> 
#> outer factor: design (nlvls = 11)
#> inner factor: contr  (nlvls = 4)
#> 
#>             estim    sqrt  fixed
#> gamma^2    0.0000  0.0000     no
#> phi        0.5000            yes
#> 
#> Test for Residual Heterogeneity: 
#> QE(df = 81) = 274.8546, p-val < .0001
#> 
#> Test of Moderators (coefficient(s) 1:4): 
#> QM(df = 4) = 1358.7369, p-val < .0001
#> 
#> Model Results:
#> 
#>    estimate      se      zval    pval    ci.lb    ci.ub     
#> a   -0.5058  0.0228  -22.1706  <.0001  -0.5505  -0.4611  ***
#> c   -1.0120  0.0275  -36.7345  <.0001  -1.0660  -0.9580  ***
#> e   -0.5876  0.0297  -19.8069  <.0001  -0.6458  -0.5295  ***
#> g   -0.5728  0.0262  -21.8420  <.0001  -0.6242  -0.5214  ***
#> 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

fullres <- msc_full(ps)
#> ....................
plot(fullres, compare_to = "b")
#> Loading required package: ggplot2
#> Warning: Removed 1 rows containing missing values (geom_point).
#> Warning: Removed 1 rows containing missing values (geom_linerange).
```

<img src="man/figures/README-example-1.png" width="100%" />
