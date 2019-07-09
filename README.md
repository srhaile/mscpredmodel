
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
library(ggplot2)
theme_set(theme_bw())

dat <- msc_sample_data()
head(dat)
#>       study  id outcome    a  b  c  d    e    f    g  h  i age female    x1
#> 1.249     1   1       0   NA NA NA NA   NA 0.66 0.61 NA NA  54      1 -1.37
#> 1.250     1  10       1 0.58 NA NA NA 0.55 0.60 0.57 NA NA  66      1 -1.60
#> 1.251     1 100       0 0.55 NA NA NA 0.53 0.57 0.54 NA NA  77      1 -0.44
#> 1.252     1 101       0 0.63 NA NA NA   NA 0.66 0.61 NA NA  38      1 -0.38
#> 1.253     1 102       1 0.66 NA NA NA   NA 0.70   NA NA NA  72      1 -0.89
#> 1.254     1 103       1 0.68 NA NA NA 0.62 0.72 0.65 NA NA  65      1 -1.51

M <- 25
bs.example <- get_bs_samples(data = dat, id = id, cohort = study, 
                             outcome = outcome, n.samples = M, 
                             scores = c("a", "b", "c", "e", "g"), 
                             moderators = c("age", "female"))
ps <- compute_performance(bs.example, fn = oe_ratio, 
                          lbl = "O:E ratio")
summary(ps)
#>   score nonmiss median   q1   q3
#> 1     a      15   1.06 0.38 1.10
#> 2     b      13   1.37 0.63 1.40
#> 3     c      12   0.75 0.29 0.89
#> 4     e      12   0.98 0.36 1.15
#> 5     g      11   1.07 0.50 1.12

agg <- aggregate_performance(ps, reference = "b")
check_transitivity(agg, graph = TRUE)
#> Warning in qt(a, object$df.residual): NaNs produced

#> Warning in qt(a, object$df.residual): NaNs produced

#> Warning in qt(a, object$df.residual): NaNs produced

#> Warning in qt(a, object$df.residual): NaNs produced
```

<img src="man/figures/README-example-1.png" width="60%" />

    #> # A tibble: 10 x 9
    #>    contr moderator term   estimate std.error statistic  p.value  conf.low conf.high
    #>    <fct> <fct>     <chr>     <dbl>     <dbl>     <dbl>    <dbl>     <dbl>     <dbl>
    #>  1 g-a   age       age      0.0514  NaN        NaN     NaN      NaN        NaN     
    #>  2 a-b   age       age      0.0621    0.0274     2.27    0.0445   0.00181    0.122 
    #>  3 c-b   age       age      0.0209    0.0197     1.06    0.313   -0.0229     0.0647
    #>  4 e-b   age       age      0.0930    0.0398     2.34    0.0442   0.00299    0.183 
    #>  5 g-b   age       age      0.0382    0.0425     0.898   0.399   -0.0624     0.139 
    #>  6 g-a   female    female   2.40    NaN        NaN     NaN      NaN        NaN     
    #>  7 a-b   female    female  -0.460     1.02      -0.450   0.661   -2.71       1.79  
    #>  8 c-b   female    female   0.521     0.563      0.925   0.377   -0.734      1.78  
    #>  9 e-b   female    female  -0.930     1.12      -0.833   0.427   -3.46       1.60  
    #> 10 g-b   female    female  -0.559     1.00      -0.556   0.596   -2.93       1.82

    modc <- consistency(agg)
    modi <- inconsistency(agg)
    modi
    #> 
    #> Multivariate Meta-Analysis Model (k = 48; method: REML)
    #> 
    #> Variance Components: 
    #> 
    #> outer factor: cohort (nlvls = 15)
    #> inner factor: contr  (nlvls = 6)
    #> 
    #>             estim    sqrt  fixed
    #> tau^2      0.0797  0.2823     no
    #> rho        0.5000            yes
    #> 
    #> outer factor: design (nlvls = 6)
    #> inner factor: contr  (nlvls = 6)
    #> 
    #>             estim    sqrt  fixed
    #> gamma^2    0.0430  0.2074     no
    #> phi        0.5000            yes
    #> 
    #> Test for Residual Heterogeneity: 
    #> QE(df = 44) = 2509.9110, p-val < .0001
    #> 
    #> Test of Moderators (coefficient(s) 1:4): 
    #> QM(df = 4) = 59.4103, p-val < .0001
    #> 
    #> Model Results:
    #> 
    #>    estimate      se    zval    pval   ci.lb   ci.ub     
    #> a    0.7692  0.1327  5.7952  <.0001  0.5091  1.0294  ***
    #> c    0.6692  0.1445  4.6295  <.0001  0.3859  0.9524  ***
    #> e    0.9207  0.1440  6.3922  <.0001  0.6384  1.2031  ***
    #> g    0.9635  0.1450  6.6455  <.0001  0.6793  1.2476  ***
    #> 
    #> ---
    #> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    check_homogeneity(modi)
    #>   tau2   QE df QEp
    #> 1 0.08 2510 44   0
    check_consistency(ps)
    #> Warning: Removed 5 rows containing missing values (geom_linerange).
    #> Warning: Removed 5 rows containing missing values (geom_point).

<img src="man/figures/README-example-2.png" width="60%" />

``` r

fullres <- msc_full(ps)
plot(fullres, compare_to = "c")
#> Warning: Removed 2 rows containing missing values (geom_linerange).
#> Warning: Removed 2 rows containing missing values (geom_point).
```

<img src="man/figures/README-example-3.png" width="60%" />
