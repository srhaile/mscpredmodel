
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mscpredmodel

The goal of mscpredmodel is to make external validation and comparison
of prediction models via the network meta-analytic approach multiple
score comparison (MSC) straightforward for researchers involved in
systematic reviews and (network) meta-analysis of prediction models.

## Installation

You can install the released version of mscpredmodel from
[github](https://github.com/) with:

``` r
devtools::install_github("srhaile/mscpredmodel")
```

## Example

This is a basic example which shows you a typical analysis for a dataset
having individual patient data for 30 cohorts, each with some
combination of scores a, b, c, e, and g. The example here has only 25
bootstrap samples to speed up the runtime, but an actual analysis should
use more.

``` r
library(mscpredmodel)
library(ggplot2)
theme_set(theme_bw())

dat <- msc_sample_data()
head(dat)
#>     study  id outcome    a    b    c    d    e    f    g  h  i age female
#> 1.1     1   1       1 0.53   NA   NA   NA 0.52 0.54 0.52 NA NA  43      1
#> 1.2     1  10       0 0.41 0.30 0.54 0.33 0.44 0.39 0.43 NA NA  47      0
#> 1.3     1 100       0 0.56   NA 0.68 0.62 0.54 0.57 0.55 NA NA  39      0
#> 1.4     1 101       1 0.44 0.32 0.57 0.38 0.46 0.43 0.45 NA NA  52      1
#> 1.5     1 102       0 0.41   NA   NA 0.32   NA 0.38 0.42 NA NA  51      0
#> 1.6     1 103       0   NA   NA 0.67 0.61 0.54 0.57 0.55 NA NA  47      1
#>        x1
#> 1.1 -0.78
#> 1.2  1.60
#> 1.3  0.73
#> 1.4  2.03
#> 1.5  0.14
#> 1.6  1.19

M <- 25
bs.example <- get_bs_samples(data = dat, id = id, cohort = study, 
                             outcome = outcome, n.samples = M, 
                             scores = c("a", "b", "c", "e", "g"), 
                             moderators = c("age", "female"))
ps <- compute_performance(bs.example, fn = int_calib_index, 
                          lbl = "ICI")
summary(ps)
#>   score nonmiss median    q1   q3
#> 1     a      15  0.104 0.068 0.12
#> 2     b      13  0.140 0.088 0.18
#> 3     c      12  0.097 0.075 0.23
#> 4     e      12  0.102 0.083 0.13
#> 5     g      11  0.093 0.077 0.13

agg <- aggregate_performance(ps, reference = "b")
check_transitivity(agg, graph = TRUE)
#> Warning in qt(a, object$df.residual): NaNs produced

#> Warning in qt(a, object$df.residual): NaNs produced

#> Warning in qt(a, object$df.residual): NaNs produced

#> Warning in qt(a, object$df.residual): NaNs produced

#> Warning in qt(a, object$df.residual): NaNs produced

#> Warning in qt(a, object$df.residual): NaNs produced
#> Warning: The shape palette can deal with a maximum of 6 discrete values
#> because more than 6 becomes difficult to discriminate; you have 7.
#> Consider specifying shapes manually if you must have them.
#> Warning: Removed 72 rows containing missing values (geom_point).
```

<img src="man/figures/README-example-1.png" width="60%" />

    #> # A tibble: 12 x 9
    #>    contr moderator term  estimate std.error statistic   p.value  conf.low
    #>    <fct> <fct>     <chr>    <dbl>     <dbl>     <dbl>     <dbl>     <dbl>
    #>  1 a-b   age       age   -5.94e-3   0.00114     -5.23   2.81e-4  -0.00844
    #>  2 c-b   age       age   -1.17e-2   0.00332     -3.54   7.64e-3  -0.0194 
    #>  3 e-b   age       age   -9.47e-3   0.00329     -2.87   2.07e-2  -0.0171 
    #>  4 g-b   age       age   -6.94e-3   0.00202     -3.43   8.99e-3  -0.0116 
    #>  5 c-a   age       age   -1.58e-2 NaN          NaN    NaN       NaN      
    #>  6 e-a   age       age   -7.20e-4 NaN          NaN    NaN       NaN      
    #>  7 a-b   female    fema… -1.43e-1   0.0394      -3.63   3.98e-3  -0.230  
    #>  8 c-b   female    fema… -4.55e-1   0.0776      -5.86   3.79e-4  -0.633  
    #>  9 e-b   female    fema… -1.70e-1   0.100       -1.70   1.27e-1  -0.401  
    #> 10 g-b   female    fema… -2.62e-1   0.0566      -4.62   1.70e-3  -0.392  
    #> 11 c-a   female    fema…  8.61e-1 NaN          NaN    NaN       NaN      
    #> 12 e-a   female    fema…  3.93e-2 NaN          NaN    NaN       NaN      
    #> # … with 1 more variable: conf.high <dbl>
    
    modc <- consistency(agg)
    modi <- inconsistency(agg)
    modi
    #> 
    #> Multivariate Meta-Analysis Model (k = 48; method: REML)
    #> 
    #> Variance Components:
    #> 
    #> outer factor: cohort (nlvls = 15)
    #> inner factor: contr  (nlvls = 7)
    #> 
    #>             estim    sqrt  fixed 
    #> tau^2      0.0028  0.0525     no 
    #> rho        0.5000            yes 
    #> 
    #> outer factor: design (nlvls = 7)
    #> inner factor: contr  (nlvls = 7)
    #> 
    #>             estim    sqrt  fixed 
    #> gamma^2    0.0000  0.0000     no 
    #> phi        0.5000            yes 
    #> 
    #> Test for Residual Heterogeneity:
    #> QE(df = 44) = 601.6165, p-val < .0001
    #> 
    #> Test of Moderators (coefficients 1:4):
    #> QM(df = 4) = 96.4464, p-val < .0001
    #> 
    #> Model Results:
    #> 
    #>    estimate      se    zval    pval   ci.lb   ci.ub 
    #> a    0.0865  0.0153  5.6532  <.0001  0.0565  0.1165  *** 
    #> c    0.1475  0.0169  8.7127  <.0001  0.1143  0.1807  *** 
    #> e    0.1249  0.0165  7.5500  <.0001  0.0925  0.1573  *** 
    #> g    0.1130  0.0169  6.6781  <.0001  0.0798  0.1462  *** 
    #> 
    #> ---
    #> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    
    check_homogeneity(modi)
    #>     tau2  QE df     QEp
    #> 1 0.0028 602 44 5.3e-99
    check_consistency(ps)

<img src="man/figures/README-example-2.png" width="60%" />

``` r

fullres <- msc_full(ps)
plot(fullres, compare_to = "c")
```

<img src="man/figures/README-example-3.png" width="60%" />
