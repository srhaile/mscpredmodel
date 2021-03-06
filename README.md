
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
#>   score nonmiss median   q1   q3 performance
#> 1     a      15   1.06 0.38 1.10   O:E ratio
#> 2     b      13   1.37 0.63 1.40   O:E ratio
#> 3     c      12   0.75 0.29 0.89   O:E ratio
#> 4     e      12   0.98 0.36 1.15   O:E ratio
#> 5     g      11   1.07 0.50 1.12   O:E ratio
check_transitivity(ps, graph = TRUE)
```

<img src="man/figures/README-example-1.png" width="60%" />

    #>    contr moderator   term estimate std.error statistic p.value conf.low conf.high
    #> 2    e-a       age    age  5.7e-03    0.0022    2.5758   0.028  0.00077    0.0106
    #> 6    g-a       age    age  3.5e-03    0.0014    2.4761   0.035  0.00030    0.0067
    #> 10   b-a       age    age  2.2e-03    0.0037    0.5810   0.573 -0.00600    0.0103
    #> 14   c-a       age    age  1.8e-05    0.0029    0.0061   0.995 -0.00653    0.0066
    #> 4    e-a    female female  8.3e-02    0.0857    0.9678   0.356 -0.10799    0.2738
    #> 8    g-a    female female  8.2e-02    0.0423    1.9453   0.084 -0.01339    0.1778
    #> 12   b-a    female female -2.2e-02    0.1161   -0.1863   0.856 -0.27717    0.2339
    #> 16   c-a    female female -1.1e-01    0.0951   -1.1673   0.270 -0.32292    0.1009

    msc_model(ps, mtype = "consistency")
    #>   s1 s2       model  type estimate  ci.lb  ci.ub    pval   measure mods
    #> b  a  b consistency model   0.2851  0.247  0.323 3.8e-49 O:E ratio     
    #> c  a  c consistency model  -0.1836 -0.221 -0.146 8.7e-22 O:E ratio     
    #> e  a  e consistency model  -0.0076 -0.045  0.030 6.9e-01 O:E ratio     
    #> g  a  g consistency model  -0.0024 -0.041  0.036 9.0e-01 O:E ratio
    (modi <- msc_model(ps, mtype = "inconsistency"))
    #>   s1 s2         model  type estimate  ci.lb  ci.ub    pval   measure mods
    #> b  a  b inconsistency model   0.2851  0.247  0.323 3.8e-49 O:E ratio     
    #> c  a  c inconsistency model  -0.1836 -0.221 -0.146 8.7e-22 O:E ratio     
    #> e  a  e inconsistency model  -0.0076 -0.045  0.030 6.9e-01 O:E ratio     
    #> g  a  g inconsistency model  -0.0024 -0.041  0.036 9.0e-01 O:E ratio

    check_homogeneity(modi)
    #>           tau2  QE df     QEp
    #> network 0.0038 325 44 1.8e-44
    check_consistency(ps)
    #> Warning: Removed 5 rows containing missing values (geom_linerange).
    #> Warning: Removed 5 rows containing missing values (geom_point).

<img src="man/figures/README-example-2.png" width="60%" />

``` r

fullres <- msc_full(ps, ref = "c")
plot(fullres)
```

<img src="man/figures/README-example-3.png" width="60%" />
