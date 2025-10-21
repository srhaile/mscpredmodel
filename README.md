
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

dat <- msc_sample_data(n.cohorts = 10)
head(dat)
#>     study  id outcome    a     b  c     d    e    f    g  h  i age female   x1    sex
#> 1.1     1   1       0 0.18 0.114 NA 0.043   NA 0.13 0.22 NA NA  44      1 0.65 female
#> 1.2     1  10       0 0.20 0.132 NA 0.059   NA 0.15 0.24 NA NA  61      1 0.40 female
#> 1.3     1 100       0 0.18 0.116 NA 0.045 0.26   NA 0.22 NA NA  61      1 0.75 female
#> 1.4     1 101       0 0.15 0.098 NA 0.031 0.24 0.10 0.19 NA NA  58      1 0.70 female
#> 1.5     1 102       0 0.18 0.115 NA    NA 0.26 0.13 0.22 NA NA  45      1 0.29 female
#> 1.6     1 103       1 0.22 0.143 NA 0.070 0.30 0.17 0.25 NA NA  48      1 0.61 female

get_performance_statistics(data = dat, 
                          scores = c("a", "b", "c", "e", "g"), 
                          cohort = "study", outcome = "outcome", 
                          fn = list(AUC = c_statistic, 
                                    `O/E` = oe_ratio,
                                    BS = brier_score))
#> $AUC
#>       a    b    c    e    g
#> 1  0.75 0.81   NA 0.76 0.73
#> 10 0.72 0.73   NA 0.72   NA
#> 2  0.61 0.66 0.66   NA 0.66
#> 3  0.78 0.78 0.80 0.76 0.78
#> 4  0.73 0.71 0.73 0.74   NA
#> 5  0.65 0.65 0.62   NA 0.61
#> 6  0.65   NA 0.67 0.68   NA
#> 7  0.61 0.59 0.61 0.58 0.60
#> 8  0.58   NA 0.58 0.62   NA
#> 9  0.57 0.57   NA   NA 0.53
#> 
#> $`O/E`
#>       a    b    c    e    g
#> 1  0.41 0.57   NA 0.41 0.36
#> 10 0.20 0.31   NA 0.16   NA
#> 2  0.72 0.92 0.56   NA 0.68
#> 3  0.38 0.57 0.28 0.30 0.38
#> 4  0.38 0.51 0.27 0.29   NA
#> 5  0.57 0.84 0.46   NA 0.55
#> 6  0.95   NA 0.74 1.03   NA
#> 7  0.67 0.89 0.52 0.63 0.68
#> 8  0.73   NA 0.58 0.71   NA
#> 9  1.05 1.46   NA   NA 1.05
#> 
#> $BS
#>        a     b    c    e    g
#> 1  0.101 0.081   NA 0.14 0.11
#> 10 0.096 0.068   NA 0.13   NA
#> 2  0.218 0.194 0.26   NA 0.22
#> 3  0.107 0.089 0.15 0.14 0.12
#> 4  0.103 0.081 0.14 0.13   NA
#> 5  0.171 0.155 0.22   NA 0.18
#> 6  0.239    NA 0.27 0.24   NA
#> 7  0.193 0.175 0.24 0.21 0.20
#> 8  0.224    NA 0.27 0.23   NA
#> 9  0.246 0.267   NA   NA 0.25
#> 
#> attr(,"class")
#> [1] "perfsumm"

mod <- msc(scores = c("a", "b", "c", "e", "g"),
           outcome = "outcome", subjid = "id",
           cohort = "study", mods = NULL, data = dat,
           fn = list("AUC" = c_statistic,
                     "CS" = calibration_slope,
                     "BS" = brier_score),
           model = "consistency", direct = FALSE, indirect = FALSE,
           ref = "first")
#> Loading required package: future.apply
#> Loading required package: future
#> Loading required package: metafor
#> Loading required package: Matrix
#> Loading required package: metadat
#> Loading required package: numDeriv
#> 
#> Loading the 'metafor' package (version 4.8-0). For an
#> introduction to the package please type: help(metafor)
#> Loading required package: metafor
#> Loading required package: Matrix
#> Loading required package: metadat
#> Loading required package: numDeriv
#> 
#> Loading the 'metafor' package (version 4.8-0). For an
#> introduction to the package please type: help(metafor)
#> Loading required package: metafor
#> Loading required package: Matrix
#> Loading required package: metadat
#> Loading required package: numDeriv
#> 
#> Loading the 'metafor' package (version 4.8-0). For an
#> introduction to the package please type: help(metafor)
mod
#>    perfmeasure term    type estimate std.error statistic p.value conf.low conf.high ref evidence
#> 1          AUC    b summary  0.00404    0.0067     0.605 5.5e-01 -0.00904     0.017   a  network
#> 2          AUC    c summary  0.00739    0.0091     0.810 4.2e-01 -0.01050     0.025   a  network
#> 3          AUC    e summary  0.00077    0.0094     0.081 9.4e-01 -0.01767     0.019   a  network
#> 4          AUC    g summary -0.00422    0.0076    -0.554 5.8e-01 -0.01916     0.011   a  network
#> 5           CS    b summary  0.08049    0.1343     0.599 5.5e-01 -0.18270     0.344   a  network
#> 6           CS    c summary  0.02656    0.1457     0.182 8.6e-01 -0.25907     0.312   a  network
#> 7           CS    e summary  1.22449    0.2734     4.480 7.5e-06  0.68873     1.760   a  network
#> 8           CS    g summary  0.34282    0.1767     1.941 5.2e-02 -0.00344     0.689   a  network
#> 9           BS    b summary -0.01967    0.0041    -4.798 1.6e-06 -0.02770    -0.012   a  network
#> 10          BS    c summary  0.04246    0.0042    10.015 1.3e-23  0.03415     0.051   a  network
#> 11          BS    e summary  0.02107    0.0038     5.604 2.1e-08  0.01370     0.028   a  network
#> 12          BS    g summary  0.00689    0.0040     1.731 8.3e-02 -0.00091     0.015   a  network
plot(mod)
```

<img src="man/figures/README-example-1.png" width="60%" />

``` r

check_homogeneity(mod)
#>   measure    tau2  QE df     QEp
#> 1     AUC 1.3e-05  23 24 5.0e-01
#> 2      CS 3.8e-02  27 24 3.0e-01
#> 3      BS 9.5e-05 169 24 1.1e-23
```
