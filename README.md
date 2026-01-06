
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
#>     study  id outcome    a  b    c     d  e    f  g  h  i age female    x1  sex
#> 1.1     1   1       1 0.26 NA 0.36 0.105 NA 0.21 NA NA NA  49      0 -1.13 male
#> 1.2     1  10       0 0.33 NA 0.45 0.193 NA 0.29 NA NA NA  68      0 -1.09 male
#> 1.3     1 100       0 0.24 NA 0.35 0.094 NA 0.20 NA NA NA  43      0 -1.42 male
#> 1.4     1 101       0 0.31 NA 0.43    NA NA 0.27 NA NA NA  57      0 -0.95 male
#> 1.5     1 102       0 0.31 NA 0.42 0.166 NA 0.27 NA NA NA  44      0 -1.04 male
#> 1.6     1 103       0 0.30 NA 0.42    NA NA   NA NA NA NA  43      0 -0.81 male

get_performance_statistics(data = dat, 
                          scores = c("a", "b", "c", "e", "g"), 
                          cohort = "study", outcome = "outcome", 
                          fn = list(AUC = c_statistic, 
                                    `O/E` = oe_ratio,
                                    BS = brier_score))
#> $AUC
#>       a    b    c    e    g
#> 1  0.69   NA 0.66   NA   NA
#> 10 0.66 0.67 0.66 0.68   NA
#> 2  0.74   NA 0.74 0.76   NA
#> 3  0.70 0.68 0.68 0.68 0.71
#> 4  0.62 0.63   NA 0.61 0.60
#> 5  0.65 0.64 0.64   NA   NA
#> 6  0.70 0.72 0.70   NA 0.71
#> 7  0.62 0.62   NA 0.61 0.65
#> 8  0.60 0.60 0.59   NA 0.60
#> 9  0.65 0.66 0.63 0.64 0.67
#> 
#> $`O/E`
#>       a    b    c    e    g
#> 1  0.53   NA 0.35   NA   NA
#> 10 0.98 1.32 0.81 1.02   NA
#> 2  0.30   NA 0.22 0.21   NA
#> 3  0.44 0.64 0.29 0.34 0.38
#> 4  0.88 1.17   NA 0.87 0.86
#> 5  1.02 1.36 0.81   NA   NA
#> 6  0.57 0.85 0.46   NA 0.58
#> 7  1.11 1.41   NA 1.15 1.12
#> 8  1.03 1.36 0.82   NA 1.07
#> 9  0.41 0.59 0.32 0.34 0.38
#> 
#> $BS
#>        a    b    c    e    g
#> 1  0.148   NA 0.19   NA   NA
#> 10 0.237 0.25 0.25 0.24   NA
#> 2  0.082   NA 0.12 0.11   NA
#> 3  0.138 0.12 0.19 0.16 0.15
#> 4  0.238 0.24   NA 0.24 0.24
#> 5  0.238 0.25 0.25   NA   NA
#> 6  0.164 0.15 0.21   NA 0.18
#> 7  0.220 0.25   NA 0.23 0.22
#> 8  0.244 0.26 0.26   NA 0.24
#> 9  0.143 0.12 0.20 0.17 0.15
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
mod
#>    perfmeasure term    type estimate std.error statistic p.value conf.low conf.high ref evidence
#> 1          AUC    b summary  0.00099    0.0041      0.24 8.1e-01 -7.0e-03   0.00893   a  network
#> 2          AUC    c summary -0.00968    0.0048     -2.01 4.5e-02 -1.9e-02  -0.00024   a  network
#> 3          AUC    e summary -0.00099    0.0063     -0.16 8.8e-01 -1.3e-02   0.01138   a  network
#> 4          AUC    g summary  0.00281    0.0058      0.48 6.3e-01 -8.6e-03   0.01423   a  network
#> 5           CS    b summary  0.01155    0.0720      0.16 8.7e-01 -1.3e-01   0.15267   a  network
#> 6           CS    c summary -0.18249    0.0805     -2.27 2.3e-02 -3.4e-01  -0.02471   a  network
#> 7           CS    e summary  1.13059    0.1646      6.87 6.5e-12  8.1e-01   1.45321   a  network
#> 8           CS    g summary  0.53495    0.1183      4.52 6.1e-06  3.0e-01   0.76681   a  network
#> 9           BS    b summary  0.00202    0.0069      0.29 7.7e-01 -1.1e-02   0.01550   a  network
#> 10          BS    c summary  0.03593    0.0068      5.32 1.0e-07  2.3e-02   0.04918   a  network
#> 11          BS    e summary  0.01451    0.0074      1.97 4.9e-02  9.5e-05   0.02893   a  network
#> 12          BS    g summary  0.00663    0.0074      0.90 3.7e-01 -7.8e-03   0.02109   a  network
plot(mod)
```

<img src="man/figures/README-example-1.png" width="60%" />

``` r

check_homogeneity(mod)
#>   measure    tau2  QE df     QEp
#> 1     AUC 2.5e-05  22 24 5.7e-01
#> 2      CS 1.3e-02  31 24 1.7e-01
#> 3      BS 3.7e-04 395 24 6.6e-69
```
