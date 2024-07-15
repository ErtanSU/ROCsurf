
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ROCsurf

<!-- badges: start -->

[![codecov](https://codecov.io/gh/ErtanSU/ROCsurf/graph/badge.svg?token=H5HIYVDUZU)](https://codecov.io/gh/ErtanSU/ROCsurf)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/ROCsurf)](https://cran.r-project.org/package=ROCsurf)
[![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![R-CMD-check](https://github.com/ErtanSU/ROCsurf/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ErtanSU/ROCsurf/blob/master/.github/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The objective of this project is to assess the Receiver Operating
Characteristic (ROC) surfaces for Gamma, Weibull, and Logistic
distributions. Additionally, it supports performance evaluations linked
to these ROC surfaces in the context of three-class problems.

## Installation

You can install the development version of ROCsurf via the following
code:

``` r
# install.packages("devtools")
devtools::install_github("ErtanSU/ROCsurf")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ROCsurf)
dG(c(1,2,3,4,5,200,1000),alpha=6,beta=.8)
#> [1] 9.107742e-03 8.350118e-02 1.816690e-01 2.193342e-01 1.917734e-01
#> [6] 2.715241e-99 0.000000e+00
dW(c(1,2,3,4,5,200,10000),alpha=1,beta=2)
#> [1] 3.032653e-01 1.839397e-01 1.115651e-01 6.766764e-02 4.104250e-02
#> [6] 1.860038e-44 0.000000e+00
dL(c(1,2,3,4,5,200),alpha=1,beta=.1)
#> [1] 2.500000e+00 4.539581e-04 2.061154e-08 9.357623e-13 4.248354e-17
#> [6] 0.000000e+00
```

``` r
library(ROCsurf)
pG(c(.5,1,2,3,4,25),alpha=6,beta=.8)
#> [1] 4.859954e-05 1.838085e-03 4.202104e-02 1.771172e-01 3.840393e-01
#> [6] 1.000000e+00
pW(c(.5,1,2,3,4,100),alpha=1,beta=2)
#> [1] 0.2211992 0.3934693 0.6321206 0.7768698 0.8646647 1.0000000
pL(c(.5,1,2,100),alpha=1,beta=.1)
#> [1] 0.006692851 0.500000000 0.999954602 1.000000000
```

``` r
library(ROCsurf)
qG(c(.9971,0.5,0.3),alpha=6,beta=.8)
#> [1] 11.956348  4.536129  3.613711
qW(c(.9971,0.5,0.3),alpha=1,beta=2)
#> [1] 11.6860891  1.3862944  0.7133499
qL(c(.9971,0.5,0.3),alpha=1,beta=.1)
#> [1] 1.5840140 1.0000000 0.9152702
```

``` r
library(ROCsurf)
rG(10,alpha=6,beta=.8)
#>  [1] 2.835589 4.487960 4.993129 7.465337 4.363234 3.931606 7.777716 2.695726
#>  [9] 3.898687 6.799784
rW(10,alpha=1,beta=2)
#>  [1] 2.4421907 2.6507354 1.0140019 0.9411196 3.4773911 2.4308225 3.5317633
#>  [8] 2.3734884 1.7885298 2.2898301
rL(10,alpha=1,beta=.1)
#>  [1] 0.8201721 0.8356582 1.0751062 0.7459417 1.3854890 1.1820001 0.7887300
#>  [8] 1.2213914 1.1275483 1.0391970
```

``` r
library(ROCsurf)
x<-rW(100,  2,  1)
y <- rG(100,  2, 2)
z <- rW(100,  6,  9)
r.tc_vus(x=x,y=y,z=z,
         init_param=c(alpha1=2,beta1=1,alpha2=2,beta2=2,
                      alpha3=6,beta3=9),
         model=c("WGW"), method=c("MLE"))
#> [1] 0.7912406
```

``` r
library(ROCsurf)
x<- rW(100,  2,  1)
y <- rG(100,  2, 2)
z <- rW(100,  6,  9)
r.tc_index(x=x,y=y,z=z,
           init_param=c(alpha1=2,beta1=1,alpha2=2,
                        beta2=2,alpha3=6,beta3=9),
           init_index=c(median(x),median(y)),
           model=c("WGW"),
           method=c("MLE"))
#>          c₁       c₂      TPF₁      TPF₂      TPF₃
#> J  1.593545 6.200913 0.9415987 0.5841529 0.9479027
#> ED 1.291759 7.003363 0.8505880 0.6854393 0.8798185
#> MV 1.454482 6.510617 0.9079366 0.6291285 0.9269231
#> NI 1.465425 6.477498 0.9110534 0.6251875 0.9294522
#> M  1.341002 6.830401 0.8702382 0.6677304 0.8985064
```
