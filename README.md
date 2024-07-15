
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

You can install the development version of PLindleyROC via the following
code:

``` r
# install.packages("devtools")
devtools::install_github("ErtanSU/PLindleyROC")
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
#>  [1] 5.014395 7.478968 3.993109 7.340816 3.338429 3.704237 7.033924 5.415578
#>  [9] 7.090577 2.858571
rW(10,alpha=1,beta=2)
#>  [1] 1.7803489 0.3415579 0.2229419 0.2081608 0.9559279 0.1565231 1.1233859
#>  [8] 1.3014633 0.3240137 0.3787292
rL(10,alpha=1,beta=.1)
#>  [1] 0.9141303 1.1386914 1.0638101 0.9861419 1.0115614 0.8795247 1.0533510
#>  [8] 0.6720048 0.9577669 1.1468490
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
#> [1] 0.7778899
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
#> J  1.905677 5.719356 0.9403354 0.5093325 0.9248716
#> ED 1.533210 6.513564 0.8339161 0.6321922 0.8401581
#> MV 1.712139 6.119012 0.8953695 0.5740733 0.8882849
#> NI 1.737129 6.073607 0.9023358 0.5663510 0.8930221
#> M  1.608500 6.335505 0.8623463 0.6073986 0.8634695
```
