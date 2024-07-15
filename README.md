
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ROCsurf

<!-- badges: start -->

[![codecov](https://codecov.io/gh/ErtanSU/ROCsurf/graph/badge.svg?token=H5HIYVDUZU)](https://codecov.io/gh/ErtanSU/ROCsurf)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/ROCsurf)](https://cran.r-project.org/package=ROCsurf)
[![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)

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
#>  [1] 5.917825 2.979159 5.838546 5.173973 2.094935 3.075440 5.668035 5.046787
#>  [9] 4.113200 5.014748
rW(10,alpha=1,beta=2)
#>  [1] 1.3110825 0.7779134 6.7550000 2.5107787 0.1324531 1.8670518 3.7577509
#>  [8] 1.2523281 0.6048622 1.3589039
rL(10,alpha=1,beta=.1)
#>  [1] 1.0289670 1.3402274 1.1981434 0.9800550 0.9897064 1.1746219 0.8060061
#>  [8] 0.9373823 0.9887945 0.9968264
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
#> [1] 0.8456558
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
#> J  1.706378 6.054242 0.9348244 0.6525118 0.9479673
#> ED 1.460015 6.690269 0.8616875 0.7390811 0.8929925
#> MV 1.603189 6.265512 0.9093095 0.6872427 0.9331939
#> NI 1.610301 6.243586 0.9112888 0.6844771 0.9348726
#> M  1.492409 6.572374 0.8738204 0.7267540 0.9057189
```
