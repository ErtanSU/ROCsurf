
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ROCsurf

<!-- badges: start -->

[![codecov](https://codecov.io/gh/ErtanSU/ROCsurf/graph/badge.svg?token=H5HIYVDUZU)](https://app.codecov.io/gh/ErtanSU/ROCsurf)
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
#>  [1] 5.263249 2.746883 6.095677 4.689061 7.435349 6.559738 6.898039 4.857616
#>  [9] 3.375865 6.733771
rW(10,alpha=1,beta=2)
#>  [1] 1.0009102 1.6093087 1.9906277 0.4007770 2.7977577 1.8105947 0.2820471
#>  [8] 4.0858497 1.0279749 1.2837081
rL(10,alpha=1,beta=.1)
#>  [1] 0.9394262 1.2702281 1.0048722 1.0936450 1.0106708 0.8210651 0.6985202
#>  [8] 1.0682473 1.0884213 0.9931167
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
#> [1] 0.8229934
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
#> J  1.755200 5.825962 0.9443484 0.5039488 0.9282470
#> PM 1.381565 6.703931 0.8371204 0.6259782 0.8417992
#> MV 1.562807 6.266451 0.9002974 0.5676947 0.8912959
#> NI 1.587273 6.218550 0.9070946 0.5602863 0.8959032
#> M  1.459123 6.503283 0.8670537 0.6006489 0.8662109
```

``` r
library(ROCsurf)
x<- rW(100,  2,  1)
y <- rG(100,  2, 2)
z <- rW(100,  6,  9)
r.tc_graph(x=x,y=y,z=z,
           init_param=c(alpha1=2,beta1=1,alpha2=2,
                        beta2=2,alpha3=6,beta3=9),
           empirical=FALSE,model=c("WGW"),
           method=c("MLE"))
```

<figure>
<img src="C:/Users/stati/Desktop/plot.png" alt="Plot Example" />
<figcaption aria-hidden="true">Plot Example</figcaption>
</figure>

## Corresponding Author

Department of Statistics, Faculty of Science, Selcuk University, 42250,
Konya, Turkey <br />

Email:<https://www.researchgate.net/profile/Ertan-Akgenc>

## References

Akgenç, E., and Kuş, C., 2023, Statistical Inference for ROC Surface
Analysis Under the Three-Class Problems, 7th International Congress of
Researchers, Statisticians and Young Statisticians (IRSYSC-2023).

B. R. Mosier and L. E. Bantis., 2021, Estimation and construction of
confidence intervals for biomarker cutoff-points under the shortest
euclidean distance from the roc surface to the perfection corner,
Statistics in medicine, 40(20):4522–4539.

G. Jameson., 2016, The incomplete gamma functions., The Mathematical
Gazette, 100(548):298–306.

T. Dong., 2014, Selected Topics on Statistical Methods for Three and
Multiple Class Diagnostic Studies, State University of New York at
Buffalo.

J. Luo and C. Xiong., 2013, Youden index and associated cut-points for
three ordinal diagnostic groups, Communications in Statistics-Simulation
and Computation, 42(6):1213–1234.

F. Edition, A. Papoulis, and S. U. Pillai., 2002, Pobability, random
variables, and stochastic processes, McGraw-Hill Europe: New York, NY,
USA.

A. J. Hallinan Jr., 1993, A review of the weibull distribution, Journal
of Quality Technology, 25(2):85–93.

N. Balakrishnan., 1991, Handbook of the logistic distribution, CRC
Press.
