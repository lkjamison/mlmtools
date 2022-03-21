
# mlmtools

Multilevel and mixed effects models often require specialized data
pre-processing and further post-estimation derivations and graphics to
gain insight into model results. `mlmtools` is a suite of pre- and
post-estimation tools for multilevel models in R. The package’s
post-estimation tools are designed to work with models estimated using
`lme4`’s lmer function, which fits linear mixed effects regression
models. Although nearly all the functions provided in the `mlmtools`
package exist as singleton functions within other R packages, they are
often improved in `mlmtools` and more accessible by being located within
a multilevel modeling specific package.

The package was developed by Jessica Mazen, Laura Jamison, Erik Ruzek,
and Gus Sjobek.

## Included functions

-   Intraclass correlation coefficients (ICC)
-   Variance explained
    -   When comparing nested models
    -   ![R^2](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;R%5E2 "R^2")
-   Test of necessity of random intercepts
-   Visualizations
    -   Associations between variables within clusters
    -   Associations between variables between clusters
    -   Caterpillar plots

## Installation

To install the latest release version (0.0.0.9) from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("lj5yn/mlmtools")
#> Downloading GitHub repo lj5yn/mlmtools@HEAD
<<<<<<< HEAD
#> Installing 1 packages: Rcpp
#> Installing package into 'C:/Users/erik.ruzek/AppData/Local/Temp/1/RtmpCyJOvF/temp_libpath51282ae578da'
#> (as 'lib' is unspecified)
#> Installing package into 'C:/Users/erik.ruzek/AppData/Local/Temp/1/RtmpCyJOvF/temp_libpath51282ae578da'
=======
#> Rcpp (1.0.8 -> 1.0.8.3) [CRAN]
#> Installing 1 packages: Rcpp
#> Installing package into 'C:/Users/erik.ruzek/AppData/Local/Temp/1/RtmpCyJOvF/temp_libpath51284b2f6c6f'
#> (as 'lib' is unspecified)
#> package 'Rcpp' successfully unpacked and MD5 sums checked
#> 
#> The downloaded binary packages are in
#>  C:\Users\erik.ruzek\AppData\Local\Temp\1\Rtmp6DnsmB\downloaded_packages
#>       v  checking for file 'C:\Users\erik.ruzek\AppData\Local\Temp\1\Rtmp6DnsmB\remotes2c41e88b\lj5yn-mlmtools-7364190/DESCRIPTION'
#>       -  preparing 'mlmtools':
#>    checking DESCRIPTION meta-information ...     checking DESCRIPTION meta-information ...   v  checking DESCRIPTION meta-information
#>       -  checking for LF line-endings in source and make files and shell scripts
#>       -  checking for empty or unneeded directories
#>      NB: this package now depends on R (>=        NB: this package now depends on R (>= 3.5.0)
#>      WARNING: Added dependency on R >= 3.5.0 because serialized objects in
#>      serialize/load version 3 cannot be read in older versions of R.
#>      File(s) containing such objects:
#>        'mlmtools/Data/instruction.rda'
#>   -  building 'mlmtools_0.0.0.9000.tar.gz'
#>      
#> 
#> Installing package into 'C:/Users/erik.ruzek/AppData/Local/Temp/1/RtmpCyJOvF/temp_libpath51284b2f6c6f'
>>>>>>> 03792c21d74ddfae49919204fcdabb0fde7efb6e
#> (as 'lib' is unspecified)
```

## Sample workflow

Working with the included data, we briefly show how some of the included
functions can be used.

``` r
# data
library(mlmtools)
library(lme4)
#> Loading required package: Matrix
data("instruction")

# fit variance components model
fit1 <- lmer(mathgain ~ 1 + (1|classid), instruction)
  
# intraclass correlation coefficient
ICCm(fit1)
#> Likeness of mathgain values of units in the same classid factor: 0.149

# add predictor
fit2 <- lmer(mathgain ~ mathkind + (1|classid), instruction)

# variance explained by adding predictor
varCompare(fit2, fit1)
#> fit2 explains 30.91% more variance than fit1
```

## Visualizations

Rich visualizations of associations can be had along with caterpillar
plots, which graph the 95% prediction intervals for the random effects.

``` r
# visualze within-group association
withinPlot(x = "mathkind", y = "mathgain", grouping = "classid", dataset = instruction)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="75%" />

``` r
# caterpillar plot
caterpillarPlot(fit2, grpvar = "classid")
#> [1] "classid"
```

<img src="man/figures/README-unnamed-chunk-4-2.png" width="75%" />
