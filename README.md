<!-- README.md is generated from README.Rmd. Please edit that file -->


velo
====

velo is an R package which provides bicycle-related calculations which are frequently used by bike enthusiasts. However, many of these functions are spread over the web and/or do not exist. The main goals of the velo-package are:

-   Make available the most important bicycle-related calculations in one place (well, so far, there is just one function, ...).

-   In the future, RShiny should make the implemented functions interactively usable.

You can install the latest development version from github with

``` {.r}
if (packageVersion("devtools") < 1.6) {
    install.packages("devtools")    
    }
devtools::install_github("hadley/lazyeval")
devtools::install_github("jannes-m/velo")
```

Try another chunk:

``` {.r}
summary(1:200)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>    1.00   50.75  100.50  100.50  150.20  200.00
```
