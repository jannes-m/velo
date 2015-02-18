<!-- README.md is generated from README.Rmd. Please edit that file -->


velo
====

velo is an R package which provides bicycle-related calculations frequently used by bike enthusiasts. So far, many of these functions are spread over the web, are unreliable due to insufficient documentation and/or do not exist. The main goals of the velo-package are:

-   Bring together the most important bicycle-related calculations in a well-documented, transparent and reproducible manner (well, so far, there are just two functions, ...).

-   In the future, RShiny should make the implemented functions interactively usable.

You can install the latest development version from github with

``` {.r}
if (packageVersion("devtools") < 1.6) {
    install.packages("devtools")    
    }
if (!"lazyeval" %in% installed.packages()[, "Package"]) {
devtools::install_github("hadley/lazyeval")  
}
devtools::install_github("jannes-m/velo")
```

TO DO
=====

-   calc\_fixie\_teeth: Add function arguments that let the user specify the teeth range of cog and chainring.
-   schoolmath package is rather slow, write a faster version to calculate the least common denominator and the greatest common divisor
-   function to calculate the spoke length
-   function to calculate the chain length
-   implement functions in RShiny
