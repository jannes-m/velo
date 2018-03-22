<!-- README.md is generated from README.Rmd. Please edit that file -->
velo
====

**velo** is an R package which provides bicycle-related calculations frequently used by bike enthusiasts. Until now many of these functions are spread over the web, are unreliable mostly due to insufficient documentation and/or do not exist. The main goals of the velo-package are:

-   Bring together the most important bicycle-related calculations in a well-documented, transparent and reproducible manner. So far, **velo** lets you compute:

1.  chain lengths.
2.  the optimal combination of teeths for fixie bikes.
3.  spoke lenghts.
4.  the trail.

-   Make them interactively usable via RShiny Apps (see the sibling [velo\_apps](https://github.com/jannes-m/velo_apps) repository).

You can install the latest development version from github with

``` r
if (packageVersion("devtools") < 1.6) {
    install.packages("devtools")    
    }
if (!"lazyeval" %in% installed.packages()[, "Package"]) {
devtools::install_github("hadley/lazyeval")  
}
devtools::install_github("jannes-m/velo")
```

TO DO
-----

-   function calc\_chain also for fixie bikes.
-   build Shiny apps for all functions.
