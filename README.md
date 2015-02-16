<!-- README.md is generated from README.Rmd. Please edit that file -->


velo
====

velo is an R package which provides bicycle-related calculations which are frequently used by bike enthusiasts. However, many of these functions are spread over the web and/or do not exist. The main goals of the velo-package are:

-   Make available the most important bicycle-related calculations in one place.

-   In the future the implemented functions should be interactively usable with RShiny Apps.

You can install the latest development version from github with

    ```R
    if (packageVersion("devtools") < 1.6) {
      install.packages("devtools")
    }
    devtools::install_github("hadley/lazyeval")
    devtools::install_github("jannes-m/velo")
    ```
