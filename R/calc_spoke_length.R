#' @title Calculation of spoke lengths
#' @description Calculate the spoke lengths for a given ERD, flange, etc.
#' @param left_flange_d Left flange diameter.
#' @param right_flange_d Right flange diameter.
#' @param ERD Effective rim diameter.
#' @param spoke_hole_d spoke hole diameter.
#' @param cross Number of crosses.
#' @param wl Width from center to left flange.
#' @param wr width from center to right flange
#' @param osb Offset spoke bed.
#' @param n Total number of spokes.

ERD <- 558
osb <- 0
wl <- 34
wr <- 34
left_flange_d <- 67
right_flange_d <- 67
spoke_hole_d <- 2.4
cross <- 3
n <- 38

# write a closure
calc_spoke <- function(flange_d, wl = NULL, wr = NULL) {
#   sqrt(
#     (flange_d / 2 * sin(2 * pi * cross / (n / 2)))^2 +
#       (ERD / 2 - 
#          ((flange_d / 2) * cos(2 * pi * cross / (n / 2))))^2 +
#       if (!is.null(wl)) {
#         (wl + osb)^2  
#       } else {
#         (wr - osb)^2
#       }
#       
#   ) - spoke_hole_d / 2  
  f_d <- (flange_d / 2 * sin(2 * pi * cross / (n / 2)))^2
  erd_2 <- (ERD / 2 - ((flange_d / 2) * cos(2 * pi * cross / (n / 2))))^2
  w_eff <- if (!is.null(wl)) {
    (wl + osb)^2  
  } else {
    (wr - osb)^2
  }
  shd <-   spoke_hole_d / 2  
  sqrt(f_d + erd_2 + w_eff) - shd
  
}

calc_spoke(flange_d = left_flange_d, wl = wl)
power <- function(exponent) {
  function(x) {
    x ^ exponent
  }
}

square <- power(2)
square(2)


