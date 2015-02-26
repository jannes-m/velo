#' @title Calculation of spoke lengths
#' @description Calculate the spoke lengths for a given erd, flange, etc.
#' @param left_flange_d Left flange diameter.
#' @param right_flange_d Right flange diameter.
#' @param erd Effective rim diameter.
#' @param spoke_hole_d spoke hole diameter.
#' @param cross Number of crosses.
#' @param wl Width from center to left flange.
#' @param wr width from center to right flange
#' @param osb Offset spoke bed.
#' @param n Total number of spokes.
#' @examples 
#' calc_spoke_lengths(n = 36, erd = 558, left_flange_d = 67,
#'                    right_flange_d = 67, wl = 44, wr = 31)


calc_spoke_lengths <- function(n, erd, left_flange_d, right_flange_d, 
                               wl, wr, cross = 3, spoke_hole_d = 2.4, osb = 0) {
  # write some kind of closure
  calc_spoke <- function(flange_d, wl = NULL, wr = NULL) {
    f_d <- (flange_d / 2 * sin(2 * pi * cross / (n / 2)))^2
    erd_2 <- (erd / 2 - ((flange_d / 2) * cos(2 * pi * cross / (n / 2))))^2
    w_eff <- if (!is.null(wl)) {
      (wl + osb)^2  
    } else {
      (wr - osb)^2
    }
    shd <- spoke_hole_d / 2  
    sqrt(f_d + erd_2 + w_eff) - shd
    
  }
  data.frame(
    "left_length" = round(calc_spoke(flange_d = left_flange_d, wl = wl), 1),
    "right_length" = round(calc_spoke(flange_d = right_flange_d, wr = wr), 1)
  )
}

# erd <- 558
# osb <- 0
# wl <- 44
# wr <- 31
# left_flange_d <- 67
# right_flange_d <- 67
# spoke_hole_d <- 2.4
# cross <- 3
# n <- 36



