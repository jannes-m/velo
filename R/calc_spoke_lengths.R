#' @title Calculation of spoke lengths
#' @description Calculate the spoke lengths for a given effective rim diameter
#'   (ERD) in combination with the necessary hub dimensions.
#' @param left_flange_d Diameter of the left flange (mm).
#' @param right_flange_d Diameter of the right flange (mm).
#' @param erd Effective rim diameter of the wheel (mm).
#' @param spoke_hole_d Diameter of the spoke hole (mm).
#' @param cross Number of times each spoke crosses other spokes. The default is
#'   the common 3-cross lacing pattern.
#' @param wl Width from the hub center to the left flange (mm).
#' @param wr Width from the hub center to the right flange (mm).
#' @param osb Offset spoke bed in mm (default: 0).
#' @param n Total number of spokes per bike wheel (default: 32).
#' @author Jannes Muenchow
#' @return The function returns a data.frame with one row and the columns
#'   "left_length" and "right_length" in mm.
#' @export
#' @examples 
#' calc_spoke_lengths(n = 36, erd = 558, left_flange_d = 45,
#'                    right_flange_d = 45, wl = 44, wr = 31)


calc_spoke_lengths <- function(n = 32, erd, left_flange_d, right_flange_d, 
                               wl, wr, cross = 3, spoke_hole_d = 2.4, 
                               osb = 0) {
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
# left_flange_d <- 40
# right_flange_d <- 40
# spoke_hole_d <- 2.4
# cross <- 3
# n <- 36



