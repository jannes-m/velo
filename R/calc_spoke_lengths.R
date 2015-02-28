#' @title Calculation of spoke lengths
#' @description Calculate the spoke lengths for a given effective rim diameter
#'   (ERD) in combination with the necessary hub dimensions.
#' @param left_flange_d Diameter of the left flange (mm).
#' @param right_flange_d Diameter of the right flange (mm).
#' @param erd Effective rim diameter (mm), which is the diameter of the rim at
#'   the nipple seats, sometimes also referred to as over spoke diameter.
#' @param spoke_hole_d Diameter of the spoke hole (mm).
#' @param cross Number of times each spoke crosses other spokes. The default is
#'   3 (3-cross lacing pattern).
#' @param wl Width from the hub center to the left flange (mm).
#' @param wr Width from the hub center to the right flange (mm).
#' @param n Total number of spokes per bike wheel (default: 32).
#' @author Jannes Muenchow
#' @return The function returns a data.frame with one row and the columns
#'   \strong{left_length} and \strong{right_length} in mm.
#' @export
#' @note So far the function assumes that the nipple has a length of 12 mm.
#' @references Roger Musson (2013): A professional guide to wheelbuilding. 6th
#'   Edition. \url{http://www.wheelpro.co.uk/}.
#' @examples 
#' # Rigida Zac 2000 with 36 spoke holes
#' # Shimano Deore LX FH-M580 (rear hub)
#' calc_spoke_lengths(n = 36, erd = 537, left_flange_d = 45,
#'                    right_flange_d = 45, wl = 37.5, wr = 17.5)

calc_spoke_lengths <- function(n = 32, erd, left_flange_d, right_flange_d, 
                               wl, wr, cross = 3, spoke_hole_d = 2.4) {
  # write some kind of closure
  calc_spoke <- function(flange_d, w) {
    f_d <- (flange_d / 2 * sin(2 * pi * cross / (n / 2)))^2
    erd_2 <- (erd / 2 - ((flange_d / 2) * cos(2 * pi * cross / (n / 2))))^2
    w_eff <- w^2
    shd <- spoke_hole_d / 2  
    sqrt(f_d + erd_2 + w_eff) - shd
  }
  #calculate the spoke lengths
  data.frame(
    "left_length" = round(calc_spoke(flange_d = left_flange_d, w = wl), 1),
    "right_length" = round(calc_spoke(flange_d = right_flange_d, w = wr), 1)
  )
}

