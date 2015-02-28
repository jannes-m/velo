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
#' @details Essentially, Euclidean geometry solves the problem of calculating 
#'   the length of a spoke. ERD and hub dimensions provide the necessary lengths
#'   while crossing pattern and the number of spoke holes determine the angle. 
#'   Please refer to 
#'   \url{http://www.wheelpro.co.uk/support/spoke-length-proof.php} for an
#'   excellent mathematical proof, both in terms of explanation and depiction.
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
  # calculate the angle in degrees (depends on spoke hole number and 
  # crossing pattern)
  angle <- 360 / (n / 2) * cross
  # convert the angle to radians (R's trigonometric functions demand radians)
  rad <-  angle * pi / 180
  
  # write an internal function
  calc_spoke <- function(flange_d, w) {
    # square the rim radius
    r_2 <- (erd / 2)^2
    # square the hub flange diameter
    h_2 <- (flange_d / 2)^2
    # square the flange offset
    f_2 <-  w^2
    # take the angle into account
    a <- 2 * (erd / 2) * (left_flange_d / 2) * cos(rad)
    # Pythagorean theorem and cosine rule in conjunction
    sqrt(r_2 + h_2 + f_2 - a) -
      # subtract half the spoke hole diameter
      spoke_hole_d / 2
  }
  # calculate the spoke lengths
  data.frame(
    "left_length" = round(calc_spoke(flange_d = left_flange_d, w = wl), 1),
    "right_length" = round(calc_spoke(flange_d = right_flange_d, w = wr), 1)
  )
}

