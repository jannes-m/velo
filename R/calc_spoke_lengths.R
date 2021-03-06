#' @title Calculation of spoke lengths
#' @description Calculate the spoke lengths for a given effective rim diameter 
#'   (ERD) in combination with the necessary hub dimensions.
#' @param left_flange_d Diameter of the left flange (mm).
#' @param right_flange_d Diameter of the right flange (mm).
#' @param erd Effective rim diameter (mm), which is the diameter of the rim at 
#'   the nipple seats, i.e. the diameter at the end of the spokes in the 
#'   finished wheel. This is sometimes also referred to as over spoke diameter.
#' @param spoke_hole_d Diameter of the spoke hole (mm).
#' @param cross Number of times each spoke crosses other spokes. The default is 
#'   3 (3-cross lacing pattern).
#' @param wl Width from the hub center to the left flange (mm).
#' @param wr Width from the hub center to the right flange (mm).
#' @param n Total number of spokes per bike wheel (default: 32).
#' @param offset The distance (in mm) by which the spoke holes are positioned
#'   away from the rim centerline (default: 0).
#' @param side side can take one of three values: "no_offset" (default), "front"
#'   or "rear". You only need to specify this argument if your rim comes with an
#'   offset. Offsets are subtracted from \code{wl} and added to \code{wr} in the
#'   rear case (sprokes on the right side cause wheel dish), and vice versa in
#'   the front case (disc brake on the left might cause wheel dish).
#' @details Essentially, Euclidean geometry solves the problem of calculating 
#'   the length of a spoke. ERD and hub dimensions provide the necessary lengths
#'   while crossing pattern and the number of spoke holes determine the angle.
#'   The function uses following formulas to derive the spoke length:
#'  \deqn{r^2 = (\frac{erd}{2}^{2}}{r^2 = (erd / 2)^2}
#'  \deqn{h^2 = (\frac{flange_d}{2}^2}{h^2 = (flange_d / 2)^2}
#'  \deqn{f^2 = w^2}{f^2 = w^2}
#'  \deqn{cos_a = (\cos((\frac{360}{\frac{n}{2}} * cross) *
#'   pi / 180)}{cos_a = cos((360 / (n / 2) * cross) * pi / 180)}
#'   \deqn{spoke_length = 
#'    \sqrt(r^2 + h^2 + f^2 - 
#'    2 * \frac{erd}{2} * \frac{flange_d}{2} * cos_a) - 
#'    \frac{spoke_hole_d}{2}}{spoke_length = 
#'    sqrt(r^2 + h^2 + f^2 - 
#'    2 * (erd / 2) * (flange_d / 2) * cos_a) - 
#'    spoke_hole_d / 2}
#' for \eqn{flange_d = left or right flange diameter,
#'           w = width from the the hub center to the left or right flange}
#' 
#'  Please refer to 
#'   \url{http://www.wheelpro.co.uk/support/spoke-length-proof.php} for an
#'   excellent mathematical proof, both in terms of explanation and depiction.
#' @author Jannes Muenchow
#' @return The function returns a data.frame with one row and the columns 
#'   \strong{left_length} and \strong{right_length} in mm.
#' @note The function is not applicable to straight pull spokes.
#' @export
#' @references Roger Musson (2013): A professional guide to wheelbuilding. 6th 
#'   Edition. \url{http://www.wheelpro.co.uk/}.
#' @examples 
#' # Rigida Zac 2000 with 36 spoke holes
#' # Shimano Deore LX FH-M580 (rear hub)
#' calc_spoke_lengths(n = 36, erd = 537, left_flange_d = 45,
#'                    right_flange_d = 45, wl = 37.5, wr = 17.5)
#' # using an offset
#' calc_spoke_lengths(n = 36, erd = 537, left_flange_d = 45,
#'                    right_flange_d = 45, wl = 37.5, wr = 17.5, offset = 3,
#'                    side = "rear") 

calc_spoke_lengths <- function(n = 32, erd = NULL, left_flange_d = NULL, 
                               right_flange_d = NULL, wl = NULL, wr = NULL,
                               cross = 3, spoke_hole_d = 2.4, offset = 0,
                               side = "no_offset") {
  
  # test if all necessary arguments were provided
  args <- c("erd", "left_flange_d", "right_flange_d", "wl", "wr")
  ind <- mapply(function(x) {
    is.null(get(x))},
    as.list(args))
  if (any(ind)) {
    stop("Please specify also: ", paste(args[ind], collapse = ", "))
  }
  
  # test if all args are positive
  args <- c(args, "cross", "spoke_hole_d", "offset")
  ind  <- mapply(function(x) {
    get(x) < 0},
    as.list(args))
  if (any(ind)) {
   stop(paste(args[ind], collapse = ", "), " should be positive!") 
  }
  
  # incorporate offset
  if (side == "rear") {
    wl <- wl - offset
    wr <- wr + offset
    }
  else if (side == "front") {
    wl <- wl + offset
    wr <- wr - offset
  }

  # calculate the angle in degrees (depends on spoke hole number and 
  # crossing pattern)
  angle <- 360 / (n / 2) * cross
  # convert the angle to radians (R's trigonometric functions demand radians)
  rad <-  angle * pi / 180
  # take the cosinus
  cos_a <- cos(rad)
  
  # write an internal function
  calc_spoke <- function(flange_d, w) {
    # square the rim radius
    r_2 <- (erd / 2)^2
    # square the hub flange diameter
    h_2 <- (flange_d / 2)^2
    # square the flange offset
    f_2 <-  w^2
    # Pythagorean theorem and cosine rule in conjunction
    sqrt(r_2 + h_2 + f_2 - 2 * (erd / 2) * (flange_d / 2) * cos_a) -
      # subtract half the spoke hole diameter
      spoke_hole_d / 2
  }
  # calculate the spoke lengths
  data.frame(
    "left_length" = round(calc_spoke(flange_d = left_flange_d, w = wl), 1),
    "right_length" = round(calc_spoke(flange_d = right_flange_d, w = wr), 1)
  )
}
