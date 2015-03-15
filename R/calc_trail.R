#' @title Calculate trail
#' @description Calculate the trail. For further information have a look at
#'   \url{http://en.wikipedia.org/wiki/Bicycle_and_motorcycle_geometry}.
#' @param dia Diameter of the wheel including the tire [mm].
#' @param rake The rake, also known as fork offest [mm].
#' @param ha Head angle [degrees].
#' @details \code{calc_trail} uses trigonometric functions to calculate the
#'   trail. Please refer to \code{vignette("velo_trail", package = "velo")} for
#'   a detailed description and mathematical proof.
#' @author Dirk Haas, Jannes Muenchow
#' @return The function returns a numeric representing the trail [mm].
#' @export
#' @examples 
#' calc_trail(dia = 700, rake = 58, ha = 73)
calc_trail <- function(dia = NULL, rake = NULL, ha = NULL) {
  # test
  args <- c("dia", "rake", "ha")
  ind <- mapply(function(x) is.null(get(x)), as.list(args))
  if (any(ind)) {
    stop("Please specify: ",
         paste(args[ind], collapse = ", "))
  }
  
  # calculate alpha
  alpha <- 90 - ha
  # convert to radians
  alpha <- alpha * pi / 180
  # calculate hypotenuse of the upper triangle (hypo = opposite leg (rake) /
  # sin(alpha))
  hypo <- rake / sin(alpha)
  # opposite leg (of the lower triangle) = radius - hypotenuse
  opp_leg <- dia / 2 - hypo
  # calculate trail using tangens
  opp_leg / tan(ha * pi / 180)
}

