#' @title Front and rear gear teeth (Fixie).
#' @description \code{calc_fixie_teeth} calculates the optimal number of front
#' and rear gear teeth for a given gear ratio/transmission.
#' @param ratio The desired gear ratio given as a positive numeral.
#' @param nrow An integer indicating the maximal number of rows which should be
#' returned by the function.
#' @details The function assumes that the chainring (front sprocket) varies in
#'   size from 20 teeth to 59 and the cog varies from 14 teeth to 21 teeth.
#' @return The function returns a dataframe with following columns:
#' \enumerate{
#' \item{\strong{front} The optimal number of chainring teeth.}
#' \item{\strong{rear} The optimal number of cog teeth.}
#' \item{\strong{ratio} The gear ratio.}
#' \item{\strong{skid_1} The number of skid patches for single-legged skidders.
#' See \code{\link{calc_skids}} for more details.}
#' \item{\strong{skid_2} The number of skid patches for two-legged skidders.
#' See \code{\link{calc_skids}} for more details.}
#' }
#' The function displays all possible combinations in descending order which
#' range +/- 0.1 around the desired gear ratio.
#' @author Jannes Muenchow
#' @importFrom dplyr mutate_ mutate filter_ arrange select
#' @importFrom lazyeval interp
#' @export
#' @keywords Single speed bike, Fixie
#' @examples
#' calc_fixie_teeth(2.8, 5)
calc_fixie_teeth <- function(ratio = 2.8, nrow = 5) {
  if (ratio %% 1 == 0 | ratio <= 2) {
    snrow(paste0("Function argument ratio must be positive and",
                " must not be an integer!"))
  }
  # range of the number of teeth of the front sprocket
  front <- 31:59
  # range of the number of teeth of the rear sprocket
  rear <- 14:21
  # define the tolerance
  tol <- 0.1
  # expand
  d <- expand.grid("front" = front, "rear" = rear)
  # find the transmission ratio for each combination
  d <- mutate(d, ratio = front / rear)
  # build the condition
  cond <- interp(~ ratio >= min & ratio <= max,
                 min = ratio - tol, max = ratio + tol)
  # subset according to the condition
  d <- filter_(d, cond) %>%
    # round your gear transmission to the third decimal place
    mutate(ratio = round(ratio, 3)) %>%
    # subtract the desired ratio from your output
    mutate_(ratio_2 = interp(~ratio - opt, opt = ratio)) %>%
    # select only the specified number of rows
    head(., nrow)  
  
  # calculate the number of skid patches
  skids <- calc_skids(d$front, d$rear)
  
  # cbind d and skid patches
  cbind(d, skids) %>%  
  # order the result by the lowest difference
  arrange(-skid_2, abs(ratio_2)) %>%
  select(-ratio_2)
}