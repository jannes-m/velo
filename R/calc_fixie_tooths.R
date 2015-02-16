#' @title Front and rear gear tooths (Fixie).
#' @description \code{calc_fixie_tooths} calculates the optimal number of front
#'   and rear gear tooths for a given gear ratio/transmission.
#' @param ratio The desired gear ratio given as a positive numeral.
#' @param tol Tolerance indicating the range around which the desired gear ratio
#'   might deviate (default: 0).
#' @details The function assumes that the front sprocket has between 31 and 62
#'   tooths, and the rear sprocket between 14 and 21.
#' @return The function returns a dataframe with the ten best results in 
#'   descending order.
#' @author Jannes Muenchow
#' @importFrom dplyr mutate_ filter_ arrange select
#' @export 
#' @keywords Single speed bike, Fixie
#' @examples
#' calc_fixie_tooths(2.8, 0.1)

calc_fixie_tooths <- function(ratio = 2.8, tol = 0) {
  if (ratio %% 1 == 0 | ratio <= 2) {
    stop(paste0("Function argument ratio must be positive and", 
                " must not be an integer!"))
  }
  # range of the number of tooths of the front sprocket
  front <- 31:62
  # range of the number of tooths of the rear sprocket
  rear <- 14:23
  # expand
  d <- expand.grid("front" = front, "rear" = rear)
  # find the transmission ratio for each combination
  d <- mutate(d, ratio = front / rear)
  # build the condition
  cond <- interp(~ ratio >= min & ratio <= max, 
                 min = ratio - tol, max = ratio + tol)
  
  # subset according to the condition
  filter_(d, cond) %>%
    # round your gear transmission to the third decimal place
    mutate(ratio = round(ratio, 3)) %>%
    # subtract the desired ratio from your output
    mutate_(ratio_2 = interp(~ratio - opt, opt = ratio)) %>%
    # and order the result by the lowest difference
    arrange(abs(ratio_2)) %>%
    select(-ratio_2) %>%
    # display only the first ten rows
    head(., 10)
}
