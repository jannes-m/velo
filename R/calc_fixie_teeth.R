#' @title Optimal teeth combination (Fixie)
#' @description \code{calc_fixie_teeth} calculates the optimal number of
#'   chainring (front) and cog (rear) teeth for a given gear ratio.
#' @param ratio The desired gear ratio given as a positive numeral.
#' @param tol Tolerance around which the specified gear ratio might deviate with
#'   0.1 as default.
#' @param front Range of the number of chainring teeth given as an integer vector.
#' @param rear Range of the number of cog teeth given as an integer vector.
#' @param nrow An integer indicating the maximal number of rows which should be
#' returned by the function.
#' @return The function returns a dataframe with following columns:
#' \enumerate{
#' \item{\strong{front} The optimal number of chainring teeth (see \code{Details}).}
#' \item{\strong{rear} The optimal number of cog teeth (see \code{Details}).}
#' \item{\strong{ratio} The actual gear ratio.}
#' \item{\strong{skid_1} The number of skid patches for single-legged skidders.
#' See \code{\link{calc_skids}} for more details.}
#' \item{\strong{skid_2} The number of skid patches for two-legged skidders.
#' See \code{\link{calc_skids}} for more details.}
#' }
#' @details The function keeps only teeth combinations resulting in gear
#'   ratios within the specified tolerance range. Secondly, it sorts the results
#'   by \code{skid_2} in descending order.
#' @author Jannes Muenchow
#' @importFrom dplyr mutate_ mutate filter_ arrange select
#' @importFrom lazyeval interp
#' @export
#' @keywords Single speed bike, Fixie
#' @examples
#' calc_fixie_teeth(2.8, 5)
calc_fixie_teeth <- function(ratio = 2.8, tol = 0.1, 
                             front = 31:59,
                             rear = 14:21,
                             nrow = 5) {
  
  # test if ratio is numeric and greater than 1
  if (ratio %% 1 == 0 | ratio <= 1) {
    stop(paste0("Function argument ratio must be positive and",
                " must not be an integer!"))
  }
  
  # test if input params are integers
  if (any(!vapply(list(front, rear), function(x) {
    isTRUE(all.equal(x %% 1, 0))
  }, logical(1)))) {
    stop("Both x and y must be integers.")
  }
  
  # expand
  d <- expand.grid("front" = front, "rear" = rear)
  # find the transmission ratio for each combination
  d <- mutate(d, ratio = front / rear)
  
  # define the condition
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
  
  if (nrow(d) == 0) {
    stop("The combination of ratio and tolerance is incompatible ",
         "with the specified number of chainring and cog teeth.")
  }
  # calculate the number of skid patches
  skids <- calc_skids(d$front, d$rear)
  
  # cbind d and skid patches
  cbind(d, skids) %>%  
  # order the result by the lowest difference
  arrange(-skid_2, abs(ratio_2)) %>%
  select(-ratio_2)
}