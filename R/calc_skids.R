#' @title Skid patches
#' @description  The function \code{calc_skids} calculates the number of skid 
#'   patches for both single-legged and two-legged skidders for specified 
#'   combinations of chainring- and cog-teeth.
#' @param front The number of chainring teeth.
#' @param  rear The number of cog teeths.
#' @return The function returns a \code{data.frame} with two columns:
#' \enumerate{
#'  \item{\strong{skid_1} The number of skid patches for single-legged
#'  skidders.}
#'  \item{\strong{skid_2} The number of skid patches for ambidextrous skidders 
#'  (two-legged skidders).}
#' } 
#' @details The function was inspired by 
#'   \url{http://www.bikecalc.com/skid_patch_math}. Basically, the function
#'   divides the cog by the the least common denominator of cog and chainring
#'   teeth. This corresponds to the number of skid patches for single-legged
#'   skidders. As an extra, the function also calculates the number of skid
#'   patches for ambidextrous skidders.
#' @importFrom schoolmath  gcd prime.factor
#' @importFrom dplyr mutate %>% select
#' @export
#' @examples
#' calc_skids(54, 14)
#' # you can also specify vectors of length > 1
#' calc_skids(front = 48:54,
#'            rear = 12:18)

calc_skids <- function(front, rear) {
  
  if (length(front) != length(rear)) {
    stop("x and y must have the same length!")
  }
  
  # find prime factors which front and rear have in common, i.e. find the
  # greatest common divisor (gcd is rather slow, maybe we should enhance its
  # performance some day...)
  my_gcd <- mapply(gcd, front, rear)
  
  # Calculate skid patches, i.e. divide rear by the greatest common divisor,
  # which in fact yields the least common denominator
  d <- data.frame(skid_1 = rear / my_gcd,
                  num = front /my_gcd)
  
  # calculate ambidextrous skid patches
  # if the numerator of the simplified gear ratio is odd 
  # -> denominator times 2
  # it the numerator of the simplified gear ratio is even
  # -> still the denominator
  mutate(d, skid_2 = 
           # test if the numerator is odd
           ifelse(num %% 2 == 1,
                  yes = skid_1 * 2,
                  no = skid_1)) %>%
    select(-num)
}
