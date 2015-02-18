#' @title Find multiplier
#' @description Find lowest possible multiplier which yields an integer when
#'   multiplied with a numeric.
#' @param x a numeric vector
#' @importFrom stringr str_split
#' @importFrom dplyr %>%
#' @importFrom conf.design factorize
#' @examples
#' find_multiplier(2.25)
#' find_multiplier(8.245)
#' # x can also be a numeric vector of length > 1
#' find_multiplier(c(2.25, 8.245, 2.1))
#' @export

find_multiplier <- function(x) {
  if (!is.numeric(x)) {
    stop("x must be numeric!")
  }
  if (any(x <= 1)) {
    stop("x must be greater than 1")
  }

  # extract only figures after the decimal point
  x_2 <- as.character(x) %>%
    str_split(., pattern = "\\.") %>%
    # just keep the second element
    vapply(. , `[`, 2, FUN.VALUE = character(1)) %>%
    # convert to numeric
    as.numeric
  # Replace NAs, only form when the second list element was empty, i.e., there
  # is no decimal place
  x_2[is.na(x_2)] <- 0
  
  # count the number of decimal places
  dp <- as.character(x_2) %>%
    nchar

  # just keep prime factors that are also prime factors of 10, 100, 100, etc
  denom <- 10^dp
  
  # find the greatest common denominator
  my_gcd <- mapply(gcd, denom, x_2)
  
  # calculate the lowest possible value which yields an integer when 
  # multiplied with x
  mapply(`/`, denom, my_gcd)
}
