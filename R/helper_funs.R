#' @title Greatest common divisor
#' @description Find the greatest common divisor of two integers.
#' @param x an integer of length 1
#' @param y an integer of length 1
#' @importFrom conf.design factorize
#' @importFrom dplyr group_by filter summarise
#' @author Jannes Muenchow
#' @examples
#' find_gcd(45, 15)
#' find_gcd(1289, 124)
#' @export
find_gcd <- function(x = NULL, y = NULL) {
  
  if (any(vapply(list(x, y), is.null, logical(1)))) {
    stop("Please specify x and y.")
  }
  # test if input params are integers
  if (any(!vapply(list(x, y), function(x) {
    isTRUE(all.equal(x %% 1, 0))
  }, logical(1)))) {
    stop("Both x and y must be integers.")
  }
  
  # prime factor decomposition
  # add 1 to avoid that there are no common prime factors
  x <- c(1, factorize(x))
  y <- c(1, factorize(y))
  # find all elements shared by both vectors
  sub <-unique(x[x %in% y])
  # how often does each element occur in each vector
  d <- rbind(data.frame(table(y[y %in% sub])),
             data.frame(table(x[x %in% sub])))
  d <- group_by(d, Var1) %>%
    # take the minimum frequency (least common number of occurrences)
    filter(Freq == min(Freq)) %>%
    # if several groups share the same minimum take the first occurrence
    summarise(Freq = first(Freq))
  # convert to a vector
  facs <- as.numeric(as.character(rep(d$Var1, d$Freq)))
  # calculate the greatest common divisor
  prod(facs)
}


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
    stop("x must be greater than 1!")
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
  my_gcd <- mapply(find_gcd, denom, x_2)
  
  # calculate the lowest possible value which yields an integer when 
  # multiplied with x
  mapply(`/`, denom, my_gcd)
}
