#' @title Find multiplier
#' @description Find lowest possible multiplier which yields an integer when
#'   multiplied with a numeric.
#' param x a numeric vector
#' importFrom stringr str_split
#' importFrom dplyr %>%
#' importFrom conf.design factorize
#' @examples
#' find_multiplier(2.25)
#' find_multiplier(8.245)
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
  
  # prime factor decomposition
  p <- factorize(x_2)
  # just keep prime factors that are also prime factors of 10, 100, 100, etc
  denom <- factorize(10^dp)
  
  # make sure denom and p are lists, this is not the default,
  # if x is of length 1  
    if (!is.list(denom)) {
      denom <- list(denom)
    }
  if (!is.list(p)) {
    p <- list(p)
  }
  
  # find the lowest possible integer
  vapply(seq_along(denom), function(i) {
    # find prime factors which p and denom have in common
    tmp <- unlist(lapply(c(denom[i], p[i]), unique))
    int <- unique(tmp[duplicated(tmp)])
    # calculate the lowest possible value which yields an integer when 
    # multiplied with x
    if (p[[i]][1] != 0) {
      prod(denom[[i]]) / prod(int, na.rm = TRUE)
    } else {
      1
    }    
  }, numeric(1))   
}
 

#' @title Find smallest equivalent whole number ratio
#' @description Simplify the gear ratio to the smallest whole number ratio.
#' @param front The number of chainring teeth.
#' @param  rear The number of cog teeths.
#' @return The function returns a vector indicating the number of skid patches
#'   for single-legged skidders.
#' @details The function was inspired by
#'   \link{http://www.bikecalc.com/skid_patch_math}.
#' importFrom conf.design factorize

find_wnr <- function(front, rear) {
  
  if (length(front) != length(rear)) {
    stop("x and y must have the same length!")
  }
  
  fac_1 <- factorize(front)
  fac_2 <- factorize(rear)
  # make sure that both factors are lists, this is not the default,
  # if x any y are of length 1  
  if (!is.list(fac_1)) {
    fac_1 <- list(fac_1)
  }
  if (!is.list(fac_2)) {
    fac_2 <- list(fac_2)
  }
  
  # calculate the number of skid patches
  vapply(seq_along(fac_1), function(i) {
    # find prime factors which front and rear have in common
    tmp <- unlist(lapply(c(fac_1[i], fac_2[i]), unique))
    int <- unique(tmp[duplicated(tmp)])
    rear[[i]] / prod(int)
  }, FUN.VALUE = numeric(1))
}
