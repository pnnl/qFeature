##' Adaptations of common summary functions to return NA if all elements are NA
##' 
##' Adaptations of common summary statistical functions
##' to return \code{NA} if all elements of \code{x} are \code{NA}.  The prefix 'sp' could
##' be thought of as 'special'. These are internal functions used by the qFeature package.
##'
## @export sp.min sp.max sp.sum sp.count 
##' @rdname utility_functions
##' @aliases sp.min sp.max sp.sum sp.count sp.mean sp.skewness sp.kurtosis
##'
##' @usage
##' sp.min(x)
##' sp.max(x)
##' sp.sum(x)
##' sp.count(x)
##' sp.mean(x)
##' sp.skewness(x)
##' sp.kurtosis(x)
##' 
##' @param x Numeric vector
##' @return \code{sp.min}, \code{sp.max}, \code{sp.sum} operate just like
##' \code{\link{min}}, \code{\link{max}}, and \code{\link{sum}} except they
##' return \code{NA} if all elements of \code{x} are \code{NA}.
##' 
##' \code{sp.count} returns the number of non-missing values in the vector.
##' However, if all values are missing, it returns \code{NA}.
##' @author Landon Sego
##' @keywords misc
## @examples
## min(NA, na.rm=TRUE)
## sp.min(NA)
##
## max(NA, na.rm=TRUE)
## sp.max(NA)
##
## sum(NA, na.rm=TRUE)
## sp.sum(NA)
##
## sp.count(c(1, 3, NA, 4))
## sp.count(rep(NA, 3))


# Landon Sego, January 30, 2008

sp.min <- function(x) {
  if (!all(is.na(x)))
    return(min(x, na.rm=TRUE))
  else
    return(NA)
}

sp.max <- function(x) {
  if (!all(is.na(x)))
    return(max(x, na.rm=TRUE))
  else
    return(NA)
}

sp.sum <- function(x) {
  if (!all(is.na(x)))
    return(sum(x, na.rm=TRUE))
  else
    return(NA)
}

sp.mean <- function(x) {
  if (!all(is.na(x)))
    return(mean(x, na.rm=TRUE))
  else
    return(NA)
}

sp.count <- function(x) {
  if (ct <- sum(!is.na(x)))
    return(ct)
  else
    return(NA)
}

sp.skewness <- function(x) {
  if (!all(is.na(x)))
    return(moments::skewness(x, na.rm=TRUE))
  else
    return(NA)
}

sp.kurtosis <- function(x) {
  if (!all(is.na(x)))
    return(moments::kurtosis(x, na.rm=TRUE))
  else
    return(NA)
}

