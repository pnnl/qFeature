##' Create a function that calculates the summary statistics
##'
##' Create a customized function that calculates desired summary statistics for a vector
##' of numerical input
##'
##' The acceptable values of \code{stats} are
##' \describe{
##'  \item{"min"}{minimum}
##'  \item{"q1"}{first quartile (25th percentile)}
##'  \item{"mean"}{mean}
##'  \item{"med"}{median (50th percentile)}
##'  \item{"q3"}{third quartile (75th percentile)}
##'  \item{"max"}{maximum}
##'  \item{"sd"}{standard deviation}
##'  \item{"sum"}{sum}
##'  \item{"ss"}{sum of squares}
##'  \item{"count"}{number of non-missing elements in the vector}
##'  \item{"skew}{skewness}
##'  \item{"kurt"}{kurtosis (Pearson's measure)}
##' }
##' 
##' All statistics are calcuated such that if there are too many \code{NAs}, an \code{NA}
##' is returned.  The 'moments' package namespace is loaded if \code{skew} or \code{kurt}
##' are requested.
##' 
##' @export
##' 
##' @param stats A character vector of summary statistics.  See details for acceptable
##' values.
##'
##' @return A function that takes a single argument of a numerical vector, and returns
##' a named vector of the summary statistics of that vector
##'
##' @author Landon Sego
##' 
##' @examples
##'f <- summaryStats(c("q3","q1","mean","count","med"))
##'f
##'f(rnorm(200))

summaryStats <- function(stats) {

  # Check stats
  validStats <- c("min", "q1", "mean", "med", "q3",
                  "max", "sd", "sum", "ss", "count", "skew", "kurt")

  if (!(all(stats %in% validStats))) {
    stop("Valid arguments for 'stats' are '", paste(validStats, collapse = "', '"), "'")
  }

  # Load the moments namespace if "skew" or "kurt" are requested
  if (any(c("skew", "kurt") %in% stats)) {

    if (!requireNamespace("moments")) {
      stop("The 'momments' package must be installed to compute skewness ",
           "('skew') or kurtosis ('kurt')")
    }
  }
  
  # Create stat elements
  sw <- function(x) {
    switch(x,
           "min"   = "\n  min = sp.min(x),",
           "q1"    = "\n  q1 = stats::quantile(x, 0.25, na.rm = TRUE)[[1]],",
           "mean"  = "\n  mean = sp.mean(x),",
           "med"   = "\n  med = stats::median(x, na.rm = TRUE),",
           "q3"    = "\n  q3 = stats::quantile(x, 0.75, na.rm = TRUE)[[1]],",
           "max"   = "\n  max = sp.max(x),",
           "sd"    = "\n  sd = stats::sd(x, na.rm = TRUE),",
           "sum"   = "\n  sum = sp.sum(x),",
           "ss"    = "\n  ss = sp.sum(x^2),",
           "count" = "\n  count = sp.count(x),",
           "skew"  = "\n  skew = sp.skewness(x),",
           "kurt"  = "\n  kurt = sp.kurtosis(x),")
  } # sw
  

  # Create the function to calculate the named vector of summary stats
  fText <- paste("function(x) { c(",
                 paste(unlist(lapply(stats, sw)), collapse = ""),
                 ")\n}", sep = "")

  # Remove the final comma
  fText <- sub("),)\n}",  "))\n}", fText, fixed = TRUE)

  # Create the function
  return(eval(parse(text = fText)))
  
} # summaryStats

