##' Check and validate arguments for fitQ()
##'
##' Check and validate arguments for \code{\link{fitQ}} before they are used.
##'
##' This function exists so that multiple calls to \code{\link{fitQ}}
##' from \code{\link{getFeatures}} would not involve repeating the checks on essentially
##' the same arguments.
##' 
##' @param x1 The predictor of the regression model, whose length must
##' be odd, and it must be monotonic (increasing or decreasing).  Typically this
##' would be an evenly spaced, increasing vector.
##' 
##' @param min.window The minimum number of non-missing data points in a window
##' that are required to fit the regression model.
##' 
##' @param start The index of the center of the first window
##' 
##' @param skip The number of indexes to advance the center of the moving
##' window each time the model is fit.
##' 
##' @param linear.only \code{=TRUE} fits a simple linear regression model with
##' \code{x1} as the single predictor, instead of a quadratic regression model.
##' 
##' @return An object of class \code{valid_fitQ_args} that can be passed to the \code{x1} argument
##' of \code{\link{fitQ}}. It is a list with the same
##' elements that were passed into the function: \code{x1}, \code{min.window}, \code{start},
##' \code{skip}, \code{linear.only}, plus one more, the bandwidth (\code{bw}), which is equal to
##' \code{(length(x1) - 1) / 2}.
##' 
##' @author Landon Sego

check_fitQ_args <- function(x1 = -10:10,
                            min.window = 5,
                            start = 1,
                            skip = 1,
                            linear.only = FALSE) {

  # Basic checks
  stopifnot(is.numeric(x1),
            is.numeric(min.window),
            length(min.window) == 1,
            min.window %% 1 == 0,
            is.numeric(start),
            start %% 1 == 0,
            is.numeric(skip),
            length(skip) == 1,
            skip %% 1 == 0,
            skip > 0,
            is.logical(linear.only),
            length(linear.only) == 1)
  
  # Sanity checks
  if (any(is.na(x1))) {
    stop("x1 has one or more missing values\n")
  }
  if (!(length(x1) %% 2)) {
    stop("Length of x1 must be odd\n")
  }

  # Define the window bandwith (in terms of the number of data points)
  bw <- length(x1) %/% 2

  # x1 should be monotonic
  dx1 <- diff(x1)
  
  if (!(all(dx1 > 0) | all(dx1 < 0))) {
    stop("x1 is not monotonic\n")
  }

  # Checks for start and skip
  if (start > bw + 1) {
    warning("Since 'start' > (length(x1) + 1) / 2, part of the data at the beginning\n",
            "  of the series will not be covered by a window")
  }
  if (skip > 2 * bw + 1) {
    warning("Since 'skip' > the window length, parts of the data throughout\n",
            "  the series will not be covered by a window")
  }

  # Gather the output and assign a class
  out <- list(x1 = x1, min.window = min.window, start = start,
              skip = skip, linear.only = linear.only, bw = bw)

  class(out) <- c("valid_fitQ_args", class(out))

  return(out)

} # check_fitQ_args
