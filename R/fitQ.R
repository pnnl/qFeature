# This function calls the C quadratic fitting routine
# Fits the quadratic regression model to a moving window of
# a data vector.

# Landon Sego, January 30, 2008

# Added the 'linear.only' argument, Landon Sego, Dec 19, 2009

##' Fits a moving window quadratic (or simple linear) regression model
##' 
##' Fits a moving quadratic (or simple linear) regression model over a series
##' using a 2-sided window.  It dynamically accounts for the incomplete windows
##' which are caused by missing values and which occur at the beginning and end
##' of the series.
##' 
##' Fits the model
##' 
##' \code{y = a + b*x1 + c*x1^2 + e}
##' 
##' if \code{linear.only = FALSE}, or, if \code{linear.only = TRUE}, fits
##' 
##' \code{y = a + b*x1 + e}
##' 
##' The model is fit repeatedly over a moving window. For the quadratic model,
##' it rapidly calculates the least squares estimates by centering the
##' predictors so that they are orthogonal (or nearly orthogonal, if data in
##' the window are missing).  Then the orthogonal coefficients are
##' 'backtransformed' to the original, non-orthogonal parameterization.
##' Centering and orthogonality are not required to quickly fit the simple
##' linear regression model.
##' 
##' The handling of the moving windows and missing values in this function is
##' very similar to \code{\link[Smisc]{smartFilter}} in \pkg{Smisc}.
##'
##' @export
##' 
##' @param y A numeric vector
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
##' @return An object of class \code{fitQ}, which is list with
##' the 4 component vectors that contain the results of the
##' quadratic model fits.
##' \item{a}{The estimated intercepts}
##' \item{b}{The estimated linear coefficients}
##' \item{c}{The estimated quadratic coefficients. These are \code{NA} is
##'          \code{linear.only = TRUE}}
##' \item{d}{The root mean squared error (RMSE)}
##' 
##' @author Landon Sego
##' 
##' @examples
##'# Calculate the rolling window quadratic fit
##'y <- fitQ(rnorm(25), -3:3)
##'y
##'summary(y)
##'
##'# Define our own summary stats
##'summary(y, stats = c("count", "mean", "kurt"))

fitQ <- function(y,
                 x1 = -10:10,
                 min.window = 5,
                 start = 1,
                 skip = 1,
                 linear.only = FALSE,
                 checkArgs = TRUE) {
    
#### Consolidating the checks here...
    
  n <- length(y)

  # Basic checks
  stopifnot(is.numeric(y),
            is.numeric(x1),
            is.numeric(min.window),
            length(min.window) == 1,
            min.window %% 1 == 0,
            is.numeric(skip),
            length(skip) == 1,
            skip %% 1 == 0,
            is.logical(linear.only))
    
  # Define the window bandwith (in terms of the number of data points)
  bw <- length(x1) %/% 2

  # Sanity checks
  if (any(is.na(x1)))
    stop("x1 has one or more missing values\n")
  if (!(length(x1) %% 2))
    stop("Length of x1 must be odd\n")
  
  # x1 should be monotonic
  dx1 <- diff(x1)
  
  if (!(all(dx1 > 0) | all(dx1 < 0)))
    stop("x1 is not monotonic\n")
  if (start > n)
    stop("'start' must be <= 'length(y)'\n")
  if (skip <= 0)
    stop("'skip' must be > 0\n")
  if (start > bw + 1)
    warning("Since 'start' > (bandwidth + 1), part of the data at the beginning\n",
            "  of the series will not be covered by a window\n")
  if (skip > 2 * bw + 1)
    warning("Since 'skip' > the window length, parts of the data throughout\n",
            "  the series will not be covered by a window\n")
  
  # Index of window centers
  win.centers <- seq(start, n, by = skip)
  num.windows <- length(win.centers)

  if (!linear.only) {

    # Center the x1
    mx1 <- mean(x1)
    x1.c <- x1 - mx1
    
    # Create quadratic predictor variable and center it
    x2 <- x1.c^2
    mx2 <- mean(x2)
    x2.c <- x2 - mx2
  
    # Determine whether x1 and x2 are orthogonal
    orthog <- round(t(x1.c) %*% x2.c, 10) == 0
  
    # Call the C 'fitQ' method that fits the quadratic model
    # over the entire data vector and return a list of the a's, b's, c's, and d's.
    out <- .C("fitQ",
              as.double(y),
              as.integer(!is.na(y)),
              as.integer(n),
              as.double(x1),
              as.double(x1.c),
              as.double(x2.c),
              as.double(mx1),
              as.double(mx2),
              as.integer(orthog),
              as.integer(bw),
              as.integer(min.window),
              as.integer(win.centers - 1),
              as.integer(num.windows),
              a = double(num.windows),
              b = double(num.windows),
              c = double(num.windows),
              d = double(num.windows),
              NAOK = TRUE)[letters[1:4]]

  }

  else {
    
    out1 <- .C("fitL",
               as.double(y),
               as.integer(!is.na(y)),
               as.integer(n),
               as.double(x1),
               as.double(mean(x1)),
               as.double(t(x1) %*% x1),
               as.integer(bw),
               as.integer(min.window),
               as.integer(win.centers - 1),
               as.integer(num.windows),
               a = double(num.windows),
               b = double(num.windows),
               d = double(num.windows),
               NAOK = TRUE)[c("a", "b", "d")]

    out <- list(a = out1$a, b = out1$b, c = rep(NA, num.windows), d = out1$d)
 
  }

  # Give it a class
  class(out) <- c("fitQ", class(out))
  
  return(out)
  
} # fitQ

##' @method summary fitQ
##'
##' @describeIn fitQ Calculates summary statistics for a \code{fitQ} object, returning a
##' named vector with summary statistics.  The names take the form [coefficient].[stat],
##' where the coefficient is one of "a", "b", "c", or "d", and the stat is the statistic
##' required in the \code{stats} argument of the \code{summary} method.
##' 
##' @param stats A character vector of summary statistics that are valid for
##' \code{\link{summaryStats}}. Alternatively, the object returned by
##' \code{\link{summaryStats}} may also be supplied for this argument.
##'
##' @export

# Summary method for fitQ
summary.fitQ <- function(fitQ_object,
                         stats = c("min", "q1", "mean", "med", "q3",
                                   "max", "sd", "count")) {

  # Create the summary stat function, unless it has been passed in
  if (!is.function(stats)) {
    stats <- summaryStats(stats)
  }

  # Calculate the summary statistics
  out <- unlist(lapply(fitQ_object, stats))

  return(out)

} # summary.fitQ
