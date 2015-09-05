# This function calls the C quadratic fitting routine
# Fits the quadratic regression model to a moving window of
# a data vector.

# Landon Sego, January 30, 2008

# Added the 'linear.only' argument, Landon Sego, Dec 19, 2009

##' Fits a moving window quadratic (or simple linear) regression model
##' 
##' Fits a moving quadratic (or simple linear) regression model over a series
##' using a 2-sided window.
##'
##' This function dynamically accounts for the incomplete windows
##' which are caused by missing values and which occur at the beginning and end
##' of the series.
##' A quadratic regression model is used if \code{linear.only = FALSE}:
##' 
##' \code{y = a + b*x1 + c*x1^2 + e}
##' 
##' and a simple linear regression model is used if \code{linear.only = TRUE}:
##' 
##' \code{y = a + b*x1 + e}
##'
##' where \code{a}, \code{b}, and \code{c} are regression coefficients, and \code{e}
##' represents the residual error.  The regression model is fit repeatedly over a two-sided
##' moving window across the values of \code{y}. The middle element of \code{x1} is aligned with
##' an element of \code{y} and then the regression model coefficients are calculated for that element
##' of \code{y}.
##' Consequently, the size of the moving window is determined by
##' the the number of elements in \code{x1}. For example, if \code{x1 = -3:3}, the size
##' of the moving window is 7. 
##'
##' For the quadratic model, \code{fitQ} rapidly calculates the least squares estimates by centering the
##' predictors so that they are orthogonal (or nearly orthogonal, if data in
##' the window are missing).  Then the orthogonal coefficients are
##' 'backtransformed' to the original, non-orthogonal parameterization.
##' Centering and orthogonality are not required to quickly fit the simple
##' linear regression model.
##' 
##' The handling of the moving windows and missing values in this function is
##' very similar to \code{\link[Smisc:smartFilter]{Smisc::smartFilter}}.  
##'
##' Instead of a numeric vector, the \code{x1} argument can be a
##' \code{valid_fitQ_args} object (returned by \code{\link{check_fitQ_args}}), in which
##' case all the subsequent arguments to \code{fitQ} are ignored
##' (because the \code{valid_fitQ_args} object contains all those arguments). This is useful
##' because \code{fitQ} is called repeatedly by \code{\link{getFeatures}} over the same set of
##' argument values.
##' 
##' @export
##'
##' @param y A numeric vector
##' 
##' @inheritParams check_fitQ_args
##' 
##' @return An object of class \code{fitQ}, which is list with
##' the 4 vectors that contain the results of the
##' quadratic model fits:
##' \item{a}{The estimated intercepts}
##' \item{b}{The estimated linear coefficients}
##' \item{c}{The estimated quadratic coefficients. These are \code{NA} is
##'          \code{linear.only = TRUE}}
##' \item{d}{The root mean squared error (RMSE)}
##' The first element of the vectors \code{a}, \code{b}, \code{c}, and \code{d} contains the model
##' coefficients corresponding to the first data point in \code{y}.  The second element in
##' the vectors \code{a}, \code{b}, \code{c}, and \code{d} contains the model
##' coefficients corresponding to the second data point in \code{y}, etc.
##' 
##' @author Landon Sego
##' 
##' @examples
##'# Calculate the rolling window quadratic fit
##'z <- fitQ(rnorm(25), -3:3)
##'z
##'summary(z)
##'
##'# Or we can request our own summary stats
##'summary(z, stats = c("count", "mean", "kurt"))
##'

## Text and examples for the Appendix of the vignette

## # Now let's illustrate in greater detail how \code{fitQ} behaves:
## y <- rnorm(10)
## x1 <- -3:3
## x2 <- x1^2
## f <- fitQ(y, x1)
## f

## # A little function to extract out the same pieces from the \code{lm} object that
## # we wish to compare to \code{f}, the \code{fitQ} object
## extract.lm <- function(lmObject) {
    
##   x <- c(coef(lmObject), summary(lmObject)$sigma)
##   names(x) <- letters[1:4]

##   return(x)
  
## }

## # A little function for extracting the i'th element of each of the vectors in
## # a \code{fitQ} object, to compare to the output of \code{extract.lm()}:
## extract.fitQ <- function(fitQobject, i) {

##    return(unlist(lapply(fitQobject, function(x) x[i])))
    
## }

## # For the first element in \code{y}, the regression model that would be fit would be
## extract.lm(lm(y[1:4] ~ x1[4:7] + x2[4:7]))

## # Note how the middle point of \code{x1}, which, in this case, is \code{x[4] = 0},
## # is aligned with \code{y[1]}.
## # However, the first element in the vectors of \code{f} are \code{NA}:
## extract.fitQ(f, 1)

## # This is because the default value of \code{min.window = 5} requires
## # there be at least 5 non-missing data points to fit the regression model, whereas there were only 4 in
## # this case.  For the second element of \code{y}, the regression model is constructed
## # by aligning the middle point of \code{x1} with the second element of \code{y}: 
## extract.lm(lm(y[1:5] ~ x1[3:7] + x2[3:7]))

## # And the second vector elements from \code{fitQ()} match the results from the \code{lm()} fit:
## extract.fitQ(f, 2)

## # Here's what we see for the third element in \code{y}:
## extract.lm(lm(y[1:6] ~ x1[2:7] + x2[2:7]))
## extract.fitQ(f, 3)

## # Once we get to the fourth element, we are able to fit a model with the whole window
## # of seven elements (because \code{x1} has seven elements):
## extract.lm(lm(y[1:7] ~ x1 + x2))
## extract.fitQ(f, 4)

## # And all subsequent elements in the body of \code{y} use the whole window as well,
## extract.lm(lm(y[2:8] ~ x1 + x2))
## extract.fitQ(f, 5)

## # Until we get to the end of \code{y}, in which case the the last elements of \code{x1} start
## # dropping off.  Here's the fit for the 8th element of \code{y}:
## extract.lm(lm(y[5:10] ~ x1[1:6] + x2[1:6]))
## extract.fitQ(f, 8)

## # And the 9th element of \code{y}:
## extract.lm(lm(y[6:10] ~ x1[1:5] + x2[1:5]))
## extract.fitQ(f, 9)

## # An illustration how how \code{fitQ()} handles \code{NA}'s is also in order.  Let's insert an
## # \code{NA} into \code{y}:
## y[3] <- NA
## f <- fitQ(y, x1)
## f

## # The first elements of the \code{fitQ} object are \code{NA} because there are only 3 non-missing
## # data points in the window for the first element:
## sum(!is.na(y[1:4]))

## # The second elements
## # are \code{NA} because there are only 4 non-missing data points in the window that is centered on the
## # second element of y:
## sum(!is.na(y[1:5]))

## # However, for the third element of y, we now have 5 non-missing data points, which is the default
## # value for \code{min.window}, the minimum allowable number of non-missing values:
## sum(!is.na(y[1:6]))

## # And we see that the models agree
## extract.lm(lm(y[1:6] ~ x1[2:7] + x2[2:7]))
## extract.fitQ(f, 3)

fitQ <- function(y,
                 x1 = -10:10,
                 min.window = 5,
                 start = 1,
                 skip = 1,
                 linear.only = FALSE) {
    
  # Check whether x1 is a 'valid_fitQ_args' object.  If not, check the args
  if (inherits(x1, "valid_fitQ_args")) {
    a <- x1
  }
  else {
    a <- check_fitQ_args(x1 = x1, min.window = min.window, start = start,
                         skip = skip, linear.only = linear.only)
  }

  # Checks on y that need to always be done
  stopifnot(is.numeric(y),
            is.vector(y),
            length(y) > 3)
  
  # Number of elements in y
  n <- length(y)

  # Checks that depend on y
  if (a$start > n) {
    stop("'start' must be <= 'length(y)'")
  }

  if (n < a$min.window) {
    stop("'length(y)' must be >= 'min.window'")
  }
  
  # Index of window centers
  win.centers <- seq(a$start, n, by = a$skip)
  num.windows <- length(win.centers)

  # Quadratic fit
  if (!a$linear.only) {

    # Center the x1
    mx1 <- mean(a$x1)
    x1.c <- a$x1 - mx1
    
    # Create quadratic predictor variable and center it
    x2 <- x1.c ^ 2
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
              as.double(a$x1),
              as.double(x1.c),
              as.double(x2.c),
              as.double(mx1),
              as.double(mx2),
              as.integer(orthog),
              as.integer(a$bw),
              as.integer(a$min.window),
              as.integer(win.centers - 1),
              as.integer(num.windows),
              a = double(num.windows),
              b = double(num.windows),
              c = double(num.windows),
              d = double(num.windows),
              NAOK = TRUE)[letters[1:4]]

  }

  # Linear fit
  else {
    
    out1 <- .C("fitL",
               as.double(y),
               as.integer(!is.na(y)),
               as.integer(n),
               as.double(a$x1),
               as.double(mean(a$x1)),
               as.double(t(a$x1) %*% a$x1),
               as.integer(a$bw),
               as.integer(a$min.window),
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
##' @param object An object of class \code{fit} returned by \code{\link{fitQ}}.
##' 
##' @param \dots Additional (unused) arguments required for consistency of S3 methods
##' 
##' @param stats A character vector of summary statistics that are valid for
##' \code{\link{summaryStats}}. Alternatively, the object returned by
##' \code{\link{summaryStats}} may also be supplied for this argument.
##'
##' @export

# Summary method for fitQ
summary.fitQ <- function(object, ...,
                         stats = c("min", "q1", "mean", "med", "q3",
                                   "max", "sd", "count")) {

  # Create the summary stat function, unless it has been passed in
  if (!inherits(stats, "summaryStats_function")) {
    stats <- summaryStats(stats)
  }

  # Calculate the summary statistics
  out <- unlist(lapply(object, stats))

  return(out)

} # summary.fitQ
