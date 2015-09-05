# This function is a simple, slow version of fitQ, which can be used to test fitQ().

# Landon Sego, 21 Dec 2009


##' Fits the moving window quadratic (or simple linear) regression model
##' 
##' A very slow version of \code{\link{fitQ}} that was coded independently in
##' order to check \code{\link{fitQ}}.  It is identical in every respect to
##' \code{\link{fitQ}} except that it uses a slower algorithm that uses \code{\link{lm}}
##' for the model fit.
##'
##' This function is for testing and validation only and is not exported.
##'
##' @param y A numeric vector
##' @param x1 The linear predictor of a the regression model, whose length must
##' be odd, and it must be monotonic (increasing or decreasing).
##' @param min.window The minimum number of non-missing data points in a window
##' that are required to fit the regression model.
##' @param start The index of the center of the first window
##' @param skip The number of indexes to advance the center of the moving
##' window each time the model is fit.
##' @param linear.only \code{=TRUE} fits a simple linear regression model with
##' \code{x1} as the single predictor, instead of a quadratic regression model.
##' @return A matrix with the 4 columns that contain the results of the
##' quadratic model fits:  \item{a}{The estimated intercepts} \item{b}{The
##' estimated linear coefficients} \item{c}{The estimated quadratic
##' coefficients.  These are \code{NA} is \code{linear.only=TRUE}} \item{d}{The
##' root mean squared error (RMSE)}
##'
##' @author Landon Sego
##' @keywords misc
fitQslow <- function(y, x1, min.window = 5, start = 1, skip = 1, linear.only = FALSE) {

  # y is the data vector
  # bw is the bandwidth

  bw <- length(x1) %/% 2
  n <- length(y)
  nf <- length(x1)

  # Set -9999 to NA
  y[round(y,14) == -9999] <- NA
  
  # Sanity checks
  if (bw < 1)
    stop("The bandwidth must be >= 1\n");
  if (min.window < 3)
    stop("The minimum number of non-missing data points in the window\n  must be >= 3 (since >= 3 points are required to fit a quadratic model)\n)");
  if (min.window > length(x1))
    stop("The minimum number of non-missing data points in the window\n  must be <= window length (2*bandwidth + 1)\n");
  if (length(y) < min.window)
    stop("The length of the data vector must be >= minimum number of\n  non-missing data points in the window\n");
  if (start > bw + 1)
    warning("Since 'start' > (bandwidth + 1), part of the data at the beginning\n",
            "  of the series will not be covered by a window\n")
  if (skip > 2 * bw + 1)
    warning("Since 'skip' > the window length, parts of the data throughout\n",
            "  the series will not be covered by a window\n")
  if (any(is.na(x1)))
    stop("x1 has one or more missing values\n")
  if (!(length(x1) %% 2))
    stop("Length of x1 must be odd\n")
  if (start > n)
    stop("'start' must be <= 'length(y)'\n")
  if (skip <= 0)
    stop("'skip' must be > 0\n")

  
  # Create predictor variables
  x2 <- x1^2

  lmFit <- function(y) {

    fitLM <- TRUE

    not.na.Y <- !is.na(y)

    # If any are missing
    if (!all(not.na.Y)) {
      # Note, if all are missing, the conditional statement below returns
      # FALSE & NA which is equal to FALSE

      # Find the indexes of the first and last non-missing values
      first.non.missing <- which(not.na.Y)[1]
      last.non.missing <- which(rev(not.na.Y))[1]
      last.non.missing <- c(nf:1)[last.non.missing]

      # The following 3 conditions are being checked:
      # 1. Num of nonNA is at least as big as the min window size
      # 2. First nonNA occurs on or before center point of window
      # 3. Last nonNA occurs on or after center point of window
      if (!((sum(not.na.Y) >= min.window) &
            (first.non.missing <= bw + 1) &
            (last.non.missing  >= bw + 1))) 
        fitLM <- FALSE
    }

    if (fitLM) {

      if (!linear.only) {
        m1 <- stats::lm(y ~ x1 + x2)
        minNum <- 3
      }
      else {
        m1 <- stats::lm(y ~ x1)
        minNum <- 2
      }
      if (sum(not.na.Y) > minNum)
        d <- sqrt(resid(m1) %*% resid(m1) / (sum(not.na.Y) - minNum))
      else
        d <- 0

      if (!linear.only)
        out <- c(coef(m1),d)
      else
        out <- c(coef(m1), NA, d)
      
      names(out) <- NULL
      return(out)
    }
    else
      return(rep(NA,4))

  } # lmFit

  # Create the response matrix, selecting the columns that correspond to the
  # windows we want to fit
  win.centers <- seq(start, length(y), by=skip)
  resp.mat <- make.response.mat(y, bw=bw)[,win.centers]

  if (length(win.centers)==1)
    resp.mat <- matrix(resp.mat,ncol=1)

  # Apply the lmFit function over the response matrix
  fits <- t(apply(resp.mat, 2, lmFit))
  colnames(fits) <- letters[1:4]
  rownames(fits) <- 1:NROW(fits)

  return(fits)
  
} # fitQslow

