# This function  calculates the continuous IVT using lm(). It runs very slowly.
# It provides a standard of comparison to calcIVTc()

# demonstrated that calcIVTslow() and calcIVTc() were equivalent on January 30, 2008
# using 'r22_final_comparison_of_slow_vs_fast_continuous_IVT.R'

# Landon Sego, January 30, 2008

##' Fits the moving window quadratic regression model and summarizes results by
##' phases
##' 
##' A very slow version of \code{\link{calcIVTc}} that was coded independently
##' in order to check \code{\link{calcIVTc}}.  It is identical in every respect
##' to \code{\link{calcIVTc}} except that it uses a slower algorithm
##' that uses \code{\link{lm}} to fit the linear model across the window and it
##' returns the labeled IVT vector in a different order.
##'
##' A test that verified that calcIVTslow() and calcIVTc() were equivalent was performred January 30, 2008
##' 
##' @param y Vector of data for a particular variable for the whole flight
##' @param phase.vec Phase vector that indicates the phases of the flight (must
##' have same length as \code{y} with elements that are in sequentially
##' increasing order)
##' @param x1 See \code{\link{fitQ}}
##' @param min.window See \code{\link{fitQ}}
##' @param start See \code{\link{fitQ}}
##' @param skip See \code{\link{fitQ}}
##' @param actual.min.max if \code{TRUE}, the actual minimum and maximum data
##' values within the phase are reported for \code{a.min} and \code{a.max}.  If
##' \code{FALSE}, The minimum and maximum values of the calculated intercepts
##' within the phase are reported for \code{a.min} and \code{a.max}.
##' @param linear.only See \code{\link{fitQ}}
##' @return A named vector containing the coefficients of the regression fits
##' summarized by phase.  The names of the vector have the following general
##' format: \code{p.PHASE.COEFFICIENT.STATISTIC}, as illustrated in the
##' examples below: \item{p.1.a.min}{The minimum intercept (or minimum data
##' value if \code{actual.min.max=TRUE}) in phase 1} \item{p.4.a.max}{The
##' maximum intercept (or maximum data value if \code{actual.min.max=TRUE}) in
##' phase 4} \item{p.2.b.total}{The sum of the coefficients of the linear term
##' for phase 2} \item{p.5.c.ss}{The sum of squares of the coefficients of the
##' quadratic terms for phase 5} \item{p.1.d.total}{The sum of the RMSE's of
##' the regression fits in phase 1} \item{p.7.start.value}{The first data point
##' in phase 7} \item{p.8.end.value}{The last data point in phase 8}
##' \item{p.3.n.count}{The number of data points in phase 3}
##' @author Landon Sego
##' @keywords misc

calcIVTslow <- function(y, phase.vec, x1, min.window=5, start=1, skip=1, actual.min.max=TRUE, linear.only=FALSE) {

  # y is the data vector
  # bw is the bandwidth

  bw <- length(x1) %/% 2
  n <- length(y)
  nf <- length(x1)

  # Set -9999 to NA
  y[round(y,14) == -9999] <- NA
  
  # Sanity checks
  if (length(phase.vec) != length(y))
    stop("'phase.vec' and 'y' do not have the same length\n")
  if (!all(sort(phase.vec)==phase.vec))
    stop("'phase.vec' not listed in sequential order\n")
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
  phase.vec.win <- phase.vec[win.centers]

  if (length(win.centers)==1)
    resp.mat <- matrix(resp.mat,ncol=1)

  # Apply the lmFit function over the response matrix
  fits <- t(apply(resp.mat, 2, lmFit))
  colnames(fits) <- letters[1:4]
  rownames(fits) <- 1:NROW(fits)

  if (length(phase.vec.win) != NROW(fits))
    stop("length(phase.vec.win) != NROW(fits))\n")
  
  # Initialize matrix for summaries
  ivtMat <- matrix(NA, ncol=19, nrow=length(unique(phase.vec)),
                   dimnames=list(unique(phase.vec),
                                 c("a.total",
                                   "b.total",
                                   "c.total",
                                   "d.total",
                                   "a.ss",
                                   "b.ss",
                                   "c.ss",
                                   "d.ss",
                                   "a.min",
                                   "b.min",
                                   "c.min",
                                   "d.min",
                                   "a.max",
                                   "b.max",
                                   "c.max",
                                   "d.max",
                                   "n.count",
                                   "start.value",
                                   "end.value")))
                                  
  # Create the summaries

  # Totals, ss, min, max
  for (i in letters[1:4]) {
    #Totals
    tot <- tapply(fits[,i], phase.vec.win, sp.sum)
    ivtMat[,paste(i,"total",sep=".")] <- tot[rownames(ivtMat)]
    #ss
    tot <- tapply(fits[,i], phase.vec.win, function(x) sp.sum(x^2))
    ivtMat[,paste(i,"ss",sep=".")] <- tot[rownames(ivtMat)]

    if ((actual.min.max) & (i == "a"))
      next
    else {
      # min
      tot <- tapply(fits[,i], phase.vec.win, sp.min)
      ivtMat[,paste(i,"min",sep=".")] <- tot[rownames(ivtMat)]
      # max
      tot <- tapply(fits[,i], phase.vec.win, sp.max)
      ivtMat[,paste(i,"max",sep=".")] <- tot[rownames(ivtMat)]
    }
  }

  if (actual.min.max) {

    # actual min
    tot <- tapply(y, phase.vec, sp.min)
    ivtMat[,"a.min"] <- tot[rownames(ivtMat)]

    # actual max
    tot <- tapply(y, phase.vec, sp.max)
    ivtMat[,"a.max"] <- tot[rownames(ivtMat)]
    
  }

  # Get the n.counts, using the linear term
  tot <- tapply(fits[,2], phase.vec.win, sp.count)
  ivtMat[,"n.count"] <- tot[rownames(ivtMat)]

  # Get the starting values
  tot <- tapply(y, phase.vec, function(x) x[1])
  ivtMat[,"start.value"] <- tot[rownames(ivtMat)]

  # Get the ending values
  tot <- tapply(y, phase.vec, function(x) x[length(x)])
  ivtMat[,"end.value"] <- tot[rownames(ivtMat)]

  # Reformat into a single vector with the appropriate names
  nms <- rep(NA, NROW(ivtMat)*NCOL(ivtMat))
  out <- rep(NA, NROW(ivtMat)*NCOL(ivtMat))
  for (i in 1:NROW(ivtMat)) {
    for (j in 1:NCOL(ivtMat)) {
      out[j + NCOL(ivtMat)*(i-1)] <- ivtMat[i,j]
      nms[j + NCOL(ivtMat)*(i-1)] <- paste("p", rownames(ivtMat)[i], colnames(ivtMat)[j], sep=".")
    }
  }

  names(out) <- nms

  out
  
} # calcIVTslow

