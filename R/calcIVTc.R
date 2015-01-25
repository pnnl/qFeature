# Rapidly calculates the IVT components for a single continuous variable, y.
# 'start' and 'skip' correspond to actual data points within 'y'

# Landon Sego, January 30, 2008

##' Fits the moving window regression model and summarizes results by phases
##' 
##' Fits the moving window regression model over a single vector of data and
##' summarizes results by phases
##' 
##' @export
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
##' @seealso \code{\link{continuousIVT}}, \code{\link{fitQ}},
##' \code{\link{calcIVTslow}}
##' @keywords misc


calcIVTc <- function(y, phase.vec, x1, min.window=5, start=1, skip=1, actual.min.max=TRUE, linear.only=FALSE) {

  # Sanity checks
  if (length(phase.vec) != length(y))
    stop("'phase.vec' and '", deparse(substitute(y)), "' do not have the same length\n")
  if (!all(sort(phase.vec) == phase.vec))
    stop("Elements of 'phase.vec' not in sequential order\n")

  # Set -9999 to NA
  y[round(y,14) == -9999] <- NA

  # If all the data are missing, return a single NA and exit the function
  if (all(is.na(y)))
    return(NA)

  # Fit the quadratic regressions to moving windows across 'y'
  fits <- try(fitQ(y, x1, min.window=min.window, start=start, skip=skip, linear.only=linear.only),
              silent=TRUE)

  # To more easily diagnose potential problems...
  if (class(fits) == "try-error")
    stop("fitQ() failed:  ", fits)

  # Define the phase vector that corresponds to the windows that
  # were fit by fitQ
  phase.vec.win <- phase.vec[seq(start, length(y), by=skip)]

  # Verify the output from the fitQ_1 routine matches the lengths of phase.vec.win
  if (any(length(phase.vec.win) != unlist(lapply(fits, length))))
    stop("one of 'length(phase.vec.win) != unlist(lapply(fits, length)))'\n")
    
  # Calculate summaries by phase

  # The n.count is the number of windows that were actually fit in
  # each phase.  We'll use the linear term to check 
  n.count <- tapply(fits[["b"]], phase.vec.win, sp.count)
  names(n.count) <- paste("p", names(n.count), "n.count", sep=".")

  # If the actual mins and maxes are requested.  The actual mins and
  # maxes are placed in the 'a.min' and 'a.max' elements
  if (actual.min.max) {

    # Calculate total and ss a, b, c, and d for each phase
    s1 <- unlist(lapply(fits, tapply, phase.vec.win,
                        function(x) c(total = sp.sum(x),
                                      ss = sp.sum(x^2))))

    # Calculate the min and max for b, c, d columns by phase    
    s2 <- unlist(lapply(fits[letters[2:4]], tapply, phase.vec.win,
                        function(x) c(min = sp.min(x),
                                      max = sp.max(x))))
    # Combine them 
    s3 <- c(s1, s2)

    # Fix their names (e.g. switching 'a.3' to 'p.3.a')
    names(s3) <- unlist(lapply(strsplit(names(s3),"\\."),
                               function(x) paste(c("p", x[c(2,1,3)]),
                                                 collapse=".")))
    
    # Calculate min, max, start and end values for each phase
    s4 <- unlist(tapply(y, phase.vec,
                        function(x) c(a.min = sp.min(x),
                                      a.max = sp.max(x),
                                      start.value = x[1],
                                      end.value = x[length(x)])))

    # Fix names
    names(s4) <- paste("p", names(s4), sep=".")
    
    # Final vector
    return(c(s3, s4, n.count))

  } # If the actual mins and maxes of the data are requested

  # If the mins and maxes of the a's are preferred
  else {

    # Calculate total and ss a, b, c, and d for each phase
    s1 <- unlist(lapply(fits, tapply, phase.vec.win,
                        function(x) c(total = sp.sum(x),
                                      ss = sp.sum(x^2),
                                      min = sp.min(x),
                                      max = sp.max(x))))

    # Fix their names (e.g. switching 'a.3' to 'p.3.a')
    names(s1) <- unlist(lapply(strsplit(names(s1),"\\."),
                               function(x) paste(c("p", x[c(2,1,3)]),
                                                 collapse=".")))
    
    # Calculate min, max, start and end values for each phase
    s2 <- unlist(tapply(y, phase.vec,
                        function(x) c(start.value = x[1],
                                      end.value = x[length(x)])))

    # Fix names
    names(s2) <- paste("p", names(s2), sep=".")

    # Final vector
    return(c(s1, s2, n.count))
    
  } # else if the mins and maxes of the a's are preferred
    
} # calcIVTc

