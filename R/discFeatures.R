# Calculates the percentage of time in each state and
# the number transitions between states, summarized by phase
# The algorithm works for 2 to 55 states
# (but we bound it above at 40 states just to play it safe)

# Landon Sego,  2008-01-30

# Reworked for the qFeature package, 2014-01-24


##' Summarizes the transitions between discrete states by phase
##' 
##' Calculates the percentage of time in each state and the number transitions
##' between states
##' 
##' @export
##' 
##' @param y A vector with discrete states
##' 
##' @param vname character string giving a prefix for the name labels
##' 
##' @return A named vector with the fraction of time spent in each state, and the
##' number of transitions from one state to another
##' 
##' @author Landon Sego
##' 
##' @seealso \code{\link{transMap}}, \code{\link{sp.table}}
##' 
##' @examples
##' x <- c("a","a","a","b","b","a","a","c","c","b","b")
##' discFeatures(x, vname = "Test")
##'
##' discFeatures(c(T, F, F, T, F, F, F), vname = "switch")
##'
##' discFeatures(c(1, 1, 1, 2, 2), vname = "simple")
##'
##' discFeatures(c(rep(pi, 3), rep(exp(1), 3), rep(sqrt(2), 3)))

discFeatures <- function(y, vname = "V") {

  # Convert the vector to a factor
  if (!is.factor(y)) {

    # Make sure it's a vector
    if (!is.vector(y)) {
      stop("'y' must be a vector or a factor")
    }
    
    y <- as.factor(y)
    
  } # if y wasn't a factor to begin with

  # Get the levels
  ly <- levels(y)
  
  # Determine the number of levels
  nStates <- length(ly)

  # Special case of nStates = 1
  if (nStates == 1) {
    out <- 1
    names(out) <- paste(vname, "percent", ly, sep = ".")
    return(out)
  }

  # Verify we don't have more than 55 states or levels
  if (nStates > 55) {
    stop("'", deparse(substitute(y)), "' has ", nStates, "states, which is larger than\n",
         "upper limit of 55 states. Consider aggregating some of the states or treating\n",
         "the variable as a continuous.")
  }
  
  # Removing missing data from y 
  y <- y[!is.na(y)]

  # Process strictly using integers
  y.codes <- as.integer(y)
  y.codes.unique <- as.integer(1:nStates)
  
  # Implement the 1 to 1 mapping whose first differences uniquely
  # identify the transitions between the states
  tmap <- transMap(y.codes, y.codes.unique)

  # Restore the original labels
  tmap$from <- factor(tmap[["from"]], levels = y.codes.unique, labels = ly)
  tmap$to <- factor(tmap[["to"]], levels = y.codes.unique, labels = ly)
  
  # Create the labels for transitions between two different states
  trans.state.labels <- paste(vname, "num_trans",
                              paste(tmap[["from"]], tmap[["to"]], sep="_"),
                              sep = ".")

  # Create The labels for transitions within the same state
  # (which are summarized as a percentage)
  non.trans.state.labels <- paste(vname, "percent", ly, sep = ".")

  # Summary functions that will be applied in the loop below
  trans.summary <- function(x) {
    sp.table(x, tmap[["transValue"]])
  }
  
  pct.summary <- function(x) {
    sp.table(x, y.codes.unique, pct = TRUE)
  }

  # Gather the summaries together
  out <- c(pct.summary(y.codes), trans.summary(tmap[["diffMap"]]))
  names(out) <- c(non.trans.state.labels, trans.state.labels)

  return(out)
  
} # discFeatures
