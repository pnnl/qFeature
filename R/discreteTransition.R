# Calculates the percentage of time in each state and
# the number transitions between states, summarized by phase
# The algorithm works for 2 to 55 states
# (but we bound it above at 40 states just to play it safe)

# Landon Sego,  2008-01-30


##' Summarizes the transitions between discrete states by phase
##' 
##' Calculates the percentage of time in each state and the number transitions
##' between states, summarized by phase
##' 
##' @export 
##' @param y an integer vector of a discrete flight parameter
##' @param phase.vec see \code{\link{continuousIVT}}
##' @param phases.to.process see \code{\link{continuousIVT}}
##' @param vname character string giving a prefix for the state transition
##' labels
##' @return A matrix with the phases on the rows and the summaries of the
##' transition counts and the percentage of time spent in each state on the
##' columns.
##' @author Landon Sego
##' @seealso \code{\link{transMap}}, \code{\link{sp.table}}
##' @keywords misc
##' @examples
##' 
##' x <- c(1,1,1,2,2,1,1,3,3,2,2)
##' phase.vec <- c(rep(4,5), rep(5,6))
##' discreteTransition(x, phase.vec, 4:5, vname="Test")
##' 

discreteTransition <- function(y, phase.vec, phases.to.process, vname="V") {

  # vname is a prefix for the state transition labels
  
  if (length(y) != length(phase.vec))
    stop("length(y) != length(phase.vec))\n")
  
  # Missing data indicator
  missing <- (y == -9999) | is.na(y)

  # Removing missing data from y and phase.vec
  y <- y[!missing]
  phase.vec <- phase.vec[!missing]

  # Select phases to process
  phases.to.process.ind <- phase.vec %in% phases.to.process
  y <- y[phases.to.process.ind]
  phase.vec <- phase.vec[phases.to.process.ind]

  # Assume that y is positive and integer valued in [0,99].
  # This assumption required by the labeling convention
  if (any((y %% 1) != 0))
    stop("Fractional value of y found. ",
         "All values of y must be positive integers from 0 to 99\n")

  if (any((y < 0) | (y > 99))) 
    stop("All values of y must be positive integers from 0 to 99\n")

  # Ensuring that R recognizes y as an integer is critital.  Otherwise,
  # to unique() runs VERY slowly.
  y <- as.integer(y)
  y.unique <- sort(unique(y))
  
  # Ensure at least 2 levels...ASSUMING THAT THOSE TWO LEVELS ARE 0 AND 1
  if (length(y.unique) == 1) 
    y.unique <- 0:1
#    y.unique <- c(y.unique, y.unique+1)

  # Implement the 1 to 1 mapping whose first differences uniquely
  # identify the transitions between the states
  tmap <- transMap(y, y.unique)

  # Create the labels for transitions between two different states
  trans.state.labels <- paste(vname, "transition",
                              paste(substr(as.character(100 + tmap[["from"]]), 2, 3),
                                    substr(as.character(100 + tmap[["to"]]), 2, 3),
                                    sep="_"),
                              sep=".")

  # Create The labels for transitions within the same state (which are summarized as a percentage)
  tmp <- substr(as.character(100 + y.unique), 2, 3)
  non.trans.state.labels <- paste(vname, "percent",
                                  paste(tmp, tmp, sep="_"),
                                  sep=".")

  # Summary functions that will be applied in the loop below
  trans.summary <- function(x) 
    sp.table(x, tmap[["transValue"]])
  
  pct.summary <- function(x)
    sp.table(x, y.unique, pct=TRUE)


  # concatenate the labels
  labels <- c(non.trans.state.labels, trans.state.labels)
  n <- length(labels)

  # Initialize the output list
  out <- vector(mode="list", length(phases.to.process))
  
  # Summarize over the phases
  count <- 1
  for (pp in phases.to.process) {
    
     pp.ind <- phase.vec == pp
     
     if (any(pp.ind)) 
       out[[count]] <- c(pct.summary(y[pp.ind]), trans.summary(tmap[["diffMap"]][pp.ind]))

     else 
       out[[count]] <- rep(NA, n)
     
     count <- count + 1
  }

  # Returns a matrix with phases on the rows and summary statistics on the columns for
  # the particular flight parameter that was passed into the function via 'y'
  matrix(unlist(out), byrow=TRUE, nrow=length(out), ncol=n, dimnames=list(phases.to.process, labels))

} # discreteTransition
