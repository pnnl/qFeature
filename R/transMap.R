# This function is a wrapper for the C routine "transMap".  The algorithm is
# summarized in the comments of the C code.

# Landon Sego, January 30, 2008

##' Summarizes the type and location of transitions between discrete states in
##' an integer vector
##' 
##' Summarizes the type and location of transitions between discrete states in
##' an integer vector
##' 
##' The algorithm applies a discrete, 1-1 map, which we'll call \code{m}, to the vector
##' \code{y} such that the first differences of m(y) uniquely identify the
##' transitions between all the possible values of \code{y}. It creates the
##' map, calculates the first differences of \code{m(y)}, and produces a set of
##' transition labels that indicate which value of \code{m(Y)} corresponds to
##' each of the possible transitions among the unique states of \code{y}.
##'
##' @export
##' 
##' @param y The integer vector whose state transitions will be identified
##' 
##' @param y.unique An integer vector containing the unique values of \code{y}
##' 
##' @return A list with the following components: (also see example below)
##' \item{diffMap}{The first differences of \code{m(y)} which identify the
##' position and type of state transition in \code{y}}
##' \item{from}{The 'from' state labels}
##' \item{to}{The 'to' state labels}
##' \item{transValue}{The value that \code{diffMap} will take when
##' \code{y} transitions from the state label in \code{from} to the corresponding
##' state label in \code{to}}
##' 
##' @author Landon Sego
##' 
##' @seealso This function is called in \code{\link{discFeatures}}
##' 
##' @keywords misc
##' 
##' @examples
##' 
##' transMap(c(1,1,1,2,2,1,1,3,3,2,2), 1:3)
##' 
##' # These are the mapped values which signal when
##' # a transition took place from one state to another
##' # and they uniquely identify which type of transistion
##' # it was.  Their values can be decoded using 'transValue'
##' # below.  A value of 0 indicates that no transition occured
##' # at that index.
##' # $diffMap
##' #  [1]  0  0  0 -1  0  1  0 -3  0  2  0
##' 
##' # The 'from' state labels
##' # $from
##' # [1] 1 1 2 2 3 3
##' 
##' # The 'to' state labels
##' # $to
##' # [1] 2 3 1 3 1 2
##' 
##' # The transistion value key.  e.g., -1 corresponds to a
##' # transition from state 1 to state 2.  -3 corresponds
##' # to a transition from state 1 to state 3.  etc.
##' # $transValue
##' # [1] -1 -3  1 -2  3  2
##' 

transMap <- function(y, y.unique) {

  nY <- length(y)
  nU <- length(y.unique)

  # In R (non-compiled code) the algorithm works for up to 55 unique states,
  # but we'll bound away from it to be safe.
  if (nU > 40)
    stop("There are more than 40 unique states, which may jeopardize the ability\n",
         "to uniquely distinguish the transitions.\n")

  # The 2^x mapping
  map <- 2^(0:(nU - 1))
  ntrans <- nU^2

  # Call the C routine
  out <- .C("transMap",
            as.integer(y),
            as.integer(nY),
            as.integer(y.unique),
            as.integer(nU),
            as.integer(map),
            as.integer(ntrans),
            diffMap = integer(nY),
            from = integer(ntrans),
            to = integer(ntrans),
            transValue = integer(ntrans))[c("diffMap", "from", "to", "transValue")]

  # Remove the transitions from a state to itself
  nonZero <- as.logical(out[["transValue"]])
  out[["from"]] <- out[["from"]][nonZero]
  out[["to"]] <- out[["to"]][nonZero]
  out[["transValue"]] <- out[["transValue"]][nonZero]

  # Return the list
  return(out)
     
} # transMap
