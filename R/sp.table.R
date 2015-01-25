# This function is a wrapper function for the 'sp_table' routine
# that is very similar to the 'table' function in R, only simpler.

# Landon Sego, January 30, 2008


##' A 'special' table function optimized for integers
##' 
##' A function similar to the \code{\link{table}} function that works only for
##' integers for added speed.
##' 
##' \code{y.unique} may contain values that are not in \code{y}.
##'
##' @export
##' 
##' @param y An integer vector
##' 
##' @param y.unique The unique values of \code{y} whose occurence in \code{y}
##' will be counted
##' 
##' @param pct \code{=TRUE} report the counts as percentages
##' 
##' @return An integer vector containing the counts or percentages of each of
##' the elements of \code{y.unique}
##' 
##' @author Landon Sego
##' 
##' @seealso \code{\link{table}}
##' 
##' @examples
##' x <- sample(c(rep(1,5), rep(2,3)))
##' sp.table(x, 1:2)
##' sp.table(x, 1:3)

sp.table <- function(y, y.unique, pct = FALSE) {

  n.y <- length(y)
  n.u <- length(y.unique)
  
  out <- .C("sp_table",
            as.integer(y),
            as.integer(n.y),
            as.integer(y.unique),
            as.integer(n.u),
            totals = integer(n.u))$totals

#  names(out) <- y.unique

  if (pct) {
    return(out / n.y)
  }
  else {
    return(out)
  }

} # sp.table
