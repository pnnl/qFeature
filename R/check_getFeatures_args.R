##' Check and validate arguments for getFeatures() and ddply_getFeatures()
##'
##' Check and validate arguments for \code{\link{getFeatures}} and \code{\link{ddply_getFeatures}}
##' before they are used by those functions.
##'
##' This function exists so that multiple calls to \code{\link{getFeatures}}
##' from \code{\link{ddply_getFeatures}} would not involve repeating the checks on essentially
##' the same arguments.
##'
##' @export
##' @param y Data frame, each row containing a vector of measurements for a particular point in time,
##' with columns indicating the discrete and/or continuous measured variables (and possibly other
##' descriptive variables).  The data processed presuming the rows are orderd chronologically.
##'
##' @param cont Vector of integers or a character vector indicating the columns
##' of \code{x} that correspond to continuous variables.  These are the variables from which features
##' will be extracted by fitting the moving regression model using \code{\link{fitQ}}.
##'
##' @param disc Vector of integers or character vector indicating the columns
##' of \code{x} that correspond to variables that will be treated as discrete. These are the variables
##' from which features will be extracted using \code{\link{discFeatures}}.
##'
##' @param centerScale Logical indicating whether the continuous variables (indicate by \code{cont})
##' should be centered and scaled by the global mean and standard deviation of that variable.  By
##' 'global', we mean all the values of a continuous variable, say \code{x}, in \code{y} are used
##' to compute the mean and standard deviation.  The resulting value for the continuous variable,
##' \code{x}, is equivalent to \code{y$x <- (y$x - mean(y$x)) / sd(y$x)}.
##'
##' @param stats This argument defines the summary statistics that will be calculated
##' for each of the regression parameters.  It can be a character vector of summary statistics,
##' which are passed to \code{\link{summaryStats}}.  Or the function object returned by
##' \code{\link{summaryStats}} may be supplied.
##'
##' @param fitQargs Named list of arguments for \code{\link{fitQ}}. If \code{NULL}, the default
##' arguments of \code{\link{fitQ}} are used. Any argument for \code{\link{fitQ}} may be included
##' except \code{y}.
##' 
##' @return An object of class \code{valid_getFeatures_args} that can be passed to the first argument
##' of \code{\link{getFeatures}} or \code{\link{ddply_getFeatures}}. It is a list with the same
##' elements that were passed into the function: \code{y}, \code{cont}, \code{disc}, \code{stats},
##' and \code{fitQ}, with the exception that the continuous variables of \code{y} are now
##' centered and scaled if \code{centerScale = TRUE}, and \code{cont} and \code{disc} are converted
##' to the character column names (if integers were originally provided).
##'
##' @author Landon Sego
 
# Function for checking the inputs of getFeatures() and ddply_getFeatures()
check_getFeatures_args <- function(y, cont = NULL, disc = NULL, centerScale = TRUE,
                                   stats = c("min", "q1", "mean", "med", "q3", "max", "sd", "count"),
                                   fitQargs = NULL) {

  # structural checks
  stopifnot(is.data.frame(y),
            nrow(y) > 2,
            !(is.null(cont) & is.null(disc)),
            is.character(stats) | inherits(stats, "summaryStats_function"),
            is.logical(centerScale),
            length(centerScale) == 1)

  # alias for the column names
  colNames <- colnames(y)

  # Verify the column names exist in the data and convert them to character string
  # if numeric indexes were provided
  cont <- Smisc::selectElements(cont, colNames)
  disc <- Smisc::selectElements(disc, colNames)

  # Verify all variables specified by 'cont' are numeric or integer
  if (!is.null(cont)) {
    if (!all(areNumeric <- sapply(Smisc::select(y, cont), is.numeric))) {
      stop("The following columns indicated in 'cont' are not numeric: '",
           paste(cont[!areNumeric], collapse = "', '"), "'")
    }
  }
  
  # Center and scale
  if (!is.null(cont) & centerScale) {

    # scale continuous variables
    scaled.y.cont <- as.data.frame(scale(Smisc::select(y, cont)))

    # bind them back into the original data frame
    y <- cbind(Smisc::select(y, setdiff(colNames, cont)), scaled.y.cont)

    # Restore original order of columns
    y <- Smisc::select(y, colNames)
    
  }
  
  # Define the summary stats function only once
  if (!inherits(stats, "summaryStats_function")) {
    stats <- summaryStats(stats)
  }
  
  # Check fitQargs
  if (!is.null(fitQargs)) {
      
    stopifnot(is.list(fitQargs))

    n.fitQargs <- names(fitQargs)
    n.fitQ <- names(formals(check_fitQ_args))
    
    if (!(all(n.fitQargs %in% n.fitQ))) {
      stop("'", paste(setdiff(n.fitQargs, n.fitQ), collapse = "', '"),
           "' are not valid arguments for fitQ()")
    }

  }
  # If fitQargs is NULL, then go with the defaults in check_fitQ_args()
  else {

    fitQargs <- as.list(formals(check_fitQ_args))

  }

  # Call check_fitQ_args() 
  fitQargs <- do.call(check_fitQ_args, fitQargs)
  
  # Return the arguments
  out <- list(y = y, cont = cont, disc = disc, stats = stats, fitQargs = fitQargs)
  class(out) <- c("valid_getFeatures_args", class(out))
  return(out)

} # check_getFeatures_args
