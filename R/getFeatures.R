##' Compute the quadratic, linear, and/or discrete features of multiple variables for a single group
##' 
##' Fits the moving window quadratic (or linear) regression model for each continuous variable in a data frame,
##' and calculates summary statistics of the parameters.  Also calculates the duration and transition features of
##' discrete variables.
##' 
##' A least one of \code{cont} or \code{disc} must be specified.
##' 
##' @export
##' 
##' @param x Data frame, each row containing a vector of measurements for a particular point in time, with
##' columns indicating the measured variables (and possibly other descriptive variables).  The
##' data processed presuming the rows are orderd chronologically.
##' 
##' @param cont Vector of integers or a character vector indicating the columns
##' of \code{x} that correspond to continuous variables.  These are the variables from which features will
##' be extracted by fitting the moving regression model using \code{\link{fitQ}}.
##'
##' @param disc Vector of integers or character vector indicating the columns
##' of \code{x} that correspond to variables that will be treated as discrete. These are the variables
##' from which features will be extracted using \code{\link{discFeatures}}.
##'
##' @param stats This argument defines the summary statistics that will be calculated
##' for each of the regression parameters.  It can be a character vector of summary statistics,
##' which are passed to \code{\link{summaryStats}}.  Or the function object returned by
##' \code{\link{summaryStats}} may be supplied.
##'
##' @param fitQargs Named list of arguments for \code{\link{fitQ}}.  If \code{NULL}, the default arguments of
##' \code{\link{fitQ}} are used.
##' 
##' @return A named vector containing the features for each of the variables
##' requested in \code{cont} and \code{disc}.  The names follow the form
##' [varname].[description], where the [varname] is specified in \code{cont} and
##' \code{disc}, and [description] follows the naming convention produced by
##' \code{\link{summary.fitQ}} and \code{\link{discFeatures}}.
##' 
##' @author Landon Sego
##'
##' @examples
##' # Load the data
##' data(demoData)
##' 
##' # Select a subset of thedata
##' d <- demoData[demoData$subject == 3 & demoData$phase == "b",]
##' colnames(d)
##' 
##' # Run over that subset
##' features <- getFeatures(d, cont = 3:4, disc = 8:11, stats = c("mean", "sd"),
##'                         fitQargs = list(x1 = -5:5, start = 2))
##' str(features)
##' features

getFeatures <- function(y, cont = NULL, disc = NULL,
                        stats = c("min", "q1", "mean", "med", "q3", "max", "sd", "count"),
                        fitQargs = NULL) {

  # Perform local checks so long as this function wasn't called from ddply_getFeatures()
  # Make sure 'cont' and 'disc' are the way we want them, if getFeatures() is called
  # from the global environment.  The purpose of this is to avoid rechecking the
  # arguments multiple times when getFeatures() is called from ddply_getFeatures()
  
  if (!("ddply_getFeatures" %in% sapply(sys.calls(), function(x) as.character(x)[1]))) {
    v <- checkInputs(y, cont, disc, stats, fitQargs)
    cont <- v$cont
    disc <- v$disc
    stats <- v$stats
  }

  # Initialize the output vector
  out <- NULL
  
  # Process continuous variables
  if (!is.null(cont)) {

    # Verify they each are numeric variables
    dummy <- lapply(cont,
                    function(x) {
                      if (!is.numeric(y[,x]))
                        stop("in getFeatures(): '", x, "' was requested as a continuous variable, ",
                             "but it is not numeric", call. = FALSE)
                    })
    
    # Add the names to the vector so that they are appended in the output
    names(cont) <- cont
    
    # Calculate the features for continuous variable
    out <- unlist(lapply(cont,
                         function(x) summary(do.call(fitQ, c(list(y = y[,x]), fitQargs)),
                                             stats = stats)))
    
  }

  # Process the discrete variables
  if (!is.null(disc)) {

    # Add the names to the vector so that they are appended in the output
    names(disc) <- disc

    # Calculate the sumamries of the discrete features
    out <- c(out, unlist(lapply(disc, function(x) discFeatures(y[,x]))))
      
  }

  return(out)
    
} # getFeatures

# Function for checking the inputs of getFeatures() and qFeature()
checkInputs <- function(y, cont, disc, stats, fitQargs) {

  # structural checks
  stopifnot(is.data.frame(y),
            !(is.null(cont) & is.null(disc)),
            is.character(stats) | is.function(stats))

  ny <- colnames(y)

  # Checks for cont and disc
  checks <- function(var) {

    name <- deparse(substitute(var))
    
    if (!is.null(var)) {

      if (!is.character(var)) {

        if (!is.numeric(var)) {
          stop("Non-character entries to '", name, "' must be numeric, indicating ",
               "the column numbers of 'y'")
        }

        # Column numbers must be viable
        if (!all(var %in% 1:length(ny))) {
          stop("The following column indexes provided to '", name,
               "' are outside the range of the columns of 'y': ",
               paste(setdiff(var, 1:length(ny)), collapse = ", "))
        }
      
        var <- ny[var]
      
      } # If it's not character

      # If it is character, the names must be in the column names
      else if (!all(var %in% ny)) {

        stop("Invalid values for '", name, "':  '",
             paste(setdiff(var, ny), sep = "', '"),
             "' are not in the column names of 'y'")
      }
      
    } # If it's not NULL

    return(var)
    
  } # checks

  cont <- checks(cont)
  disc <- checks(disc)
  
  # Define the summary stats function only once
  if (!is.function(stats)) {
    stats <- summaryStats(stats)
  }
  
  # Check fitQargs
  if (!is.null(fitQargs)) {
      
    stopifnot(is.list(fitQargs))

    n.fitQargs <- names(fitQargs)
    n.fitQ <- names(formals(fitQ))[-1]
    
    if (!(all(n.fitQargs %in% n.fitQ))) {
      stop("'", paste(setdiff(n.fitQargs, n.fitQ), collapse = "', '"),
           "' are not valid arguments for fitQ()")
         
    }
    
  }

  # Return parameters that may have been modified
  return(list(cont = cont, disc = disc, stats = stats))

} # checkInputs
