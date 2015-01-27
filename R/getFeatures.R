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
##' code{\link{summary.fitQ}} \code{\link{discFeatures}}.
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

  # Basic sanity checks
  stopifnot(is.data.frame(y),
            !(is.null(cont) & is.null(disc)),
            is.character(stats) | is.function(stats),
            if (!is.null(fitQargs)) is.list(fitQargs) else TRUE)

  # Make sure 'cont' and 'disc' are the way we want them, if getFeatures() is called
  # from the global environment
  pe <- parent.frame()
  
  if (environmentName(pe) == "R_GlobalEnv") {
    vars <- checkInputs(colnames(y), cont, disc)
    cont <- vars$cont
    disc <- vars$disc
    cat("checking inputs\n")  ################# REMOVE LATER
  }

  # Make sure we have character inputs
  stopifnot(is.character(cont),
            is.character(disc))

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
    
    # Define the summary stats function
    if (!is.function(stats)) {
      sumStats <- summaryStats(stats)
    }

    # Add the names to the vector so that they are appended in the output
    names(cont) <- cont
    
    # Calculate the features for continuous variable
    out <- unlist(lapply(cont, function(x) summary(do.call(fitQ, c(list(y = y[,x]), fitQargs)), stats = sumStats)))
    
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
checkInputs <- function(ny, cont, disc) {

  # ny = colnames

  if (!is.null(cont)) {
    if (!is.character(cont)) {
      cont <- ny[cont]
    }
    if (!all(cont %in% ny)) {
      stop("'cont' column names '", paste(setdiff(cont, ny), sep = "', '"),
           "' are not in 'y'")
    }
  }
  if (!is.null(cont)) {
    if (!is.character(disc)) {
      disc <- ny[disc]
    }
    if (!all(disc %in% ny)) {
      stop("'disc' column names '", paste(setdiff(disc, ny), sep = "', '"),
           "' are not in 'y'")
    }    
  }

  return(list(cont = cont, disc = disc))

} # checkInputs



