##' Compute the quadratic, linear, and/or discrete features of multiple variables for a single group
##' 
##' Fits the moving window quadratic (or linear) regression model for each continuous variable in
##' a data frame, and calculates summary statistics of the parameters.  Also calculates the
##' duration and transition features of discrete variables.
##' 
##' A least one of \code{cont} or \code{disc} must be specified.
##'
##' Instead of a data frame, the \code{y} argument can be a
##' \code{valid_getFeatures_args} object (returned by \code{\link{check_getFeatures_args}}), in which
##' case all the subsequent arguments to \code{getFeature} are ignored
##' (because the \code{valid_getFeatures_args} object contains all those arguments). This is useful
##' if \code{getFeatures} is called repeatedly over the same set of argument values (which occurs
##' in \code{\link{ddply_getFeatures}}.
##' 
##' @export
##'
##' @inheritParams check_getFeatures_args
##'
##' @return A named vector containing the features for each of the variables
##' requested in \code{cont} and \code{disc}.  The names follow the form
##' [varname].[description], where the [varname] is specified in \code{cont} and
##' \code{disc}, and [description] follows the naming convention produced by
##' \code{\link{summary.fitQ}} and \code{\link{discFeatures}}.
##' 
##' @author Landon Sego
##'
##' @references  Amidan BG, Ferryman TA. 2005.  "Atypical Event and Typical Pattern Detection within Complex Systems."
##' IEEE Aerospace Conference Proceedings, March 2005.
##' 
##' @examples
##' # Load the data
##' data(demoData)
##' 
##' # Select a subset of thedata
##' d <- demoData[demoData$subject == 3 & demoData$phase == "f",]
##' colnames(d)
##' 
##' # Run over that subset
##' features <- getFeatures(d, cont = 3:4, disc = 8:11, stats = c("mean", "sd"),
##'                         fitQargs = list(x1 = -5:5, start = 2))
##' str(features)
##' features
##'
##' # We can also call the function by validating the arguments before hand:
##' validated <- check_getFeatures_args(d, cont = 3:4, disc = 8:11, stats = c("mean", "sd"),
##'                                     fitQargs = list(x1 = -5:5, start = 2))
##' 
##' features1 <- getFeatures(validated)
##'
##' # We get the same result
##' identical(features1, features)


getFeatures <- function(y, cont = NULL, disc = NULL, centerScale = TRUE,
                        stats = c("min", "q1", "mean", "med", "q3", "max", "sd", "count"),
                        fitQargs = NULL) {

  # Check whether y is a 'valid_getFeatures_args' object.  If not, check the args
  if (!inherits(y, "valid_getFeatures_args")) {
    y <- check_getFeatures_args(y, cont = cont, disc = disc, centerScale = centerScale,
                                stats = stats, fitQargs = fitQargs)      
  }
    
  # Initialize the output vector
  out <- NULL
  
  # Process continuous variables
  if (!is.null(y$cont)) {

    # Add the names to the vector so that they are appended in the output
    names(y$cont) <- y$cont

    # Calculate the quadratic features and resulting summary statistics for
    # a single continous variable
    calcCont <- function(x) {
      return(summary(do.call(fitQ, c(list(y = y$y[,x]), y$fitQargs)), stats = y$stats))
    }
    
    # Calculate the features for all continuous variables
    out <- unlist(lapply(y$cont, calcCont))
    
  }

  # Process the discrete variables
  if (!is.null(y$disc)) {

    # Add the names to the vector so that they are appended in the output
    names(y$disc) <- y$disc

    # Calculate the sumamries of the discrete features
    out <- c(out, unlist(lapply(y$disc, function(x) discFeatures(y$y[,x]))))
      
  }

  return(out)
    
} # getFeatures
