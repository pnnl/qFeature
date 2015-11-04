##' Compute the quadratic, linear, and/or discrete features of multiple variables for a
##' single group
##'
##' This is a wrapper that implements \code{\link{getFeatures}} for each group in a data
##' frame using \code{\link[plyr:ddply]{plyr::ddply}}.
##'
##' A least one of \code{cont} or \code{disc} must be specified.
##'
##' Instead of a data frame, the \code{y} argument can be a
##' \code{valid_getFeatures_args} object (returned by \code{\link{check_getFeatures_args}}), in which
##' case all the subsequent arguments to \code{getFeature} are ignored
##' (because the \code{valid_getFeatures_args} object contains all those arguments).
##' 
##' Parallel processing,
##' if requested via \code{nJobs > 1}, is facilitated via \code{\link[Smisc:pddply]{Smisc::pddply}},
##' a wrapper for parallelized calls to \code{\link[plyr:ddply]{plyr::ddply}}.
##'
##' @export
##' @inheritParams check_getFeatures_args
##' 
##' @param .variables character vector with variable names in \code{y} that will
##' be used to split the data. These combinations of the variables uniquely identify
##' the groups for which the features will be separately extracted.  This is passed
##' directly to the argument of the same name in \code{\link[plyr:ddply]{plyr::ddply}}.
##' 
##' @param nJobs The number of parallel jobs to run when extracting the features.
##'
##' @return A dataframe with one row for each grouping defined by \code{.variables}.
##' The features computed by \code{\link{getFeatures}} is presented across the columns.
##'
##' @author Landon Sego
##'
##' @references  Amidan BG, Ferryman TA. 2005.  "Atypical Event and Typical Pattern Detection within Complex Systems."
##' IEEE Aerospace Conference Proceedings, March 2005. 
##'
##' @examples
##'# Load the data
##'data(demoData)
##'str(demoData)
##'
##'# Calculate features for each subset defined by the unique combinations of
##'# "subject" and "phase", calculate the mean and standard deviation summary
##'# statistics to summarize the coefficients of the quadratic model fits
##'f <- ddply_getFeatures(demoData, c("subject", "phase"),
##'                       cont = 3:4, disc = 8:9, stats = c("mean", "skew"),
##'                       fitQargs = list(x1 = -5:5), nJobs = 2)
##'
##'str(f)
##'head(f)

ddply_getFeatures <- function(y, .variables, cont = NULL, disc = NULL, centerScale = TRUE,
                              stats = c("min", "q1", "mean", "med", "q3",
                                        "max", "sd", "count"),
                              fitQargs = NULL, nJobs = 1) {

  # Check whether y is a 'valid_getFeatures_args' object.  If not, check the args
  if (!inherits(y, "valid_getFeatures_args")) {
      
    y <- check_getFeatures_args(y, cont = cont, disc = disc, centerScale = centerScale,
                                stats = stats, fitQargs = fitQargs)
    
  }

  # Check the nJobs argument,  
  stopifnot(is.numeric(nJobs),
            nJobs %% 1 == 0,
            nJobs >= 1)

  # Check the .variables argument
  .variables <- Smisc::selectElements(.variables, colnames(y$y))

  # Get all the args that are not the data
  allArgs <- y[names(y) != "y"]
 
  # Construct a getFeatures wrapper suitable for ddply in order to handle the argument checking
  # performed by check_getFeatures_args
  getFeaturesW <- function(ySub, allArgs = NULL) {

    # Construct a 'valid_getFeatures_args' object to pass into getFeatures()
    yIn <- c(list(y = ySub), allArgs)
    class(yIn) <- c("valid_getFeatures_args", "list")

    return(getFeatures(yIn))
      
  } # getFeaturesW
  
  # Parallel processing
  if (nJobs > 1) {

    return(Smisc::pddply(y$y, .variables, getFeaturesW, allArgs = allArgs,
                         njobs = nJobs, .paropts = list(.packages = "qFeature")))

  }
  
  # Non-parallel processing
  return(plyr::ddply(y$y, .variables, getFeaturesW, allArgs = allArgs))
  
} # ddply_getFeatures
