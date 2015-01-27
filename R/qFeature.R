##' Compute the quadratic, linear, and/or discrete features of multiple variables for a single group
##'
##' This is a wrapper that implements \code{\link{getFeatures}} for each group in a data frame using
##' \code{\link[plyr:ddply]{plyr::ddply}}.
##'
##' A least one of \code{cont} or \code{disc} must be specified.
##' 
##' @export
##' 
##' @param x Data frame, each row containing a vector of measurements for a particular point in time, with
##' columns indicating the measured variables (and possibly other descriptive variables).  The
##' data processed presuming the rows are orderd chronologically.
##'
##' @param .variables  variables (i.e. columns) in \code{y} to split data frame by, input as 'as.quoted' variables.
##' These combinations of the variables uniquely identify the groups for which the features will be
##' separately extracted.  This is passed directly to the argument of the same name in
##' \code{\link[plyr:ddply]{plyr::ddply}}.
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
##' @param nJobs The number of jobs to run when extracting the features.  Uses the \code{.parallel} argument of
##' \code{\link[plyr:ddply]{plyr::ddply}}, which in turn relies on \code{\link[foreach:foreach]{foreach::foreach}}.
##'
##' @return A dataframe with one row for each grouping defined by \code{.variables}.  The features computed by
##' \code{\link{getFeatures}} is presented across the columns.
##'
##' @author Landon Sego
##'
##' @examples
##'# Load the data
##'data(demoData)
##'str(demoData)
##'
##'# Calculate features for each subset
##'f <- qFeature(demoData, c("subject", "phase"),
##'              cont = 3:4, disc = 8:11, stats = c("mean", "sd"),
##'              fitQargs = list(x1 = -5:5))
##'
##'str(f)
##'head(f)

qFeature <- function(y, .variables, cont = NULL, disc = NULL, 
                     stats = c("min", "q1", "mean", "med", "q3", "max", "sd", "count"),
                     fitQargs = NULL, nJobs = 1) {
    
  # Basic sanity checks.  Other arguments are checked in more detail by getFeatures()
  stopifnot(is.numeric(nJobs),
            nJobs %% 1 == 0,
            nJobs >= 1)
    
  vars <- checkInputs(colnames(y), cont, disc)
  cont <- vars$cont
  disc <- vars$disc

  if (nJobs > 1) {

    # Set up the cluster  
    cl <- parallel::makeCluster(nJobs)
    parallel::clusterEvalQ(cl, library(qFeature))
    doParallel::registerDoParallel(cl)

    # The warning supression is needed due to an outstanding issue with plyr
    # that only occurs when the function is called in parallel.
    # See https://github.com/hadley/plyr/issues/203
    o <- suppressWarnings(plyr::ddply(y, .variables, getFeatures, cont = cont, disc = disc, stats = stats,
                                      fitQargs = fitQargs, .parallel = TRUE))

    # suppressWarnings() works too well for this particular warning.  The warning vanishes and is not recoverable
    # via warnings().  TRY code in demo(error.catching)
    
    # These are the warnings that are issued
    w <- c("<anonymous>: ... may be used in an incorrect context: '.fun(piece, ...)'\n")

    # Capture the warnings
    sWarn <- warnings()


    
    # Now remove the goofy warnings
    if (!is.null(sWarn)) {
        
      sWarn <- sWarn[-which(names(sWarn) == w)]

      # If we still have warnings, then print them
      if (!is.null(sWarn)) {
        print(sWarn)
      }
          
    }

    # Shut down the cluster
    parallel::stopCluster(cl)

  } # If we're processing in parallel

  else {

    o <- plyr::ddply(y, .variables, getFeatures, cont = cont, disc = disc, stats = stats, fitQargs = fitQargs)
      
  }

  return(o)
  
} # qFeature
