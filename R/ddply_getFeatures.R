##' Compute the quadratic, linear, and/or discrete features of multiple variables for a
##' single group
##'
##' This is a wrapper that implements \code{\link{getFeatures}} for each group in a data
##' frame using \code{\link[plyr:ddply]{plyr::ddply}}.
##'
##' A least one of \code{cont} or \code{disc} must be specified.
##'
##' Parallel processing,
##' if requested via \code{nJobs > 1}, is facilitated via the 
##' \code{.parallel} argument of \code{\link[plyr:ddply]{plyr::ddply}}, which in turn
##' relies on \code{\link[foreach:foreach]{foreach::foreach}},
##' \code{\link[doParallel:doParallel]{doParallel::doParallel}}, and various functions
##' from the \pkg{parallel} package.
##' 
##' @export
##' 
##' @param x Data frame, each row containing a vector of measurements for a particular
##' point in time, with columns indicating the measured variables (and possibly other
##' descriptive variables).  The data processed presuming the rows are orderd
##' chronologically.
##'
##' @param .variables  variables (i.e. columns) in \code{y} to split data frame by, input
##' as 'as.quoted' variables. These combinations of the variables uniquely identify
##' the groups for which the features will be separately extracted.  This is passed
##' directly to the argument of the same name in \code{\link[plyr:ddply]{plyr::ddply}}.
##' 
##' @param cont Vector of integers or a character vector indicating the columns
##' of \code{x} that correspond to continuous variables.  These are the variables from
##' which features will be extracted by fitting the moving regression model using
##' \code{\link{fitQ}}.
##'
##' @param disc Vector of integers or character vector indicating the columns
##' of \code{x} that correspond to variables that will be treated as discrete. These are
##' the variables from which features will be extracted using \code{\link{discFeatures}}.
##'
##' @param stats This argument defines the summary statistics that will be calculated
##' for each of the regression parameters.  It can be a character vector of summary
##' statistics, which are passed to \code{\link{summaryStats}}.  Or the function object
##' returned by \code{\link{summaryStats}} may be supplied.
##'
##' @param fitQargs Named list of arguments for \code{\link{fitQ}}.  If \code{NULL}, the
##' default arguments of \code{\link{fitQ}} are used.
##'
##' @param nJobs The number of parallel jobs to run when extracting the features.
##'
##' @return A dataframe with one row for each grouping defined by \code{.variables}.
##' The features computed by \code{\link{getFeatures}} is presented across the columns.
##'
##' @author Landon Sego
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
##'                       cont = 3:4, disc = 8:11, stats = c("mean", "sd", "skew"),
##'                       fitQargs = list(x1 = -5:5), nJobs = 2)
##'
##'str(f)
##'head(f)

ddply_getFeatures <- function(y, .variables, cont = NULL, disc = NULL, 
                              stats = c("min", "q1", "mean", "med", "q3",
                                        "max", "sd", "count"),
                              fitQargs = NULL, nJobs = 1) {

  # Variable checking
  v <- checkInputs(y, cont, disc, stats, fitQargs)

  # Check the nJobs argument
  stopifnot(is.numeric(nJobs),
            nJobs %% 1 == 0,
            nJobs >= 1)
  
  # Parallel processing
  if (nJobs > 1) {

    # Load the requiste namespaces
    loadNamespace("parallel")

    willStop <- FALSE
    
    if (!requireNamespace("doParallel", quietly = TRUE)) {
      willStop <- TRUE
    }
    if (!require(foreach, quietly = TRUE)) {
      willStop <- TRUE
    }

    if (willStop) {
      stop("The 'doParallel' and 'foreach' packages must be installed for ",
           "parallel processing")
    }

    # Set up the cluster  
    cl <- parallel::makeCluster(nJobs)
    parallel::clusterEvalQ(cl, library(qFeature))
    doParallel::registerDoParallel(cl)

    # The warning collection is needed due to an outstanding (and innocuous) issue with
    # plyr that only occurs when the function is called in parallel.
    # See https://github.com/hadley/plyr/issues/203
    o <- tryCatchWE(plyr::ddply(y, .variables, getFeatures, cont = v$cont,
                                disc = v$disc, stats = v$stats,
                                fitQargs = fitQargs, .parallel = TRUE))

    # Shut down the cluster
    parallel::stopCluster(cl)
    
    # If we have an error, then stop
    if (is(o$value, "error")) {
      stop(o$value)
    }
    
    # Extract and remove bogus warnings
    if (!is.null(o$warning)) {

      bogus <- c("<anonymous>: ... may be used in an incorrect context: '.fun(piece, ...)'\n")
      # Identify the valid warnings
      validWarnings <- unlist(lapply(o$warning, function(x) x$message != bogus))

      # Retain valid warnings
      o$warning <- o$warning[validWarnings]

      # If there any warnings left, issue them
      if (length(o$warning)) {
        lapply(o$warning, warning)
      }
      
    } # If there are warning messages

    # Extract the computed features
    o <- o$value

  } # If we're processing in parallel

  # Non-parallel processing
  else {

    o <- plyr::ddply(y, .variables, getFeatures, cont = v$cont, disc = v$disc,
                     stats = v$stats, fitQargs = fitQargs)
      
  }

  return(o)
  
} # ddply_getFeatures

# I got this idea from demo(error.catching).  This actually trapped the warnings,
# whereas suppressWarnings() made them vanish completely
tryCatchWE <- function(expr) {
    
  W <- NULL

  w.handler <- function(w) { 
 	  W <<- c(W, w = list(w))
   	invokeRestart("muffleWarning")
  } 

  return(list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
 			                                    warning = w.handler),
              warning = W))
  
} # tryCatchWE
