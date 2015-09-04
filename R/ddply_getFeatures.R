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
##' if requested via \code{nJobs > 1}, is facilitated via the 
##' \code{.parallel} argument of \code{\link[plyr:ddply]{plyr::ddply}}, which in turn
##' relies on \code{\link[foreach:foreach]{foreach::foreach}},
##' \code{\link[doParallel:registerDoParallel]{doParallel::registerDoParallel}}, and various functions
##' from the \pkg{parallel} package.
##' 
##' @export
##'
##' @inheritParams check_getFeatures_args
##' 
##' @param .variables  variables (i.e. columns) in \code{y} to split data frame by, input
##' as 'as.quoted' variables. These combinations of the variables uniquely identify
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

    # Load the requisite namespaces
    loadNamespace("parallel")

    willStop <- FALSE
    
    if (!requireNamespace("doParallel", quietly = TRUE)) {
      willStop <- TRUE
    }
    if (!requireNamespace("foreach", quietly = TRUE)) {
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
    o <- tryCatchWE(plyr::ddply(y$y, .variables, getFeaturesW, allArgs = allArgs, .parallel = TRUE))

    # Shut down the cluster
    parallel::stopCluster(cl)
    
    # If we have an error, then stop
    if (is(o$value, "error")) {
      stop(o$value)
    }
    
    # Extract and remove bogus warnings from the plyr package
    if (!is.null(o$warning)) {

      # Seemingly unique strings from the bogus warning
      bogus1 <- c("<anonymous>: ... may be used in an incorrect context:")
      bogus2 <- c(".fun(piece, ...)")

      # Determine whether these strings are present in any warnings
      vWarn <- function(x) {
        return(!(grepl(bogus1, x, fixed = TRUE) & grepl(bogus2, x, fixed = TRUE)))
      }

      # Identify the valid warnings
      validWarnings <- unlist(lapply(o$warning, vWarn))

      # Retain valid warnings
      o$warning <- o$warning[validWarnings]

      # If there any warnings left, issue them
      if (length(o$warning)) {
        nothing <- lapply(o$warning, warning)
      }
      
    } # If there are warning messages

    # Extract the computed features
    o <- o$value

  } # If we're processing in parallel

  # Non-parallel processing
  else {

    o <- plyr::ddply(y$y, .variables, getFeaturesW, allArgs = allArgs)
      
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
