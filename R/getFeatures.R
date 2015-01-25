##' Fits the moving window regression model and summarizes results by phases
##' 
##' Fits the moving window regression model over a single vector of data and
##' summarizes results by phases
##'
##' A least one of \code{cont} or \code{disc} must be specified.
##' 
##' @export
##' 
##' @param x Data frame
##' 
##' @param cont Vector of integers or character vector indicating the columns
##' of \code{x} that correspond to variables that will be treated as continuous
##'
##' @param disc Vector of integers or character vector indicating the columns
##' of \code{x} that correspond to variables that will be treated as discrete
##'
##' @param stats Character vector of summary statistics, which are passed to
##' \code{\link{summaryStats}}.
##'
##' @param \dots Arguments for \code{\link{fitQ}}.
##' 
##' @return A named vector containing the features for each of the variables
##' requested in \code{cont} and \code{disc}.  The names follow the form
##' [varname].[description], where the [varname] is specified in \code{cont} and
##' \code{disc}, and [description] follows the naming convention produced by
##' code{\link{summary.fitQ}} \code{\link{discFeatures}}.
##' 
##' @author Landon Sego
##'
## @examples

getFeatures <- function(y, cont = NULL, disc = NULL,
                        stats = c("min", "q1", "mean", "med", "q3", "max", "sd", "count"),
                        ...) {

  # Basic sanity checks
  stopifnot(is.data.frame(y),
            !(is.null(cont) & is.null(disc)),
            is.character(stats))

  # Make sure 'cont' and 'disc' are the way we want them, if getFeatures() is called
  # from the global environment
  pe <- parent.frame()
  
  if (environmentName(pe) == "R_GlobalEnv") {
    vars <- checkInputs(colnames(y), cont, disc)
    cont <- vars$cont
    disc <- vars$disc
  }

  # Make sure we have character inputs
  stopifnot(is.character(cont),
            is.character(disc))

  # Initialize the output vector
  out <- NULL
  
  # Process continuous variables
  if (!is.null(cont)) {

    # Define the summary stats function
    sumStats <- summaryStats(stats)

    # Calculate the features for continuous variable
    out <- unlist(lapply(cont, function(x) summary(fitQ(y[,x], ...), stats = sumStats)))
    
    ## for (cv in cont) {
    ##   assign(cv, summary(fitQ(y[,cv], ...), stats = sumStats, vname = cv))
    ## }

    ## out <- Smisc::qbind(cont, type = "c")
      
  }

  # Process the discrete variables
  if (!is.null(disc)) {

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


## # Load the data
data(demoData)

# Now run over a subset
d <- demoData[demoData$subject == 3 & demoData$phase == "b",]
colnames(d)
f <- getFeatures(d, cont = 2:4, disc = 7:8, stats = c("mean", "sd"), x1 = -5:5)


