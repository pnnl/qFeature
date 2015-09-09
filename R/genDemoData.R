##' Generate the demo data for the qFeature package
##'
##' Generate the demo data for the qFeature package
##'
##' A call to \code{genDemoData} produces the same data as that given by \code{data(demoData)}. This approach allows us
##' to export this function and generate the data easily from other packages via \code{qFeature::genDemoData}
##'
##' @export
##' 
##' @return 'data.frame':	646 obs. of  11 variables.  See example below.
##'
##' @author Landon Sego
##' 
##' @examples
##' # Generate the data
##' x <- genDemoData()
##' str(x)
##'
##' # Compare to loading via data()
##' data(demoData)
##' identical(demoData, x)
##'
##' # Remove the objects
##' rm(x, demoData)

genDemoData <- function() {

  # To create the data for a subject and phase
  singleOb <- function(inVec) {

    n <- inVec[["n"]]
      
    # n = number of observations
    scaleFactor <- stats::rpois(1, lambda = 50)
  
    d <- data.frame(subject = inVec[["subject"]],
                    phase = inVec[["phase"]],
                    contA = stats::rnorm(n, mean = scaleFactor * 10, sd = scaleFactor * 2),
                    contB = log(pmax(1,
                            stats::rnorm(n, mean = c(1:(n %/% 2), seq((n %/% 2 + 1), 1, by = -1)) ^ scaleFactor + 10,
                            sd = scaleFactor * 10))),
                    contC = stats::rgamma(n, shape = 2, scale = scaleFactor),
                    contD = (1:n) ^ 2 * stats::rpois(n, lambda = scaleFactor / 10),
                    contE = stats::rbeta(n, 0.5, 0.5) * scaleFactor,
                    discA = sample(rep_len(1:4, n)),
                    discB = as.logical(stats::rbinom(n, 1, stats::runif(1, 0.3, 0.7))),
                    discC = as.factor(sample(rep_len(letters[10:12], n))),
                    discD = sample(rep_len(letters[24:26], n)),
                    stringsAsFactors = FALSE)
   
    return(d)
  
  } # singleOb
  
  
  # Basic Data set structure
  ds <- expand.grid(subject = as.factor(1:7), phase = letters[5:7])
    
  # Set the seed so results are repeatable
  set.seed(893)
  
  # Add the number of observations for each
  ds$n <- pmax(15, stats::rpois(nrow(ds), 30))

  # Generate the actual data
  for (i in 1:nrow(ds)) {

    assign(paste("d", i, sep = ""), singleOb(ds[i,]))
      
  }

  # Row bind them all together at once
  return(Smisc::qbind(paste("d", 1:nrow(ds), sep = "")))

} # genDemoData
