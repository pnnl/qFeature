# Create some fake data that can be easily used to illustrate the concepts in the
# qFeature package
demoData <- function() {

  # To create the data for a subject and phase
  singleOb <- function(inVec) {

    n <- inVec[["n"]]
      
    # n = number of observations
    scaleFactor <- rpois(1, lambda = 50)
  
    d <- data.frame(subject = inVec[["subject"]],
                    phase = inVec[["phase"]],
                    contA = rnorm(n, mean = scaleFactor * 10, sd = scaleFactor * 2),
                    contB = log(pmax(1,
                            rnorm(n, mean = c(1:(n %/% 2), seq((n %/% 2 + 1), 1, by = -1)) ^ scaleFactor + 10,
                            sd = scaleFactor * 10))),
                    contC = rgamma(n, shape = 2, scale = scaleFactor),
                    contD = (1:n) ^ 2 * rpois(n, lambda = scaleFactor / 10),
                    contE = rbeta(n, 0.5, 0.5) * scaleFactor,
                    discA = sample(rep_len(1:4, n)),
                    discB = as.logical(rbinom(n, 1, runif(1, 0.3, 0.7))),
                    discC = as.factor(sample(rep_len(letters[1:3], n))),
                    discD = sample(rep_len(letters[1:3], n)),
                    stringsAsFactors = FALSE)
   
    return(d)
  
  } # singleOb
  
  
  # Basic Data set structure
  ds <- expand.grid(subject = as.factor(1:7), phase = letters[1:3])
  
  # Add the number of observations for each
  ds$n <- pmax(15, rpois(nrow(ds), 30))

  # Set the seed so results are repeatable
  set.seed(893)

  # Generate the actual data
  for (i in 1:nrow(ds)) {

    assign(paste("d", i, sep = ""), singleOb(ds[i,]))
      
  }

  # Row bind them all together at once
  return(Smisc::qbind(paste("d", 1:nrow(ds), sep = "")))

} # demoData

# Create the dataframe
demoData <- demoData()
