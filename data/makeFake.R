# Create some fake data that can be easily used to illustrate the concepts

# These data will have 5 continuous variables and 3 discrete variables, will be separated into 3 phases, and will
# have 5 examples (or observations of potentially different lengths

singleOb <- function(obname, n) {

  # obname = the name (label) of the observation
  # n = number of observations
  scaleFactor <- rpois(1, lambda = 50)

  d <- data.frame(unit = obname,
                  phase = as.factor(sort(rep_len(1:3, n))),
                  contA = rnorm(n, mean = scaleFactor * 10, sd = scaleFactor * 2),
                  contB = log(pmax(1, rnorm(n, mean = scaleFactor  * 1000, sd = 70))),
                  contC = rgamma(n, shape = 2, scale = scaleFactor),
                  contD = rpois(n, lambda = scaleFactor / 2),
                  contE = rbeta(n, 0.5, 0.5) * scaleFactor,
                  discA = sample(rep_len(1:4, n)),
                  discB = rbinom(n, 1, runif(1, 0.3, 0.7)),
                  discC = sample(rep_len(letters[1:3], n)))
 
  return(d)

} # singleOb
