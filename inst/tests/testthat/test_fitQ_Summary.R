#Unit Test
#  Author: Lucas Tate <lucas.tate@pnnl.gov>
#  Maintainer(s): Lucas Tate <lucas.tate@pnnl.gov>
#  Created: Feb 13, 2015
#  Last Updated: Feb 16, 2015

#R (@ Last Updated)
#  R Version: R version 3.1.2 (2014-10-31) "Pumpkin Helmet"

#Package Under Test (@ Last Updated)
#  Package: qFeature
#  Version: 0.1.1 (2015-02-13)
#  Author(s): Landon Sego <Landon.Sego@pnnl.gov>
#  Maintainer(s): Landon Sego <Landon.Sego@pnnl.gov>
#  Imports: Smisc, plyr, stats
#  Suggests: testthat, moments

#Test Summary
#  The purpose of this test is to verify the summary
#  statistics (min, q1, mean, med, q3, max, sd, count) 
#  produced using the summary method on a fitQ object.

context("fitQ() - Summary Values")

require(qFeature)

test_that("fitQ() correctly calculates summary statistics of features", {
  
  #Data
  set.seed(10)
  yPar <- rnorm(10, 10, 1)
  x1Par <- -3:3
  minPar <- 4
  
  #Data frame to hold manual computation of features 
  compQuadDF <- data.frame(compQuadA = numeric(), compQuadB = numeric(), compQuadC = numeric(), compQuadD = numeric())
  
  #Function to extract features from manual computations
  compABCD <- function(compQuad) {
    compQuadA <- summary(compQuad)$coefficients[1,1]
    compQuadB <- summary(compQuad)$coefficients[2,1]
    compQuadC <- summary(compQuad)$coefficients[3,1]
    compQuadD <- mean(summary(compQuad)$sigma)
    
    return(c(compQuadA, compQuadB, compQuadC, compQuadD))
  }
  
  #Function to calculate summary statistics for a vector
  compSumStats <- function(x) {
    return(c(min(x),
             quantile(x)[2],
             mean(x),
             median(x),
             quantile(x)[4],
             max(x),
             sd(x),
             length(x)))
  }
  
  #Manual computation for windows 1-3 (leading edge)
  for(i in 1:3) {
    compQuad <- lm(yPar[1:(i+3)] ~ x1Par[(5-i):7] + I(x1Par[(5-i):7]^2))    
    compQuadDF[nrow(compQuadDF)+1,] <- compABCD(compQuad)
  }
  
  #Manual computation for windows 4-7 (complete windows)
  for(i in 1:4) {
    compQuad <- lm(yPar[i:(i+6)] ~ x1Par + I(x1Par^2))
    compQuadDF[nrow(compQuadDF)+1,] <- compABCD(compQuad)
  }
  
  #Manual computation for windows 8-10 (trailing edge)
  for(i in 1:3) {
    compQuad <- lm(yPar[(i+4):10] ~ x1Par[1:(7-i)] + I(x1Par[1:(7-i)]^2))
    compQuadDF[nrow(compQuadDF)+1,] <- compABCD(compQuad)
  }
  
  #Compute summary statistics from manual computations
  compSummary <- c(compSumStats(compQuadDF[,1]), 
                   compSumStats(compQuadDF[,2]), 
                   compSumStats(compQuadDF[,3]), 
                   compSumStats(compQuadDF[,4]))

  #Use fitQ() to produce summary statistics across windows.
  fitQSummary <- summary(fitQ(y = yPar, x1 = x1Par, min.window = minPar))
  
  #Compare manual computations to fitQ computations
  for(i in 1:32) {
    expect_that(as.numeric(fitQSummary[i]), equals(as.numeric(compSummary[i])))
  }
  
})