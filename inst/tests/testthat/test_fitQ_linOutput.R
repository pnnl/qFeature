#Unit Test
#  Author: Lucas Tate <lucas.tate@pnnl.gov>
#  Maintainer(s): Lucas Tate <lucas.tate@pnnl.gov>
#  Created: Feb 13, 2015
#  Last Updated: Feb 13, 2015

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
#  The purpose of this test is to perform a spot check of an
#  individual window to verify that the features extracted
#  a, b, c, and d match values produced by manually fitting
#  a linear regression model.

context("fitQ() - Linear Output Values")

require(qFeature)
test_that("fitQ() correctly calculates features", {
  
  #Data
  set.seed(10)
  yPar <- rnorm(20, 10, 1)
  x1Par <- -3:3
  
  #Manual Computations of Window 6
  compLinear <- lm(yPar[3:9] ~ x1Par)
  
  #Extract features from manual computation of Window 6
  compLinearA <- summary(compLinear)$coefficients[1,1]
  compLinearB <- summary(compLinear)$coefficients[2,1]
  compLinearC <- NA
  compLinearD <- mean(summary(compLinear)$sigma)
  
  #Use fitQ() and extract
  fitQLinear <- fitQ(y = yPar, x1 = x1Par, linear.only = TRUE)
  
  #Extract features from fitQ computation of Window 6
  fitQLinearA <- fitQLinear$a[6]
  fitQLinearB <- fitQLinear$b[6]
  fitQLinearC <- fitQLinear$c[6]
  fitQLinearD <- fitQLinear$d[6]
 
  #Compare manual computation to fitQ computation
  expect_that(fitQLinearA, equals(compLinearA))
  expect_that(fitQLinearB, equals(compLinearB))
  expect_that(fitQLinearC, equals(compLinearC))
  expect_that(fitQLinearD, equals(compLinearD))
  
})