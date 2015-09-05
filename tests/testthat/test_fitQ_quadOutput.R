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
#  a quadratic regression model.

context("fitQ() - Quadratic Output Values")

test_that("fitQ() correctly calculates features", {
  
  #Data
  set.seed(10)
  yPar <- rnorm(20, 10, 1)
  x1Par <- -3:3
  
  #Manual Computations of Window 6
  compQuad <- lm(yPar[3:9] ~ x1Par + I(x1Par^2))
  
  #Extract features from manual computation of Window 6
  compQuadA <- summary(compQuad)$coefficients[1,1]
  compQuadB <- summary(compQuad)$coefficients[2,1]
  compQuadC <- summary(compQuad)$coefficients[3,1]
  compQuadD <- mean(summary(compQuad)$sigma)
  
  #Use fitQ() and extract
  fitQQuad <- fitQ(y = yPar, x1 = x1Par)
  
  #Extract features from fitQ computation of Window 6
  fitQQuadA <- fitQQuad$a[6]
  fitQQuadB <- fitQQuad$b[6]
  fitQQuadC <- fitQQuad$c[6]
  fitQQuadD <- fitQQuad$d[6]
 
  #Compare manual computation to fitQ computation
  expect_that(fitQQuadA, equals(compQuadA))
  expect_that(fitQQuadB, equals(compQuadB))
  expect_that(fitQQuadC, equals(compQuadC))
  expect_that(fitQQuadD, equals(compQuadD))
  
})