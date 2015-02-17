#Unit Test
#  Author: Lucas Tate <lucas.tate@pnnl.gov>
#  Maintainer(s): Lucas Tate <lucas.tate@pnnl.gov>
#  Created: Feb 16, 2015
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
#  The purpose of this test is to verify the output
#  from the getFeatures function matches a previously
#  generated output file. The correctness of the
#  original values is assumed to be tested in the
#  fitQ() and discFeatures() tests.

context("getFeatures() - Output Values")

require(qFeature)
test_that("getFeatures() output has not changed from the archived output file", {
  
  #Reproduceable data set
  set.seed(10)
  cont1 <- rnorm(10,9,1)
  cont2 <- runif(10,0,10)
  disc1 <- discData <- c("T", "F", "F",
                         "T", "T", "T",
                         "F", "T", "F",
                         "T")
  disc2 <- c("blue", "red", "yellow",
             "yellow", "blue", "red",
             "blue", "red", "yellow",
             "blue")
  getFeaturesEx <- data.frame(cont1, cont2, disc1, disc2)
  
  #Calculate features using getFeatures()
  outgetFeatures <- getFeatures(getFeaturesEx, cont = 1:2, disc = 3:4, stats = c("mean", "sd"), fitQargs = list(x1 = -3:3))
  
  #Compare results of getFeatures() to validation data
  expect_that(outgetFeatures, equals_reference(file = "validationData/getfeatures_ValidationData.rds"))
})