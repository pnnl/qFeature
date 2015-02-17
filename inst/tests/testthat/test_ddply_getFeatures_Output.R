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

context("ddply_getFeatures() - Output Values")

require(qFeature)
test_that("ddply_getFeatures() output has not changed from the archived output file", {
  
  #Reproduceable data set
  set.seed(10)
  cont1 <- rnorm(100,9,1)
  cont2 <- runif(100,0,10)
  
  #String elements to sample from for producing discrete vectors
  disc1Element <- c("T", "F")
  disc2Element <- c("red", "blue", "yellow")
  disc3Element <- c("1", "2", "3")
  
  #Generate discrete vectors by sampling from discElements
  disc1 <- sample(disc1Element, 100, replace=TRUE)
  disc2 <- sample(disc2Element, 100, replace=TRUE)
  disc3 <- sample(disc3Element, 100, replace=TRUE)
  
  #Combine vectors into dataset
  ddplyGetFeaturesEx <- data.frame(cont1, cont2, disc1, disc2, disc3)
  
  
  #Calculate features using getFeatures()
  outddplyGetFeatures <- ddply_getFeatures(ddplyGetFeaturesEx, c("disc1", "disc2"),
                                           cont = 1:2, disc = 5, 
                                           stats = c("mean", "sd"), 
                                           fitQargs = list(x1 = -5:5), nJobs = 2)
  
  #Compare results of getFeatures() to validation data
  expect_that(outddplyGetFeatures, equals_reference(file = "validationData/ddplygetfeatures_ValidationData.rds"))
})