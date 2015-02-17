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
#  The purpose of this test is to verify the output
#  from the discFeatures function matches manually
#  calculated values.

context("discFeatures() - Output Values")

require(qFeature)
test_that("disFeatures() correctly calculates duration at each phase and transitions", {
  
  phasePar <- c(rep(letters[4:6],2), rep(letters[6:5],3))
  
  #Compute percent duration at each phase
  compPctD <- length(phasePar[phasePar == "d"])/length(phasePar)
  compPctE <- length(phasePar[phasePar == "e"])/length(phasePar)
  compPctF <- length(phasePar[phasePar == "f"])/length(phasePar)
  
  #Create data frame to hold transition data
  compTranDF <- data.frame(Prior = numeric(0), Posterior = numeric(0))
  
  #Populate transition dataframe by looking at moving window of length 2
  for(i in 1:(length(phasePar)-1)) {
    compTranDF[nrow(compTranDF)+1,] <- c(phasePar[i], phasePar[i+1])
  }
  
  #Sum counts of each transition type
  deTran <- sum(compTranDF$Prior=="d" & compTranDF$Posterior=="e")
  dfTran <- sum(compTranDF$Prior=="d" & compTranDF$Posterior=="f")
  edTran <- sum(compTranDF$Prior=="e" & compTranDF$Posterior=="d")
  efTran <- sum(compTranDF$Prior=="e" & compTranDF$Posterior=="f")
  fdTran <- sum(compTranDF$Prior=="f" & compTranDF$Posterior=="d")
  feTran <- sum(compTranDF$Prior=="f" & compTranDF$Posterior=="e")
  
  #Calculate duration and transitions using discFeatures()
  discFeatCalc <- as.numeric(discFeatures(phasePar))
  
  #Compare computed valued to discFeatures()
  expect_that(discFeatCalc[1], equals(compPctD))
  expect_that(discFeatCalc[2], equals(compPctE))
  expect_that(discFeatCalc[3], equals(compPctF))
  expect_that(discFeatCalc[4], equals(deTran))
  expect_that(discFeatCalc[5], equals(dfTran))
  expect_that(discFeatCalc[6], equals(edTran))
  expect_that(discFeatCalc[7], equals(efTran))
  expect_that(discFeatCalc[8], equals(fdTran))
  expect_that(discFeatCalc[9], equals(feTran))
  
})