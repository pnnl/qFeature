#Unit Test
#  Author: Landon Sego
#  Maintainer(s): Landon Sego <Landon.Sego@pnnl.gov>
#  Created: 2015-09-04
#  Last Updated: 2015-09-04

#Test Summary
# The purpose is to verify that the slower version which uses a different algorithm, fitQslow(), gives
# results equivalent to the faster vesrion, fitQ()

context("fitQ() equivalence to fitQslow()")

test_that("fitQ() produces equivalent results to fitQslow()", {

  # Generate some data
  x <- stats::rnorm(32)
  x[c(2,5,10:15)] <- NA
  x1 <- -2:4

  # Calculate pairs of objects to compare and test their equivalence
  tester <- function(min.window = 3, start = 1, skip = 1, linear.only = FALSE) {

      # Calculate with fast and slow
      fast <- fitQ(x, x1, min.window = min.window, start = start, skip = skip, linear.only = linear.only)
      slow <- fitQslow(x, x1, min.window = min.window, start = start, skip = skip, linear.only = linear.only)

      # Reformat output of fast to match slow
      fast <- as.matrix(as.data.frame(fast))

      # Replace NA's with -99999
      fast[is.na(fast)] <- -9999
      slow[is.na(slow)] <- -9999

      # Calculate the maximum absolute difference
      result <- max(abs(fast - slow))

      # Verify that difference is within 10 decimal places of equality
      # I used this instead of "expect_equal(fast, slow)" because I found it was behaving quite strangely
      expect_identical(round(result, 10), 0)
      
  } # tester

  # Run the test over a variety of parameter combinations
  tester(min.window = 3, start = 1, skip = 1, linear.only = FALSE)
  tester(min.window = 5, start = 1, skip = 1, linear.only = FALSE)
  tester(min.window = 3, start = 4, skip = 1, linear.only = FALSE)  
  tester(min.window = 3, start = 2, skip = 2, linear.only = FALSE)
  tester(min.window = 4, start = 3, skip = 3, linear.only = FALSE)
  tester(min.window = 3, start = 1, skip = 1, linear.only = TRUE)
  tester(min.window = 5, start = 1, skip = 1, linear.only = TRUE)
  tester(min.window = 3, start = 4, skip = 1, linear.only = TRUE)  
  tester(min.window = 7, start = 2, skip = 2, linear.only = TRUE)
  tester(min.window = 4, start = 3, skip = 3, linear.only = TRUE)  

}
