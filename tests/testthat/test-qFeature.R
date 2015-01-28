## Preliminaries to set up the environment

data(demoData)

d <- demoData[demoData$subject == 7 & demoData$phase == "b",]

cNamesN <- grep("cont", colnames(demoData))
dNamesN <- grep("disc", colnames(demoData))
cNamesC <- colnames(demoData)[cNamesN]
dNamesC <- colnames(demoData)[dNamesN]

# Insert some "random" NAs
d$contC <- NA
d$discD <- NA
d[c(3,5,12,27), "contB"] <- NA
d[c(1,10:20), "contA"] <- NA
d[c(7,15,22), "contC"] <- NA

allStats <- c("min", "q1", "mean", "med", "q3", "max", "sd", "sum", "ss",
              "count", "skew", "kurt")

# Generate various calls to getFeatures()
res1 <- getFeatures(d, cont = cNamesN, disc = dNamesC, stats = allStats,
                    fitArgs = list(x1 = -4:4, min.window = 3, start = 2, skip = 2))

res2 <- getFeatures(d, cont = cNamesC, disc = dNamesN, stats = allStats,
                    fitArgs = list(x1 = 0:4, min.window = 1, start = 3, skip = 2))

res1 <- getFeatures(d, cont = cNamesN, disc = dNamesC, stats = allStats,
                    fitArgs = list(x1 = -4:4, min.window = 3, start = 2, skip = 2))

res1 <- getFeatures(d, cont = cNamesN, disc = dNamesC, stats = allStats,
                    fitArgs = list(x1 = -4:4, min.window = 3, start = 2, skip = 2))

res1 <- getFeatures(d, cont = cNamesN, disc = dNamesC, stats = allStats,
                    fitArgs = list(x1 = -4:4, min.window = 3, start = 2, skip = 2))



context("")
                        
test_that("Numerical values are as expected", {



  expect_equal(sp.table(x, 1:2), c(5, 3))
  expect_equal(sp.table(x, 0:3), c(0, 5, 3, 0))
  expect_equal(sp.table(x, 1:3, pct = TRUE), c(0.625, 0.375, 0))

})

