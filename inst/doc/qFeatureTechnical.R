## ----eval=FALSE----------------------------------------------------------
#  # Install needed packages from CRAN
#  install.packages(c("devtools", "plyr", "moments", "foreach", "doParallel", "knitr", "mvbutils"))

## ----eval=FALSE----------------------------------------------------------
#  # Install the Smisc and qFeatures packages
#  devtools::install_github("pnnl/Smisc")
#  devtools::install_github("pnnl/qFeature")

## ----message = FALSE-----------------------------------------------------
library(qFeature)

## ----eval=FALSE----------------------------------------------------------
#  help(package = qFeature)

## ----echo = FALSE--------------------------------------------------------
# Set viewing options in the vignette
options(scipen = 999)

## ----package_structure_plot, message=FALSE, warning=FALSE, echo=FALSE----
library(mvbutils)

set.seed(9)

op <- options(warn = -1)
foodweb(where="package:qFeature",
        border="#00CCFF", boxcolor = "#E6FAFF",
        funs=c("ddply_getFeatures",
                "getFeatures",
                "discFeatures",
                "fitQ"),
        descendents = FALSE,
        ancestors = FALSE,
        expand.xbox=1.2, expand.ybox=3,
        textcolor="black", cex=1, lwd=1)
options(op)

## ------------------------------------------------------------------------
discData <- c("TRUE", "FALSE", "FALSE", NA, "TRUE", "TRUE", NA, NA, "TRUE", "FALSE", "TRUE", "FALSE", "TRUE")
discData

## ------------------------------------------------------------------------
discFeatures(discData)

## ------------------------------------------------------------------------
set.seed(10)
fitqDataEx1 <- rnorm(7, 5, 1)
fitqDataEx1

## ------------------------------------------------------------------------
fitQ(y = fitqDataEx1, x1 = -3:3, min.window = 4)

## ------------------------------------------------------------------------
y <- fitqDataEx1[1:5]
x1 <- c(-1:3)
summary(lm(y ~ x1 + I(x1^2)))

## ------------------------------------------------------------------------
set.seed(20)
fitqDataEx2 <- rnorm(7, 5, 1)
fitqDataEx2

## ------------------------------------------------------------------------
fitQ(y = fitqDataEx2, x1 = -3:3, min.window = 5)

## ------------------------------------------------------------------------
set.seed(30)
fitqDataEx3 <- rnorm(15, 5, 1)
fitqDataEx3[c(5,7, 9, 10)] <- NA
fitqDataEx3

## ------------------------------------------------------------------------
fitQ(y = fitqDataEx3, x1 = -3:3, min.window = 4)

## ------------------------------------------------------------------------
Window7 <- fitqDataEx3[4:10]
Window7

Window8 <- fitqDataEx3[5:11]
Window8

## ------------------------------------------------------------------------
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
getFeaturesEx

## ------------------------------------------------------------------------
outGetFeatures <- getFeatures(getFeaturesEx, cont = 1:2, disc = 3:4, 
                              stats = c("mean", "sd"), fitQargs = list(x1 = -3:3))
outGetFeatures

## ------------------------------------------------------------------------
data(demoData)
str(demoData)

## ------------------------------------------------------------------------
f <- ddply_getFeatures(demoData, c("subject", "phase"),
                       cont = 3:4, disc = 8, stats = c("mean", "sd"),
                       fitQargs = list(x1 = -5:5), nJobs = 2)

str(f)

## ------------------------------------------------------------------------
head(f)

## ---- eval = FALSE-------------------------------------------------------
#  file.path(path.package("qFeature"), "doc", "Explanation_of_qFeature_algorithms.pdf")

