### qFeature

An R package for extracting features from continuous or discrete time series.  These features can then be used as inputs to multivariate statistical procedures like clustering, dimensionality reduction, and classification. `qFeature` constructs the features by using moving windows of regression fits for continuous variables and by summarizing the duration and transistion features of discrete variables. This is a high-speed implementation of the feature extraction methods of the Morning Report Algorithms developed by Brett Amidan and Tom Ferryman.

You can learn more about the algorithms of `qFeature` in the [vignette](http://pnnl.github.io/qFeature).

#### Installation instructions

Begin by installing dependencies from [CRAN](http://cran.r-project.org):

    install.packages(c("devtools", "plyr", "moments", "foreach", "doParallel", "knitr", "mvbutils"))

The `Smisc` package (a dependency of `qFeature`) and `qFeature` itself contain C code and require compilation. To do this
* on a Mac, you'll need [Xcode](https://developer.apple.com/xcode/) 
* on Windows, you'll need to install [R tools](http://cran.r-project.org/bin/windows/Rtools/)
* on Linux, compilation should take place "automatically"

With the compilation tools in place, you can now install dependencies and the package itself 
from [the PNNL github site](http://github.com/pnnl):

    devtools::install_github("pnnl/Smisc")
    devtools::install_github("pnnl/qFeature")

#### Getting started

The vignette for the `qFeature` package is the principal resource for understanding what the package does.  After installing
the package, you can can browse the package and the vignette as follows:

    library(qFeature)
    browseVignettes("qFeature")

And a list of all the package functions can be found this way:

    help(package = qFeature)
    
And this will provide citation information:

    citation("qFeature")

### Contributing

We welcome contributions to this package.  Please follow [these steps](http://pnnl.github.io/qFeature/developerInstructions.html)
when contributing.
