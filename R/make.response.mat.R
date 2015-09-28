# Wrapper function to quickly build the response matrix
# where each column corresponds a moving window of the vector X.

#  Landon Sego, January 30, 2008

##' Converts a vector into a matrix where each column is a moving window
##' 
##' Converts a vector into a matrix where each column is a moving window
##' 
##' Near the beginning and end of the series, the moving window is padded with
##' \code{NA}'s as necessary.
##'
##' This function is not exported and is used only by \code{\link{fitQslow}}, which,
##' in turn, exists for validation of \code{\link{fitQ}}.
##'
##' @param X The vector to be processed
##' @param bw The bandwidth of the moving window, where the length of the
##' moving window is \code{2*bw + 1}.
##' @return A matrix, where each column contains the elements of the
##' progressing moving window across \code{X}.
##' @author Landon Sego
##' @seealso This function is used in \code{\link{fitQslow}}
##' @keywords misc
## @examples
## \dontrun{make.response.mat(rnorm(10), bw = 4)}

make.response.mat <- function(X, bw = 5) {

  win.len <- 2 * bw + 1
  
  matrix(.C("responseMat",
            as.integer(bw),
            as.integer(win.len),
            as.integer(length(X)),
            as.double(X),
            Xnew=double(length(X) * win.len),
            NAOK = TRUE)$Xnew,
         nrow=win.len)

} # make.response.mat
