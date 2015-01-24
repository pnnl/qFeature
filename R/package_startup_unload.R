# For R-2.14.0 and beyond
.onAttach <- function(libname, pkgname) {

  banner.text <- paste("\nWelcome to the calcIVT package. ",
                       "Technical documentation of the IVT algorithms can be found in\n",
                       path.package(package="calcIVT"),
                       "/doc/Description_of_enhanced_IVT_algorithms_2008-02-01.pdf.\n", sep="")

  packageStartupMessage(banner.text)
  
}

# This function would be called when namespace is unloaded (if it were not in the search path
# because it had been loaded via loadNamespace("calcIVT")
.onUnload <- function(libpath) {

  if (is.loaded("sp_table", PACKAGE = "calcIVT")) {
  
    confirm <- try(library.dynam.unload("calcIVT", libpath), silent = TRUE)
    
    if (class(confirm) != "try-error")
      cat("calcIVT shared objects are unloaded\n")
    else
      cat(confirm,"\n")
  }
}

