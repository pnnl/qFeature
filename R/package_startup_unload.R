# This function would be called when namespace is unloaded (if it were not in the search path
# because it had been loaded via loadNamespace("qFeature")
.onUnload <- function(libpath) {

  if (is.loaded("sp_table", PACKAGE = "qFeature")) {
  
    confirm <- try(library.dynam.unload("qFeature", libpath), silent = TRUE)
    
    if (class(confirm) != "try-error") {
      cat("qFeature shared objects are unloaded\n")
    }
    else {
      cat(confirm,"\n")
    }
  }
}

