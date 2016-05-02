

## setupRoxygen.R  Create Roxygen documentation, copy doc from XR

## See ?setupStep in the XRPython documentation for a discussion of this script.
## This script should only be run through XR::packageSetup()

target <- "R/pythonProxyClasses.R"
if(file.exists(target) && any(grepl("list_Python", readLines(target)))) {
    ## second stage:  create the documentation from roxygen

    roxygen2::roxygenize()
    ## assumes XR, XRPython parallel directories, should check
    ndoc <- readLines("../XR/man/noscalar.Rd")
    writeLines(ndoc, "./man/noscalar.Rd")
    message("roxygen documentation written; reinstall to update help file")
} else
    stop("The R source file pythonProxyClasses.R should have been created by the setup script \"setupClasses.R\".")
