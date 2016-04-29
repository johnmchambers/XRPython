## See ?setupStep in the XRPython documentation for a discussion of this script.
## This script should only be run through XR::packageSetup()
target <- "R/pythonProxyClasses.R"
if(file.exists(target) && any(grepl("list_Python", readLines(target)))) {
    ## second stage:  create the documentation from roxygen

    roxygen2::roxygenize()
    ## assumes XR, XRPython parallel directories, should check
    ndoc <- readLines("../XR/man/noscalar.Rd")
    writeLines(ndoc, "./man/noscalar.Rd")
}
proxyFile <- file("R/pythonProxyClasses.R", "w")
setPythonClass("list", save = proxyFile, docText = readLines("inst/tools/listDoc.txt"))
writeLines(readLines("inst/tools/listMethods.R"), proxyFile)
setPythonClass("dict", save = proxyFile, docText = readLines("inst/tools/dictDoc.txt"))
close(proxyFile)

## first time, list_Python will not be defined & so could not be exported
