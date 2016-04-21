roxygen2::roxygenize()
## assumes XR, XRPython parallel directories, should check
ndoc <- readLines("../XR/man/noscalar.Rd")
writeLines(ndoc, "./man/noscalar.Rd")
## this writes a version in /tmp that has to be hand-edited to add
## roxygen-style documentation.
proxyFile <- file("/tmp/proxyClasses.R", "w")
setPythonClass("list", save = proxyFile)
setPythonClass("dict", save = proxyFile)
close(proxyFile)
