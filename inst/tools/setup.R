roxygen2::roxygenize()
## assumes XR, XRPython parallel directories, should check
ndoc <- readLines("../XR/man/noscalar.Rd")
writeLines(ndoc, "./man/noscalar.Rd")
proxyFile <- file("/tmp/proxyClasses.R", "w")
setPythonClass("list", save = proxyFile)
setPythonClass("dict", save = proxyFile)
close(proxyFile)
