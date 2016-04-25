## write a version in /tmp  for testing
proxyFile <- file("/tmp/proxyClasses.R", "w")
setPythonClass("list", save = proxyFile, docText = readLines("inst/tools/listDoc.txt"))
setPythonClass("dict", save = proxyFile, docText = readLines("inst/tools/dictDoc.txt"))
close(proxyFile)
roxygen2::roxygenize()
## assumes XR, XRPython parallel directories, should check
ndoc <- readLines("../XR/man/noscalar.Rd")
writeLines(ndoc, "./man/noscalar.Rd")
