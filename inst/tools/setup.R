roxygen2::roxygenize()
## assumes XR, XRPython parallel directories, should check
ndoc <- readLines("../XR/man/noscalar.Rd")
writeLines(ndoc, "./man/noscalar.Rd")
