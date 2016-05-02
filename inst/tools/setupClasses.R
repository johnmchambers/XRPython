## setupClasses.R Script to write R/pythonProxyClasses.R

## See ?setupStep in the XRPython documentation for a discussion of this script.
## This script should only be run through XR::packageSetup()
    proxyFile <- file("R/pythonProxyClasses.R", "w")
    setPythonClass("list", save = proxyFile, docText = readLines("inst/tools/listDoc.txt"))
    writeLines(readLines("inst/tools/listMethods.R"), proxyFile)
    setPythonClass("dict", save = proxyFile, docText = readLines("inst/tools/dictDoc.txt"))
    close(proxyFile)

    ## first stage, list_Python will not be defined & so could not be exported
    namesp <- readLines("./NAMESPACE")
    if(!any(grepl("list_Python", namesp))) {
        nspFile <- file("./NAMESPACE", "a")
        writeLines("exportClass(list_Python, dict_Python)", nspFile)
        close(nspFile)
    }
    message("pythonProxyClasses.R written plus exports; reinstall the package")

