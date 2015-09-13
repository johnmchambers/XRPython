require(XRPython)
parse <- PythonFunction("parse", "xml.etree.ElementTree")
setwd(system.file(package = "shakespeare"))
hamlet <- parse("./plays/hamlet.xml")
ev <- RPython()
ev$AddToPath(system.file("python", package = "shakespeare"))
getSpeeches <- PythonFunction("getSpeeches", "thePlay")
Speech <- setPythonClass("Speech", "thePlay")
speeches <- getSpeeches(hamlet)
speeches

