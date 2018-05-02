### Test interfaces to some Python functions and classes in a module.
### The module is part of the 'shakespeare' package (github.com/johnmchambers/shakespeare)
### but copied here to the "shakespeareTest" directory of package 'XRPython'
library(XRPython)
source(system.file("tests", "mustbe.R", package = "XRPython"))
parse <- PythonFunction("parse", "xml.etree.ElementTree")
## define a proxy class for the parsed python object
ElementTree <- setPythonClass("ElementTree", module = "xml.etree.ElementTree")

hamlet <- parse(system.file("shakespeareTest", "plays", "hamlet.xml",
                            package = "XRPython") )
ev <- RPython()
ev$AddToPath(system.file("shakespeareTest", "python", package = "XRPython"))
getSpeeches <- PythonFunction("getSpeeches", "thePlay")
Speech <- setPythonClass("Speech", "thePlay")
speeches <- getSpeeches(hamlet)
mustbe(ev$Call("len",speeches), 1138)
firstsp <- speeches$el(0)
mustbe(unlist(ev$Get(firstsp$lines)), "Who's there?")
last <- speeches$pop()
mustbe(last$speaker, "PRINCE FORTINBRAS")
mustbe(ev$Call("len",speeches), 1137)
speeches$append(last) # put it back
mustbe(ev$Call("len",speeches), 1138)
Element <- setPythonClass("Element",
                          module = "xml.etree.ElementTree",
                          example = hamlet$getroot())
