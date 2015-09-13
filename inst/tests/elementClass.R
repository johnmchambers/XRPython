###HIDE
require(XRPython)
parse <- PythonFunction("parse", "xml.etree.ElementTree")
setPythonClass("ElementTree", module = "xml.etree.ElementTree")
setwd(system.file(package = "shakespeare"))
###SHOW
hamlet <- parse("./plays/hamlet.xml")
Element <- setPythonClass("Element",
                  module = "xml.etree.ElementTree",
                  example = hamlet$getroot())
