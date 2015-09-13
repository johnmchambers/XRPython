require(shakespeare)
require(XRPython)
ev <- RPython()
speeches <- PythonFunction("getSpeeches", "thePlay")
hamlet <- getPlays("hamlet")[[1]]
spHamlet <- speeches(hamlet)
## Speech <- setPythonClass("Speech", "thePlay") # already in package
PythonClassDef("Speech", "thePlay")
ev$ServerClassDef("Speech", "thePlay")
last <- spHamlet$pop()
last$speaker
spHamlet$append(last) # put it back
lastR <- ev$Get(last)
vectorR <- PythonFunction("vector_R", "RPython")
vectorR(last$lines, .get = TRUE)
## a method to do the same thing
ev$Get(last$getText())
## and a direct equivalent, illustrating that module RPython is imported
ev$Eval("RPython.vector_R(%s.lines)", last, .get = TRUE)
