## Various tests of defining Python functions
library(XRPython)
testf <- function(...) system.file("tests", ...,package = "XRPython" )
source(testf("mustbe.R"))
## a single def from a file
fib1 <- pythonDefine(file = testf("fib1.py"))
mustbe(fib1(10L), 55L)

ev <- RPython()
## multiple def's on one file: the Define() method.

fib2 <- ev$Define(file = testf("fib2.py"))
fib3 <- PythonFunction("fib3") # defined, but not returned 
mustbe(fib2(11), fib3(11))

## inline text; 1-line python def
fib4 <- ev$Define("def fib4(n): return fib2(n) - 1")
mustbe(fib4(10L), 54L)

## define a function that calls to an imported module
## Probably needs anaconda, on the mac?

xtraPython <- "~/anaconda2/lib/python2.7/site-packages"
here <- tryCatch(setwd(xtraPython),
                 error = function(e)e)
{if(is(here, "error"))
  message("Couldn't switch to anaconda2; sympy may be unavailable")
else {
  pythonAddToPath(getwd())
  setwd(here)
  }
}

imp <- tryCatch(ev$Import("sympy"), error = function(e)e)
{if(!is(imp, "error")) {
  rat <- ev$Define("def rat(x,y): return sympy.Rational(x,y)")
  hf <- rat(1L,2L)
  mustbe(ev$Call("float",hf), 0.5)
}
else
  message(
    gettexf("test of def using imported sympy module not run:  could not import: %s",
            e$message))
}


  
