## The structure of a Python class
## There are two ways to ask for the structure of a class in a module:
## 1- supply un- or partly quaified name of class and module= argument
## 2- supply the fully qualified name and empty module
## In accordance with Python, the first imports Class from module, the second imports module

.classStructure <- function(Class, module = "", example = TRUE, evaluator = RPython()) {
    ev <- evaluator
    names <- strsplit(Class, ".", TRUE)[[1]]
    ln <- length(names)
    qualifiedName <- !nzchar(module)
    if(ln < 2) {
        className <- Class
    }
    else {
        className <- names[[ln]]
        if(nzchar(module))
            module <- paste(module, names[-ln], collapse = ".")
        else
            module <- paste(names[-ln], collapse = ".")
    }
    if(nzchar(module)) {
        if(qualifiedName)
            ev$Import(module, className)
        else
            ev$Import(module)
    }
    ev$Import("RPython", "classStructure")
    if(is(example, "AssignedProxy") || is(example, "ProxyClassObject")) {
        ## Class <- example@serverClass
        ## module <- example@module
        obj <- example
    }
    else if(is.character(example) || identical(example, TRUE)) {
        if(identical(example, TRUE)) {
            genFun <- Class
            if(nzchar(module))
                genFun <- paste(module, genFun, sep = ".")
            example <- gettextf("%s()", genFun)
            if(nzchar(module))
                example <- c(gettextf("import %s", module), example)
        }
        obj <- tryCatch(ev$Eval(example), error = function(e)e)
        if(is(obj, "error")) {
            message(gettextf(
                "Example (%s) failed for class %s (%s)\nOnly class information will be used",
                             example, dQuote(Class), obj$message))
            obj <- ev$Eval(Class)
            useInstance <- FALSE
        }
    }
    else
        obj <- ev$Eval(Class) # should get the class object
    value <- ev$Call("classStructure", obj, .get = TRUE)
    ## Python empty lists will not be converted to character
    if(!length(value$fields))
        value$fields <- value$fieldTypes <- character()
    ## the classStructure result includes types for the fields
    ## but Python has no guarantees, so "ANY" is the safe choice
    if(is.list(value$fields)) {
        fields <- value$fields
        ## this is over cautious: classStructure always gives named list
        fnames <- allNames(fields)
        fields[nzchar(fnames)] <- list("ANY")
        value$fields <- fields
    }
    ## construct the proxy method definitions
    value$methods <- .makePythonMethods(obj, value$methods, module, ev)
    value$module <- module
    value$Class <- className
    value$ev <- ev
    value
}

.makePythonMethods <- function(obj, mnames, module, ev) {
    ## obj is expected to be some sort of proxy object  (Note that if we are
    ## REdefining the proxy class it may be from the current <class>_Python class)
    method <- asServerObject(obj)
    value <- lapply(mnames,
                    function(name) {
                        PythonFunction(name, method = method, .ev = ev)
                    })
    names(value) <- mnames
    value
}


.classModule <- function(Class) {
   c(Class, module)
}

#' Class and Generator for Python Class Description from Python Metadata
#'
#' Creates a class definition object consistent with the XR structure.
#' See \code{\link{setPythonClass}} for details and arguments to the generator for the class.
#' @param evaluator the interface evaluator; by default and usually, the current Python evaluator.
PythonClassDef <- setRefClass("PythonClassDef",
                              fields = list(
                                  className = "character",
                                  module = "character"),
                              contains = "ServerClassDef"
                              )

PythonClassDef$methods(
                       initialize = function(Class, module = "", example = TRUE, evaluator = RPython()) {
                           if(nargs()) {
                               info <- .classStructure(Class, module, example, evaluator)
                               fieldnames <- info$fields
                               fieldTypes <- as.list(info$fieldTypes)
                               names(fieldTypes) <-fieldnames
                               fields <<- as(fieldTypes, "namedList")
                               module <<- info$module
                               className <<- info$Class
                               methods <<- as(info$methods, "namedList")
                           }
                           else {
                               className <<- ""
                               module <<- ""
                           }
                       },
    show = function() {
        showF <- function(ff)
            methods::show(matrix(unlist(ff), 1, dimnames = list("Python Class", names(ff))))
        cat("Python Class:", className)
        if(nzchar(module))
            cat("; module: ", module)
        cat("\n")
        if(length(fields)) {
            cat("Fields:\n")
            showF(fields)
        }
        if(length(methods))
            cat("Methods: ", paste(names(methods), collapse = ", "), "\n")
        cat("\n")
    },
                       .methodArgs = function(mnames, evaluator) {
                           prefix <- paste0(if(nzchar(module)) paste0(module, ".")
                                            else "", className)
                           lapply(mnames, function(x) { #either a proxy or a name
                               if(is.character(x))
                                   evaluator$pythonArgs(x, prefix, method =  TRUE)
                               else
                                   x
                           })
                       }
                       )
#' Create a Proxy Class for a Python Class
#'
#' An R class is defined to act as a proxy for a specified Python class.  This specializes the
#' \code{\link[XR]{setProxyClass}} function using Python facilities for finding the class definition.
#'
#' The methods and (inferred) fields of a Python Class are determined and returned consistently
#' with the XR structure.
#' Python classes are coded as class objects in Python, but only the methods are fixed and defined.
#' Objects from the class can have any fields, usually created at initialization time but entirely legal
#' to be added by other methods later.  By default, the initialize method tries to create an object from the
#' class, with no arguments in the call to the class generator.   Supply the \code{example} argument to
#' override.
#'
#' @param Class the Python name for the class.
#' @param module the Python module, if this is not a standard library class.
#' @param example an optional (proxy for) an object from the class, to be used to define the fields in the
#' class.  If omitted, the interface tries to create a standard object from the class by calling the Python
#' generator with no argument.  Argument \code{example} can also be supplied as \code{FALSE} to suppress
#' generating the default object.
#' @param fields,methods arguments to \code{setProxyClass} and usually omitted.
#' @param ServerClass,contains,proxyObjectClass ditto.
setPythonClass <- function(Class, module = "",
                           fields = character(), methods = NULL,
                           ServerClass = Class,
                           where = topenv(parent.frame()),
                           contains = character(),
                           proxyObjectClass = "PythonObject",
                           ...,
                           example = TRUE) {
    XR::setProxyClass(Class, module, fields, methods, ServerClass = ServerClass,
        where = where, contains = contains, evaluatorClass = "PythonInterface",
        proxyObjectClass = proxyObjectClass, language = "Python", ..., example = example)
}

allIndices <- function(k) {
    tf <- c(TRUE, FALSE)
    value <- matrix(tf, 1,2)
    for(i in 2:k)
        value <- cbind(rbind(value, TRUE), rbind(value, FALSE))
    value
}

.allSubsets <- function(what) {
    k <- length(what)
    xx <- allIndices(k)
    lapply(seq(length.out = ncol(xx)), function(j)what[xx[,j]])
}

.funEvalText <- function(leftText, moduleText, reqArgs, optArgs, dots) {
    formals <- c(reqArgs, optArgs)
    if(dots)
        formals <- c(formals, "...")
    if(length(formals))
        formals <- paste0(paste(formals, collapse = ", "), ", ")
    else
        formals <- ""
    text <- paste0("function(", formals, ".ev = XRPython::RPython()) {")
    if(nzchar(moduleText))
        text <- c(text, moduleText)
    if(length(reqArgs)) {
        leftText <- paste0(leftText, ", ",paste(reqArgs, collapse = ", "))
        argText <- paste(reqArgs, collapse = ", ")
    }
    else
        argText <- character()
    nopt <- length(optArgs)
    if(nopt) {
        subs <- .allSubsets(optArgs)
        labels <- paste0("    .", sapply(subs, function(label) paste(label, collapse =".")))
        format <- sapply(subs, function(what) {
            if(length(what))paste(what, what, sep=" = ", collapse =", ")
            else ""
        })
        args <- sapply(subs, function(what) paste(what, collapse = ", "))
        if(dots)
            args <- paste0(args, ",...")
        text <- c(text,
                  paste0("    case <- !base::c(", paste0("missing(", optArgs, ")", collapse = ", "),")"
                         ),
                  paste0("    labels <- base::c(", paste(shQuote(optArgs), collapse = ", "), ")"),
                  "    label <- paste0('.', paste0(labels[case], collapse = '.'))",
                  "    switch(label,")
        leftText <- paste(labels, leftText, sep = " = ")
        leftText <- paste0(leftText, ifelse(nzchar(format), ", ", ""), format, ")," )
        ## args[nzchar(args)] <- paste0(", ", args[nzchar(args)])
        ## argText <- paste0(argText, args )
        ## leftText <- paste0(leftText, ", ", argText, "),")
        text <- c(text, leftText, "    NULL)")
    }
    else {
        if(dots)
            leftText <- paste0(leftText, ", ...")
        text <- c(text, paste0(leftText, ")"))
    }
    c(text, "}")
}

#' Proxy Objects in R for Python Functions
#'
#' A class and generator function for proxies in R for Python functions.
#'
#' An object from this class is an R function that is a proxy for a function in Python. Calls to the R function evaluate
#' a call to the Python function.  The arguments in the call are converted to equivalent Python objects;
#' these typically include proxy objects for results previously computed through the XRPython interface.
#' @slot name the name of the server language function
#' @slot module the name of the module, if that needs to be imported
#' @slot evaluatorClass the class for the evaluator, by default and usually, \code{\link{PythonInteface}}
#' @slot serverDoc the docstring from Python, if any.
#'
#' @slot serverArgs the Python argument names (not currently used).
PythonFunction <- setClass("PythonFunction",
                           contains = "ProxyFunction")

setMethod("initialize", "PythonFunction",
          function(.Object, name, module = "", method = "", ...,
                   .ev = RPython(), .get = NA){
              ## the escape to avoid requiring Python:  work
              ## up through XR::ProxyFunction to set slots directly
              if(methods::hasArg(".Data"))
                  return(callNextMethod(.Object, name = name, module = module, ..., .get = .get))
              if(missing(name))
                  return(.Object) # the no-arguments case
              if(nzchar(method)) { # should be the server expr for the object
                  fname <- gettextf("%s.%s", method, name)
                  method <- name # bad choice of arg. name; really "proxy obj. for methods"
              }
              else if(nzchar(module)) {
                  .ev$Import(module)
                  fname <- paste(module, name, sep=".")
              }
              else {
                  fname <- name
                  ## module may be included in name
                  if(grepl(".", fname, fixed = TRUE)) {
                      module <- gsub("[.][^.]*$", "", fname)
                      .ev$Import(module)
                  }
              }
              .ev$Import("RPython")
              .ev$Import("inspect")
              info <- .ev$Eval(gettextf("function_for_R(%s)",fname), .get = TRUE)
              f <- .proxyFun(name, fname, module, method, info, .get)
              environment(f) <- environment(.Object@.Data)
              .Object@.Data  <- f
              args <- as.character(info$args)
              if(info$nopt) {
                  n <- length(args)
                  nreq <- n - info$nopt
                  opt <- seq_len(n) > nreq & args != "..."
                  args <- paste0(args, ifelse(opt, " =",""))
              }
              .Object@serverArgs <- args
              .Object@name <- name
              .Object@module <- module
              .Object@evaluatorClass <- class(.ev)
              .Object@serverDoc <- as.character(.ev$Eval(gettextf("inspect.getdoc(%s)", fname), .get = TRUE))
              callNextMethod(.Object, name, module, .Data = f, evaluator = .ev, ..., .get = .get)
          })

setMethod("show", "PythonFunction",
          function(object) {
              cat(gettextf("Proxy for Python function %s, from module %s\n",
                           nameQuote(object@name), nameQuote(object@module)))
              show(object@.Data)
          })

.proxyFun <- function(name, fname, module, method, info, .get) {
    args <- as.character(info$args)
    n <- length(args)
    nopt <- info$nopt
    if(nzchar(method)) {
        callText <- gettextf("    .ev$MethodCall(.proxyObject,%s,..., .get = .get)", shQuote(method))
        n <- n-1 # get rid of Python's "self" argument
    }
    else
        callText <- gettextf("    .ev$Call(%s,..., .get = .get)", shQuote(name))
    if(nzchar(module)) {
        if(name == fname) # module was in the name
            moduleText <- gettextf("    .ev$Import(%s)", shQuote(module))
        else # need to "import name from module"
            moduleText <- gettextf("    .ev$Import(%s, %s)", shQuote(module), shQuote(name))
    }
    else
        moduleText <- ""
    text <- gettextf("function(..., .ev = XRPython::RPython(), .get = %s) {", .get)
    if(nopt < n || !info$dots)
        text <- c(text, "    nPyArgs <- nargs() - !missing(.ev)")
    if(nopt < n) {
        nreq <- as.integer(n - nopt)
        text <- c(text, gettextf(
            '    if(nPyArgs  < %d)\nstop("Python function %s() requires at least %d %s; got ",nPyArgs)',
            nreq, name, nreq, if(nreq>1) "arguments" else "argument" ))
    }
    if(!info$dots)
        text <- c(text, gettextf(
            '    if(nPyArgs  >  %d)\nstop("Python function %s() only allows %d %s; got ",nPyArgs)',
            n, name, n, if(n==1) "argument" else "arguments" ))
    if(nzchar(module))
        text <- c(text, moduleText)
    text <- c(text, callText, "}")
    eval(parse(text = text)[[1]])
}


