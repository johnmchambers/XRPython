
#' An Interface to Python
#'
#' The \code{"PythonInterface"} class provides an evaluator for computations in Python, following the structure
#' in the XR  package.  Proxy functions and classes allow use of the interface with no explicit
#' reference to the evaluator.  The function \code{RPython()} returns an evaluator object.
#'
#' The class extends the \code{"Interface"} class in the XR package and has the same fields.
#' Python-specific methods use the rPython low-level interface.  See the Chapter from the
#'  \dQuote{Extending R} book in the documents for this package for details.
PythonInterface <- setRefClass("PythonInterface",
                                 contains = "Interface",
                               methods = list(
    initialize = function(...) {
       'The Python version, with special defaults for prototypeObject and modules'
        languageName <<- "Python"
        obj <- PythonObject()
        obj$.ev <- .self
        prototypeObject <<- obj
        modules <<- new.env(parent = emptyenv())
        callSuper(...) # initialize object
        startupActions() # actions for search path, imports
     },
    ServerEval = function(strings, key = "", get = NA) {
       'The Python version using value_for_R()'
             pySend <- { if(is.na(get)) "None"
                          else if(get) "True"
                          else "False"}
             if(!is.character(strings)) # does this ever happen?
                   strings <- as.character(strings)
             strings <- paste(strings, collapse ="\n") 
             pyExpr <- deparse(strings)
             expr <- gettextf("_R_value = value_for_R(%s, %s, %s)",
                             pyExpr, deparse(key), pySend)
             PythonCommand(expr)
             string <- reticulate::py_eval("_R_value") #? reticulate has no version of python.get()
             XR::valueFromServer(string, key, get, .self)
         },
    ServerClassDef = function(Class, module = "", example = TRUE ) {
       'The Python version using PythonClassDef()'
        PythonClassDef(Class,  module, example, evaluator = .self)
    },
    ServerFunctionDef = function(name, module ="") {
       'The Python version using PythonFunction()'
        PythonFunction(name, module, .ev = .self)
    },
    Define = function(text, file) {
        'Define a Python function from a character vector, `text` or by reading the text
from a file via readLines().  Character vectors are taken to represent lines of Python code
in a function definiition.  The method returns a proxy function with a name inferred from
the text. May be used to define multiple functions, but only the first will be returned.'
        if(missing(text)) {
            if(is(file, "connection")) {
                if(!isOpen(file))
                    open(file, "r")
                con <- file
            }
            else con <- base::file(file, "r")
            on.exit(close(con))
            text <- base::readLines(con)
        }
        ## try to parse "def" line(s) for function name(s)
        fname <- ""; globals <- character()
        for(string in text) {
            if(grepl("^def ", string)) {
                thisname <- gsub("[(].*","",gsub("^def  *", "", string))
                if(grepl("^[_a-zA-Z][_a-zA-Z0-9]*$",thisname)) {
                    globals <- c(globals, thisname)
                    if(!nzchar(fname))
                        fname <- thisname
                }
            }
        }
        if(!nzchar(fname))
            warning("No function definition found; text will be evaluated anyway")
        else if(length(globals) > 1)
            message(gettextf("Multiple function definitions found; only \"%s\" will be returned", fname))
        string <- paste(text, collapse = "\n")
        Command_forR(string)
        if(nzchar(fname))
            PythonFunction(fname)
        else
            NULL
    },
    Source = function(filename) {
        'The $Source() method uses the Python function execfile() and therefore is quite efficient.'
        Command_forR("execfile(%s,_for_R)", filename) # forR?
    },
    ServerRemove = function(key) {
       'The Python version using del_for_R())'
        Eval("del_for_R(%s)", key, .get = TRUE)
    },
    PythonCommand = function(strings) {
        'A low-level command execution, needed for initializing.  Normally should not be used by applications
since it does no error checking; use $Command() instead.'
        reticulate::py_run_string(strings, convert = FALSE)
    },
    ServerSerialize = function(key, file) {
        'Serializing and unserializing in the Python interface use the pickle structure in Python.
Serialization does not rely on the R equivalent object.'
        Command_forR("pickle_for_R(%s, %s)", key, file) # forR?
    },
                                   ServerUnserialize = function(file, all = FALSE) {
       'The Python unserialize using unpickle'
       value <- Eval("start_unpickle(%s)", file, .get = FALSE)
       size <- 0L
        on.exit(Command_forR("end_unpickle()")) # forR?
        repeat {
            obj <- Eval("unpickle_for_R(%s)", value, .get = TRUE)
            if(identical(obj, FALSE)) # => EOF
                break
            size <- size + 1L
        }
        if(!all && (size == 1L))
            value <- Eval("%s[0]", value, .get = FALSE)
        value
    },
    ServerAddToPath = function(serverDirectory, serverPos) {
       'The Python version using sys.path.append()'
        if(is.na(serverPos))
          PythonCommand(paste0("sys.path.append(", shQuote(serverDirectory), ")"))
        else
          PythonCommand(paste0("sys.path.insert(",as.integer(serverPos), ", ",
                               shQuote(serverDirectory), ")"))
    },

    ## replaces the XR method for Import()
    Import = function(module, ...)  {
        'The Python version of this method replaces the general version in XR with the "import" or
"from ... import" directives in Python as appropriate.  Returns the `reticulate` version of the module object, which can be used directly.
Use "*" to import all objects from the module.'
        members <- unlist(c(...))
        hasMembers <- length(members) > 0
        imported <- base::exists(module, envir = modules)
        if(imported) # the usual case
            mod <- base::get(module, envir = modules)
        else {
            mod <- do.call(reticulate::import, list(module))
            base::assign(module,mod , envir = modules)
            if(!hasMembers) # add the module by name to _for_R
                Command_forR(paste("import", module))
        }
        if(hasMembers) {
            ## TODO:  should remember what has been imported
            code <- paste("from", module, "import", paste(members, collapse = ", "))
            Command_forR(code)
        }
        return(mod)
    },

### start a python shell
    Shell = function(endCode = "exit", prompt = "Py>: ", cont = "Py+: ") {
        'Starts an interactive Python shell.  Each line of input must be a complete Python expression
or command, which will be evaluated in the same context as $Eval() expressions.
To continue over multiple lines, end all but the last with an unescaped backslash.
The argument `endCode` is the string to type to leave the shell, by default "exit".'
        callSuper(endCode, prompt, cont)
    },
    ## used by .classStructure(), but could also be used for proxy functions.
    pythonArgs = function(fName, prefix = "", method = FALSE) {
        code <- (if(nzchar(prefix)) paste(prefix, fName, sep = ".")
                 else fName)
        value <- Eval(gettextf("arglist_for_R(%s)", code), .get = TRUE)
        if(method) {
            if(length(value) < 1)
                warning(gettextf("Empty argument list is inconsistent with %s being a method, as asserted",
                                 code))
            else
                value <- value[-1]
        }
        if(any(substr(value,1,1) == "_")) {
            fixme <- substr(value,1,1) == "_"
            old <- value
            substr(value[fixme], 1, 1) <- "."
            warning(gettextf("Valid R names cannot start with \"_\": %s changed to %s",
                             paste(old[fixme], collapse = ", "), paste(value[fixme], collapse = ", ")))
        }
        value
    },

    pythonMethods = function(Class, module = "", needsImport = nzchar(module)) {
        if(needsImport)
            Import(module)
        if(nzchar(module))
            ClassName <- paste(module, Class, sep = ".")
        else
            ClassName <- Class
        code <- paste0("getMethods(",  ClassName, ")")
        Eval(code, .get = TRUE)
    }
))

## Additional methods
PythonInterface$methods(
                    Command_forR = function(expr, ...,forR = TRUE) {  # this will become the Command method
                        key <- if(forR) "forR" else ""
                        invisible(ServerEval(ServerExpression(expr, ...), key, FALSE))
                    }
)

.PythonInterfaceClass <- getClass("PythonInterface")

#' An Evaluator for the Python Interface.
#'
#' Returns an evaluator for the Python interface.  Starts one on the first call, or if arguments are provided;
#' providing argument \code{.makeNew = TRUE} will force a new evaluator.  Otherwise, the current evaluator is
#' returned.
#'
#' See \code{\link{PythonInterface}} for details of the evaluator.
#' @param ... arguments to control whether a new evaluator is started.  Normally omitted.
#' @examples
#' if(okPython(TRUE)) {
#'   ev <- RPython()
#'   xx <- ev$Eval("[1, %s, 5]", pi)
#'   xx
#'   xx$append(4.5)
#'   ev$Command("print %s", xx)
#' }
RPython <- function(...)
    XR::getInterface(.PythonInterfaceClass, ...)


#' Proxy Objects in R for Python Objects
#'
#' This is a class for all proxy objects from a Python class with an R proxy class definition.
#' Objects will normally be from a subclass of this class, for the specific Python class.
#'
#' Proxy objects returned from the Python interface will be promoted to objects
#' from a specific R proxy class for their Python class, if such a class has been defined.
PythonObject <- setRefClass("PythonObject",
                            contains = "ProxyClassObject")

#' Function Versions of Methods for Python Interface evaluators.
#'
#' These functions allow application code to invoke evaluator methods for essentially all basic computations.   Usually, they access
#' the current Python evaluator, starting one if none exists.  For details, see the documentation for the corresponding method, under
#' \link{PythonInterface}.
#' @name functions
#' @param object For \code{pythonSend()}, an R object, to be sent to Python.  For \code{pythonGet()},
#' \code{pythonSerialize()} and \code{pythonNqme()}, a proxy object for
#' a Python object.
#' @param evaluator The evaluator object to use.  By default, and usually, the current evaluator
#' is used, and one is started if none has been.
#' @param file A filename or an open connection, for reading or writing depending on the function
NULL

#' @describeIn functions
#' sends the \code{object} to Python, converting it via methods for
#' \code{\link[XR]{asServerObject}} and returns a proxy for the converted object.
pythonSend <- function(object, evaluator = XR::getInterface(.PythonInterfaceClass))
    evaluator$Send(object)

#' @describeIn functions
#' evaluates the \code{expr} string subsituting the arguments.
#'
#' @param expr A string for a Python expression or command, with C-style fields (\code{"\%s"}) to be substituted for the following arguments, if any.
#' @param ... For the evaluation functions: Objects, either R objects to be converted or proxies for Python objects previously computed.
#' For other functions, specialized arguments for the corresponding method.
#' In particular, \code{.get=} for controlling whether the computed result should be converted.
pythonEval <- function(expr, ..., evaluator = XR::getInterface(.PythonInterfaceClass))
    evaluator$Eval(expr, ...)

#' @describeIn functions
#' evaluates the \code{expr} string subsituting the arguments; used for a command that is not
#' an expression.
pythonCommand <- function(expr, ..., evaluator = XR::getInterface(.PythonInterfaceClass))
    evaluator$Command(expr, ...)

#' @describeIn functions
#' call the function in Python, with arguments given.
#' @param fun the string name of the function; a module name must be included in the string if the function has
#' not been explicitly imported from that module.
pythonCall <- function(fun, ..., evaluator = XR::getInterface(.PythonInterfaceClass))
    evaluator$Call(fun, ...)

#' @describeIn functions
#' converts the proxy object that is its argument to an \R{} object.
pythonGet <- function(object,  evaluator = XR::getInterface(.PythonInterfaceClass))
    evaluator$Get(object)

#' @describeIn functions
#' evaluate the file of Python source.
pythonSource <- function(file, ..., evaluator = XR::getInterface(.PythonInterfaceClass))
    evaluator$Source(file, ...)

#' @describeIn functions
#' define a Python function
#' @param text  the definition as text (supply argument  file= instead  to read it from a file)
pythonDefine <- function(text, file, ..., evaluator = XR::getInterface(.PythonInterfaceClass))
    evaluator$Define(text, file, ...)

#' @describeIn functions
#' serialize the \code{object} in Python, via \code{pickle}
#' @param append should the serializing text be appended to a file; otherwise the file will be truncated on opening.
pythonSerialize <- function(object,  file, append = FALSE, evaluator = XR::getInterface(.PythonInterfaceClass))
    evaluator$Serialize(object, file, append)

#' @describeIn functions
#' unserialize the file in Python, via \code{pickle}
#' @param all should the unserialized object be a list of all serialized objects on the file?
pythonUnserialize <- function(file, all = FALSE, evaluator = XR::getInterface(.PythonInterfaceClass))
    evaluator$Unserialize(file, all)

#' @describeIn functions
#' return the name by which this proxy object was assigned in Python
pythonName <- function(object)
    XR::proxyName(object)

#' @describeIn functions
#' Start an interactive Python shell.  See the chapter file in the documentation, section 14.3.
pythonShell <- function(..., evaluator = XR::getInterface(.PythonInterfaceClass))
    evaluator$Shell(...)

#' Import a Python module or add a directory to the Python Search Path
#'
#' If called from the source directory of a package during installation, both \code{pythonImport}
#' and \code{pythonAddToPath()} also set up
#' a load action for that package.  The functional versions, not the methods themselves, should
#' be called from package source files to ensure that the load actions are created.
#' @param evaluator The evaluator object to use. Supplying this argument suppresses the load action.
#' @name Modules
NULL

#' @describeIn Modules
#'
#' Add the module and name information specified to the objects imported for Python evaluators.
#' @param ...,where  arguments for the \code{$Import()} method. See the method documentation for details.
#' @examples
#' \dontrun{
#' ## How to search from a local directory, import a function from a file there
#' ## and call the function.
#' ## Including the evaluator argument causes the path change and import to happen
#' ## right now, not in a package being loaded
#' ev <- RPython()
#' pythonAddToPath("/Users/me/myPython/", package = "",
#'                 evaluator = ev)
#' pythonImport("funEx", "foo", evaluator = ev)
#' pythonCall("foo", 1.1, 1.2)
#' }
pythonImport <- function( ...,  evaluator,
                         where = topenv(parent.frame())) {
    if(missing(evaluator))
        XR::serverImport("PythonInterface", ..., where = where)
    else
        evaluator$Import(...)
}

#' Register an Evaluator Command or Expression at Initialization
#'
#' An unevalated command or expression for the interface is supplied, typically using
#' \code{quote()} or \code{substitute}.  When an evaluator from the class is created, this
#' command will be evaluated.  Repeated calls to this function, to \code{serverAddToPath()}
#' and to \code{serverImport()} will evaluate the corresponding requests, in the order in
#' which the corresponding calls took place (typically in the source of a pacakage).
#'
#' @param command an \emph{unevaluated} command or expression for the evaluator.
pythonTask <- function(command)
    XR::serverTask("PythonInterface", command)

#' @describeIn Modules
#' Add the directory specified to the search path for future Python objects.
#'
#' @param directory the directory to add, defaults to "python"
#' @param package,pos arguments \code{package} and \code{pos} to the method, usually omitted.
pythonAddToPath <- function(directory = "python", package = utils::packageName(topenv(parent.frame())), pos = NA,  evaluator,
                            where = topenv(parent.frame())) {

    if(missing(evaluator))
        XR::serverAddToPath("PythonInterface", directory, package, pos, where = where)
    else
        evaluator$AddToPath(directory, package, pos)
}

## the path and import requirements
## first, a low-level action to import "sys"
XR::serverTask("PythonInterface", quote(PythonCommand("import sys")))

pythonAddToPath()
XR::serverTask("PythonInterface", quote(PythonCommand(
                "from RPython import getMethods, classStructure, arglist_for_R, function_for_R, objectFromJSON"))
            )
XR::serverTask("PythonInterface", quote(PythonCommand(
                "from RPython import value_for_R, del_for_R, pickle_for_R, unpickle_for_R, start_unpickle, end_unpickle, vectorR")))

pythonImport("RPython", "getMethods", "classStructure", "arglist_for_R", "function_for_R", "objectFromJSON")
pythonImport("RPython", "value_for_R", "del_for_R", "pickle_for_R", "unpickle_for_R", "start_unpickle", "end_unpickle", "vectorR")

## Correct JSON's logical constants: always use objectFromJSON()
setMethod("asServerObject",
          c("logical", "PythonObject"),
          function(object, prototype) {
               jsonString <- objectAsJSON(object, prototype)
               if(is(jsonString, "JSONScalar")) # correct logical constant
                   gettextf("objectFromJSON(%s)",nameQuote(jsonString))
               else
                   gettextf("objectFromJSON(%s)",
                            typeToJSON(jsonString, prototype)) # add escapes in the string
           })

#' Class for General Python Class Objects
#'
#' The Python side of the interface will return a general object from a Python class as an R
#' object of class "from_Python".  Its Python fields (converted to R objects) can be accessed by the \code{$}
#' operator.
#'
#' @slot serverClass the Python type.
#' @slot module the Python module, or ""
#' @slot fields the converted versioin of the Python fields; these are accessed by the \code{$} operator.
setClass("from_Python", contains = "from_Server")

setMethod("initialize", "from_Python",
    function (.Object, ...)
    {
        .Object@language <- "Python"
        callNextMethod(.Object, ..., referenceClass = TRUE)
    }
)

#' The Setup Step
#'
#' The file \code{"setup.R"} in the \code{tools} directory is designed to create an explicit definiion of proxy
#' classes for the \code{"list"} and \code{"dict"} types in Python.  The file would be run through
#' \code{XR::packageSetup()} when creating or modifying these classes in the \code{XRPython} package.  It provides
#' a useful example for the general task of creating an explicit, written-out version of a proxy class.
#'
#' The setup step generates a file \code{"R/pythonProxyClasses.R"} in the source directory for the package.  The
#' setup step needs to be run twice, first to generate the R code in that file, and again to use the
#' \code{roxygen2} package to generate documentation.
#'
#' For the first round, the package needs to be installed with an empty version of the file (the file has to exist
#' because the package uses a \code{Collate:} directive that mentions it.  Running \code{packageSetup()} this time
#' defines the proxy classes and dumps them (with some extra stuff) to the target file.  and  adds a line to the
#' \code{NAMESPACE} to export both classes. (If we were willing to let \code{roxygenize()} create the namespace
#' directives this would be automatic, but I'm not willing.)
#'
#' Now the package needs to be installed again, this time with the proxy classes, The second pass of the setup file
#' runs \code{roxygenize()}.  Finally, as usual with \code{roxygenize()},
#' the package has to be installed one more time to generate the actual documentation.
#' @name setupStep
NULL

#' Send a Non-scalar Version of an Object
#'
#' Ensures that an object is interpreted as a vector (array) when sent to the server language.
#' The default strategy is to send length-1 vectors as scalars.
#' Copied from package XR.
#'
#' @return the object, but with the S4 bit turned on.
#' Relies on the convention that XR interfaces leave S4 objects
#' as vectors, not scalars, even when they are of length 1
#' @param object A vector object.  Calling with a non-vector is an error.
#' @name noScalar
NULL

#' Write a File of Python Commands to Test Package Modules in Python
#'
#' A file of python commands will be written that set up an interactive Python session
#' having imported the contents from a file (module) of python code in an R package.
#' Typically, uploading such a file to \code{ipython} notebook allows the python code, along with additional or modified code, to
#' be tested directly without interfacing from R.
#' @param file A file name or open write connection.  The python commands generated will be written to this file.
#' @param package The R package containing the relevent module
#' @param module The file (module) to be imported.  Specifically, a command \code{"from ... import *"} will be generated.
#' Omit this argument or supply it as \code{""} to suppress this command, in which case explicit commands should be provided.
#' @param ... Additional python commands to be appended to the output file.
#' @param RPython Should the path include the XRPython code, default \code{TRUE}, which is usually what you want.
#' @param folder The name of the folder in the installed package; the default is the suggested \code{"python"}; that is, the installed
#' version of folder \code{"inst/python"} in the source for the package.  Note that it's the installed version; changes to the source
#' code must be installed to show up in the output.
ipython <- function(file, package, module = "", ..., RPython = TRUE, folder = "python") {
    dots <- list(...)
    output <- "import sys"
    if(RPython)
        output <- c(output, paste0("sys.path.append(",nameQuote(system.file("python", package = "XRPython")),")"),
                    paste0("sys.path.append(", nameQuote(system.file(folder, package = package)), ")"))
    if(nzchar(module))
        output <- c(output, paste0("from ", module, " import *"))
    ## else expect commands in ...
    for( more in dots) {
        if(is.character(more))
            output <- c(output, more)
        else
            stop("... arguments should be python commands")
    }
    writeLines(output, file)
    invisible(output)
}

#' Test if a Proxy Object is an Instance of a Python Type
#'
#' Applies the Python function \code{isinstance()} to \code{object}.  NOTE:  this function should be used to test inheritance on the Python side,
#' even if there are proxy classes for everything involved.  It is not true (with the present version of the package) that inheritance in Python
#' corresponds to inheritance in R for the proxy classes.
#' @param object Any object.  The function returns \code{FALSE} without further testing if the object is not a proxy object.
#' @param type A character string corresponding to the Python type (not to the name of a proxy class for the type).
#'
#' A Python error will result
#' if there is no such type, or if \code{object} is a proxy from another language.
#' The implementation diverges from a direct mapping into the Python \code{isinstance} to handle a Python bizarre for functions:  although \code{type(f)}
#' causes you to think functions have the obvious type, that doesn't work in \code{isinstance}.  So the R code uses what works for this case.
#' (Before we get too sarcastic, the problem is similar to that in R from primitives, making \code{class(f)} and \code{typeof(f)} confusing.)
#' @param .ev an XRPython evaluator, by default and usually the current evaluator.
isinstance <- function(object, type, .ev = RPython()) {
    if(XR::isProxy(object))
        switch(type, `function` = .ev$Eval("callable(%s)", object),
               .ev$Eval("isinstance(%s,%s)", object, as.name(type)))
    else
        FALSE
}
 
#' Convert Proxy Objects between XRPython and reticulate
#'
#' Packages XRPython and reticulate both support proxies for Python objects; that is, R objects that are
#' proxies for objects created in Python by evaluations in the respective packages.  Function \code{fromRtclt()}
#' returns the equivalent XRPython proxy object given a reticulate object.
#' Function \code{toRtclt()} returns the equivalent reticulate proxy object given an XRPython object.
#' Normally, no copying is involved in either direction.
#'
#' @param obj a proxy object, computed in XRPython for \code{toRtclt} or by reticulate for \code{fromRtclt}
#' @param .ev an XRPython evaluator, by default and usually the current evaluator.
#' @name convert
NULL

#' @describeIn convert
#' Convert from reticulate to XRPython
fromRtclt <- function(obj, .ev = XRPython::RPython()) {
    rpy <- .ev$Import("RPython")
    key <- .ev$ProxyName()
    ## call as a reticulate expression to store obj, return proxy with this key
    robj <- rpy$proxy_or_object(obj, FALSE, key)
    XR::valueFromServer(robj, key, FALSE, .ev)
}

#' @describeIn convert
#' Convert from XRPython to reticulate
toRtclt <- function(obj, .ev = XRPython::RPython()) {
    key <- XR::proxyName(obj)
    reticulate::py_run_string("import RPython")
    reticulate::py_eval(gettextf("RPython._for_R[%s]", deparse(key)), convert = FALSE)
}

#' Check for a Valid Python for Interface
#'
#' The function returns true or false according to whether a Python interface can be established.  This
#' will fail if no Python exists, if it is incompatible with this version of XRPython (e.g., 32 vs 64 bits
#' in Windows), or if for some reason it can't evaluate a trivial expression correctly.  Warnings are printed
#' but ignored.
#'
#' @param verbose Should a message with the cause of a failure be reported?  Default \code{FALSE}.
okPython <- function(verbose = FALSE) {
    ev <- tryCatch(RPython(), error = function(e)e)
    if(is(ev, "PythonInterface")) {
        test <- tryCatch(ev$Eval("1"), error = function(e)e)
        if(identical(all.equal(test, 1), TRUE))
            TRUE
        else {
            if(verbose)
                message("Attempt to evalute 1 in Python got: ", test)
            FALSE
        }
    }
    else {
        if(verbose) {
            if(is(ev, "error"))
                message("Error in creating evaluator: ", conditionMessage(ev))
            else
                message("Expected PythonInterface object, got class: ", class(ev))
        }
        FALSE
    }
}
