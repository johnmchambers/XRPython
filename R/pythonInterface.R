
#' An Interface to Python
#'
#' The PythonInterface class provides an evaluator for computations in Python, following the structure
#' in the XR  package.  Proxy functions and classes allow use of the interface with no explicit
#' reference to the evaluator.  The function \code{RPython()} returns an evaluator object.
PythonInterface <- setRefClass("PythonInterface",
                                 contains = "Interface",
                                 fields = list(
                                     operators = "character",
                                     engine = "ANY"
                                 )
                              )

PythonInterface$methods(
    initialize = function(...) {
        'On the first call, adds the python directory of this package to the search path in Python,
and imports the Python functions used in the interface methods.'
        languageName <<- "Python"
        obj <- PythonObject()
        obj$.ev <- .self
        prototypeObject <<- obj
        modules <<- new.env(parent = emptyenv())
        operators <<- .pythonOperators
        callSuper(...)
        args <- list(...)
        if(!length(serverPath)) {
            ## execute some low-level commands the first time
            PythonCommand("import sys")
            AddToPath()
            PythonCommand(
                "from RPython import getMethods, classStructure, arglist_for_R, function_for_R, objectFromJSON"
            )
            PythonCommand(
                "from RPython import value_for_R, del_for_R, pickle_for_R, unpickle_for_R, start_unpickle, end_unpickle, vector_R")
        }
     },
    ServerEval = function(strings, key = "", get = NA) {
             pySend <- { if(is.na(get)) "None"
                          else if(get) "True"
                          else "False"}
             pyExpr <- { if(is.character(strings)) deparse(strings)
                         else deparse(as.character(strings)) # does this ever happn?
                     }
             expr <- gettextf("_R_value = value_for_R(%s, %s, %s)",
                             pyExpr, deparse(key), pySend)
             rPython::python.exec(expr)
             string <- rPython::python.get("_R_value")
             XR::valueFromServer(string, key, get, .self)
         },
    ServerClassDef = function(Class, module = "", example = TRUE ) {
        PythonClassDef(Class,  module, example, evaluator = .self)
    },
    ServerFunctionDef = function(name, module ="") {
        PythonFunction(name, module, .ev = .self)
    },
    Define = function(text, file) {
        'Define a Python function from a character vector, `text` or by reading the text
from a file via readLines().  Character vectors are taken to represent lines of Python code
in a function definiition.  The method returns a proxy function with a name inferred from
the first line of the text.'
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
        ## try to parse the first line to get the function name
        fname <- text[[1]]
        fname <- gsub("[(].*","",gsub("^def  *", "", fname))
        if(grepl("[^_a-zA-Z]",fname)) # doesn't look like a function def.
            fname <- "" # At present, we just go ahead
        string <- paste(text, collapse = "\n")
        Command(string)
        if(nzchar(fname))
            PythonFunction(fname)
        else
            NULL
    },
    Source = function(filename) {
        'The $Source() method uses the Python function execfile() and therefore is quite efficient.'
        Command("execfile(%s)", filename)
    },
    ServerRemove = function(key) {
        Eval("del_for_R(%s)", key, .get = TRUE)
    },
    PythonCommand = function(strings) {
        'A low-level command execution, needed for initializing.  Normally should not be used by applications
since it does no error checking; use $Command() instead.'
        rPython::python.exec(strings)
    },
    ServerSerialize = function(key, file) {
        'Serializing and unserializing in the Python interface use the pickle structure in Python.
Serialization does not rely on the R equivalent object.'
        Command("pickle_for_R(%s, %s)", key, file)
    },
    ServerUnserialize = function(file, all) {
        value <- Eval("start_unpickle(%s)", file, .get = FALSE)
        on.exit(Command("end_unpickle()"))
        repeat {
            obj <- Eval("unpickle_for_R(%s)", value, .get = TRUE)
            if(identical(obj, FALSE)) # => EOF
                break
            value@size <- value@size + 1L
        }
        if(!all && (value@size == 1L))
            value <- Eval("%s[0]", value, .get = FALSE)
        value
    },
    ServerAddToPath = function(serverDirectory, serverPos) {
        if(is.na(serverPos))
          PythonCommand(paste0("sys.path.append(", shQuote(serverDirectory), ")"))
        else
          PythonCommand(paste0("sys.path.insert(",as.integer(serverPos), ", ",
                               shQuote(serverDirectory), ")"))
    },

    ## replaces the XR method for Import()
    Import = function(module, ...)  {
        'The Python version of this method replaces the general version in XR with the "import" or
"from ... import" directives in Python as appropriate.'
        members <- unlist(c(...))
        imported <- base::exists(module, envir = modules)
        if(imported && is.null(members)) # the usual case
            return(TRUE)
        ## has this been done before?
        if(imported) {
            hasDone <- base::get(module, envir = modules)
            if(length(members)) {
                if(all(members %in% hasDone))
                    return(TRUE)
            }
            else if ( ".IMPORT" %in% hasDone)
                return(TRUE)
        }
        else hasDone <- character()
        if(length(members))
            code <- paste("from", module, "import", paste(members, collapse = ", "))
        else {
            code <- paste("import", module)
            members <- ".IMPORT"
        }
        Command(code)
        base::assign(module, unique(c(members, hasDone)), envir = modules)
        return(members)
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
)

.PythonInterfaceClass <- getClass("PythonInterface")

#' An Evaluator for the Python Interface.
#'
#' Returns an evaluator for the Python interface.  Starts one on the first call, or if arguments are provided;
#' providing argument \code{.makeNew = TRUE} will force a new evaluator.  Otherwise, the current evaluator is
#' returned.
#'
#' See \code{\link{PythonInterface}} for details of the evaluator.
RPython <- function(...)
    XR::getInterface(.PythonInterfaceClass, ...)


.pythonOperators <- c("+", "-", "*", "/", "**", "%", "^", "&", "|", "<", "<<",
                      ">", ">>", "^")
# and the assignment forms or "or equal" forms
.pythonOperators <- c(.pythonOperators,
                      paste0(.pythonOperators, "="))
# and some miscellaneous others
.pythonOperators <- c(.pythonOperators, "<>", ".=", "~")
## (we let the Python parser handle which have unary forms or not)

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
#' @name functions
#' @param object an R object, to be sent to Python (\code{pythonSend()}) or a proxy object for
#' the Python object to be converted (\code{pythonGet()}).
#' @param evaluator The evaluator object to use.  By default, and usually, the current evaluator
#' is used, and one is started if none has been.
NULL

#' @describeIn functions
#' sends the \code{object} to Python, converting it via methods for
#' \code{\link[XR]{asServerObject}} and returns a proxy for the converted object.
pythonSend <- function(object, evaluator = XR::getInterface(.PythonInterfaceClass))
    evaluator$Send(object)

#' @describeIn functions
#' evaluates the \code{expr} string subsituting the arguments.  See the corresponding evaluator
#' method for details.
pythonEval <- function(expr, ..., evaluator = XR::getInterface(.PythonInterfaceClass))
    evaluator$Eval(expr, ...)

#' @describeIn functions
#' evaluates the \code{expr} string subsituting the arguments; used for a command that is not
#' an expression.
pythonCommand <- function(expr, ..., evaluator = XR::getInterface(.PythonInterfaceClass))
    evaluator$Command(expr, ...)

#' @describeIn functions
#' converts the proxy object that is its argument to an \R{} object.
pythonGet <- function(object, evaluator = XR::getInterface(.PythonInterfaceClass))
    evaluator$Get(object)

#' @describeIn functions
#' evaluate the expression in Python, with substitution
pythonEval <- function(expr, ..., evaluator = XR::getInterface(.PythonInterfaceClass))
    evaluator$Eval(expr, ...)

#' @describeIn functions
#' evaluate the command in Python, with substitution
pythonCommand <- function(expr, ..., evaluator = XR::getInterface(.PythonInterfaceClass))
    evaluator$Command(expr, ...)

#' @describeIn functions
#' serialize the \code{object} in Python, via \code{pickle}
pythonSerialize <- function(object,  file, append = FALSE, evaluator = XR::getInterface(.PythonInterfaceClass))
    evaluator$Serialize(object, file, append)

#' @describeIn functions
#' unserialize the file in Python, via \code{pickle}
pythonUnserialize <- function(file, all = FALSE, evaluator = XR::getInterface(.PythonInterfaceClass))
    evaluator$Unserialize(file, all)

#' Import a Python module or add a directory to the Python Search Path
#'
#' adds the module information specified to the modules imported for Python evaluators.
#'
#' If called from the source directory of a package during installation, both \code{pythonImport}
#' and \code{pythonAddToPath()} also set up
#' a load action for that package.  The functional versions, not the methods themselves, should
#' be called from package source files to ensure that the load actions are created.
#' @param ...  arguments for the \code{$Import()} method. See the method documentation for details.
pythonImport <- function( ...,  evaluator) {
    if(missing(evaluator))
        XR::serverImport("PythonInterface", ...)
    else
        evaluator$Import(...)
}

#' @describeIn pythonImport
#' adds the directory specified to the search path for Python objects.
#' @param directory the directory to add, defaults to "python"
#' @param package,pos arguments \code{package} and \code{pos} to the method, usually omitted.
#' @param evaluator The evaluator object to use. Supplying this argument suppresses the load action.
pythonAddToPath <- function(directory = "python", package = utils::packageName(topenv(parent.frame())), pos = NA,  evaluator) {
    if(missing(evaluator))
        XR::serverAddToPath("PythonInterface", directory, package, pos)
    else
        evaluator$AddToPath(directory, package, pos)
}

## Conditionally arrange to use XML package to send XML objects
ns <- tryCatch(loadNamespace("XML"), error = function(e) NULL)
if(!is.null(ns)) {
    setMethod("asServerObject",
    c("XMLInternalDocument", "PythonObject"),
          function(object, prototype) {
              file <- tempfile()
              XML::saveXML(object, file)
              gettextf("xml.etree.ElementTree.parse(%s)",
                       asServerObject(file, prototype))
          })
}
