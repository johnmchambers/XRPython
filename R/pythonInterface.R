

PythonInterface <- setRefClass("PythonInterface",
                                 contains = "Interface",
                                 fields = list(
                                     operators = "character",
                                     engine = "ANY"
                                 )
                              )

PythonInterface$methods(
    initialize = function(...) {
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

PythonObject <- setRefClass("PythonObject",
                            contains = "ProxyClassObject")

pythonSend <- function(object, evaluator = XR::getInterface(.PythonInterfaceClass))
    evaluator$Send(object)

pythonGet <- function(object, evaluator = XR::getInterface(.PythonInterfaceClass))
    evaluator$Get(object)

pythonAddToPath <- function(...,  evaluator = XR::getInterface(.PythonInterfaceClass))
    evaluator$AddToPath(...)

