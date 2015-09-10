\name{PythonInterface}
\alias{curPythonInterpreter}
\alias{evalPy}
\alias{getPy}
\alias{asPy}
\alias{deletePy}
\alias{pythonShell}
\alias{importPy}
\alias{callPy}
\alias{methodPy}
\alias{pythonPath}
\alias{PythonInterface}
\alias{PythonInterpreter-class}
\alias{optionalPythonInterpreter-class}
\alias{PythonObjectRef-class}
\alias{PythonObjectRef}
\title{
Functional Interface from R to Python
}
\description{
These functions provide a direct, middle-level interface from R to
Python.
The functions are intended to be used in a package providing a
specialized interface to particular computations in Python.
See the section \emph{Methods and Functions} below for the individual
functions and the corresponding methods for a Python interpreter object.
}
\usage{

curPythonInterpreter()

## Reference class "PythonInterpreter" has methods corresponding to
## the functions below, with the names shown in the comments, 
## and with the same arguments as the function

evalPy(format, ...) 

getPy(ref)

pythonPath <- function(directory = "python", package = NA, pythonPos = -1L)

asPy(obj)

deletePy(ref)

shell()

import(module, ...)

callPy(fun, ...)

toPython(obj)  # method

pythonMethod(object, method, ...) # method $method

## The remaining functions correspond to the low-level interface,
## and all have reference methods of exactly the same name

objectToPythonCode(obj)

evalPythonCode(strings)

pythonToR(var, type = "")


} %$
\arguments{

  \item{format}{
A string, a legitimate Python command.
Typically the command
incorporates some references to Python objects returned from previous
calls to \code{evalInPython()} or  \code{assignInPython()}.
These will be passed via the \dots{} arguments.

For simple calls to \code{evalPy()}, supply the arguments as names, but
with names not otherwise in the expression.
The dots arguments must have the corresponding names.
Be careful to use distinct names, since substitution is by simple
strings, not by parsing the expression.

For a more general solution, supply \code{format} as
a C-style format string.  The \dots{} arguments must now be unnamed;
they will match successive
\code{"\%s"} fields in the string.

See the examples.
}
  \item{directory, package, pythonPos}{
\code{pythonPath()} adds the directory path specified to the path searched by
the interpreter when looking for Python modules.  The default action
is to insert a \code{directory} named \code{"python"} at the end
of the search path.  If \code{package} is omitted, the default is to
look for the package name; if the call comes from a function in the
package source, this will retrieve the corresponding name.  In any
case, if \code{package} is a non-empty string, the directory path is
taken relative to the package's load position, meaning that the
directory path
in the package source would be relative to the \code{"inst"}
subdirectory.

To insert the new directory at the beginning of the path, supply
\code{pythonPos} as \code{0}.

To add one or more directories to the Python path when your own
package is loaded, call \code{\link{setupPythonPath}} from your \R
source code (meaning that the call will be evaluated during package
installation, and will set up a load action to call \code{pythonPath()}.)
}

  \item{ref}{
A reference to a cached Python object, to be used in subsequent calls
to \code{evalInPython()}, \code{getFromPython()} or \code{deleteInPython()}
}

  \item{obj}{
An R object that can be converted to an equivalent Python object.
Just what classes will work depends on the low-level interface, but
basic vectors and named lists are usually handled.
}

  \item{\dots}{
Various objects to be used in calls to Python.  They can be R proxy
objects, returned by previous use of the interface, or ordinary R
objects.
The interface process the proxy objects to retrieve the corresponding
Python objects from an internal table in the interface.
R objects are converted inline to supposed Python equivalents.  The
conditions under which this works depend on the particular low-level
interface. See \link{PythonLowLevelInterface}.

The special named argument \code{.Action =}
controls what is done with the result of evaluating the Python code.
If omitted, \code{.Action} defaults to the \code{"action"} field of
the interpreter, which itself defaults to \code{"keep"}.

The value of \code{.Action} is matched to \code{c("keep", "throw",
  "get")} by the rules of \code{\link{match.arg}()}.

For \code{"keep"}, the default, a new cached Python object is  created and a reference to
it returned to \R{}.
The interface keeps a dictionary of such Python objects for each
interpreter, so that the references returned may be used in further
calls.
Note that Python objects, except for scalars, are reference objects,
so that one can modify the cached objects in place without worrying
about replacing the cached reference.

If \code{.Action} is \code{"throw"} the result of the Python
computation is not used.

If  \code{.Action} is \code{"get"},
the value is returned as an equivalent R object,
assuming that is possible.

}

  \item{object, method}{
A proxy object in R for some Python class and the character string
name of a Python method for that class.
}

  \item{fun, module}{
Character string name for a Python function or module, for constructing
the corresponding function call or import directive in Python.
}


}

\section{Methods and Function}{
The function \code{curPythonInterpreter()} returns the currently
active interpreter object.
An interpreter is started when this package is loaded.
Other interpreters can be initiated by creating an object from one of
the subclasses of class \code{"PythonInterpreter"}.

The remaining functions described are aliases for corresponding
methods for an interpreter object, as shown in the comments above.
The methods typically have shorter names, not needing to differentiate
themselves from non-Python-related versions.

Python expressions are evaluated by \code{evalInPython()} and the results cached in
Python, avoiding overhead of repeated conversions and limitations on
the possible classes of Python objects involved.
This function takes a C-style format argument that it uses to
construct the Python expression.  Other arguments may be Python object
references returned from evaluating earlier expressions, or they may
be R objects.
However, the R objects must be simple enough to be converted to
in-line Python equivalents.
See the section on \emph{Data Transfer} for some suggestions.

For most computations, \code{evalInPython()} can be avoided by using
the simpler functions \code{pythonCall()} and
\code{pythonMethod()}, which construct calls to
\code{evalInPython()}, as described below.
More straightforward still is to define R classes that correspond to
Python classes, and then invoke reference class methods on the proxy
objects from these classes (see \link{PythonInterfaceClasses}).

Objects are copied
to and from Python by \code{assignInPython()} and
\code{getFromPython()}, and may be removed from the cache by
\code{deleteInPython()}.

A simple interactive Python shell is started by \code{pythonShell()}.
It reads lines from standard input, interprets each line as a
(complete) python expression and sends the expression to the
interpreter.
The shell is mainly intended to examine the interpreter for debugging
or possibly to change some settings in the interpreter.

The functions \code{pythonImport()}, \code{pythonCall()} and
\code{pythonMethod()} set up and evaluate \code{import} statements,
function calls and method invocations in Python.
The \code{fun} and \code{method} argument will be character string
names for the corresponding Python functions and methods.
Other arguments may be \R proxy objects as discussed in
\link{PythonInterfaceClasses} or sufficiently simple R objects to be
translated inline as Python objects.
}

\section{Data Transfer}{
The inline conversion of arguments sent to Python functions and
methods relies on the capabilities of the low-level interface.
With the initial Java-based implementation, conversion works well
enough for elementary vectors.  Lists are handled to some extent, but
beware that lists and named lists are handled differently; in
particular, lists need to have unique names for all elements or else
no names at all.  Anything in between will likely fail.

More complex structures in either language will usually not convert.
In any case, the inline transfer of data is rather clumsy and
inefficient.

For significant quantities of data, one should use a suitable
intermediate file format (e.g., \code{".csv"} or \code{XML}) or else
some database software.
Both \R and Python are well equipped with software and interfaces to
database management systems in order to handle such data transfers.
}

\details{
These interface functions are defined in terms of a set of low-level R
interface functions (see \link{PythonLowLevelInterface}), plus some
standard R and Python code.

The interface defined here is not primarily intended for direct use to
generate Python expressions, although see \link{PythonConveniences}
for some functions that might help.

The intended paradigm is that an R package wants to use some Python
capabilities.  The author of the package will construct some
specialized functions and/or classes to access the Python facilities.
}
\value{
For \code{evalInPython()} and \code{assignInPython()}, a reference to a
cached Python object.  For \code{getFromPython()}, a converted R
version of such an object.  For \code{deleteInPython()}, nothing useful.
}
\references{
Book in progress.
}
\author{
John M. Chambers
}

\examples{
\dontrun{
ref1 <- asPy(rnorm(10)) # make a Python list of 10 numbers
ref2 <- evalPy("X[0:len(X)/2]", X = ref1) # the first half of previous
evalPy("%s[0:len(%s)/2]", ref1, ref1) # same, using C formatting
getPy(ref2)
}
}
\keyword{ programming }
\keyword{ interface }
