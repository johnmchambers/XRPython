\name{PythonObjectRef-class}
\alias{setRPythonClass}
\alias{PythonInterfaceClasses}
\Rdversion{1.1}
%%\docType{class}
\alias{PythonObjectRef-class}
\alias{PythonInterfaceClass-class}

\title{Interface Classes in R Mapping to Python Classes}
\description{
\code{setRPythonClass} creates an \R class that dispatches methods and
accesses fields from a Python class.
The objects returned to \R, from class
\code{PythonObjectRef()} allow users to access
the Python methods and fields directly from R.
}
\usage{
setRPythonClass(Class =, PythonClass = , module = ,
    fields = , methods = , usePython = TRUE, 
    interp =, where =)
}
\arguments{
  \item{Class, PythonClass}{ The \R class to be created, and the
      Python class to which it corresponds.  It's usually good for
      these to be the same name to avoid confusion, in which case only
      one argument need be supplied. Exceptions
      occur when the Python class name conflicts with an R class, so
      that the class needs an \R-specialized name to avoid conflicts.

}

 \item{module}{
The Python module from which the class should be imported.
}
  \item{fields}{
The fields (i.e., data members) of the Python class that we want to
access directly from \R.  Python doesn't really have fields in any
formal sense, but we provide a mechanism for getting and setting named
members.
In addition to fields found by inspection, the \code{fields} argument
can be used to specify additional Python fields or to constrain any
assignments of the fields from \R; see the \emph{Python Fields} section below for details.
}
  \item{contains}{
Classes in \R from which this class should inherit.  Most usefully,
these will be other Python interface classes.  The interface does not
currently make any attempt to replicate the Python
inheritance of the class automatically.
}
  \item{methods}{
The names of the Python methods to be shared in \R.
If none are specified, the interface will share
all the Python methods discovered by examining
the Python object or class object at package install time, via \code{\link{classStructure}()}.
}
  \item{usePython}{
Should an object from the Python class be created, and used to infer
the methods and fields?  See above.
}
  \item{interp}{
The Python interpreter to be used if one is needed at installation
time. Usually omitted, using the default interpreter.
}
  \item{where}{
The namespace into which the class definition should be assigned.
Nearly always this should be the default, the package namespace
when the call to \code{setRPythonClass()} is in a package source file.
}
}
\section{Python Fields}{
Fields (i.e., data attributes) in Python are not really fields in the
sense of the analogous notion in R, C++ or Java.
They can not be inferred from the class object; most fields are
created by the initializing method, but any method or any computation
using an instance of the class can create a field and there is no
guarantee that different instance objects from the same class have the
same fields, let alone any consistency in the class of the fields.

That said, most well-designed Python classes do have specific
attributes as part of their definition.

      The methods and, to some extent, the fields of a Python class
      can be determined by inspecting an instance of the class in
      Python.
      \R{} code can obtain this information through the reference
      class and generator function
      \code{\link{classStructure}()}.  Using this information is
      generally a good idea.  \code{setRPythonClass()} will try to
      create such an object if argument \code{usePython} is
      \code{TRUE} (as it is by default).

      Alternatively, you can use \code{\link{classStructure}()} ahead
      of time to create a class structure object that can then be
      modified or supplemented and used instead of the class name in
      the call to \code{setRPythonClass()}, if you want to restrict
      the methods and fields exposed.

The other design question regarding fields is whether they should be
converted to an R object when accessed, or left as Python proxy
objects.
By default, fields are not converted (the procedure can be inefficient
for large fields).
Conversion is controlled by supplying an \R class in the \code{fields}
argument as explained above.
Note that those fields \emph{only} inferred from the class structure will be
left as proxy objects.
}
\section{Python Methods}{
...........
}
\examples{
\dontshow{
p <- curPythonInterpreter()
p$echo <- FALSE
}
pythonImport("examples")
Bag <- setRPythonClass("Bag", module = "examples")
b <- Bag()
b
b$add(1:2)
b$data
b$data$toR()

}
\keyword{classes}
