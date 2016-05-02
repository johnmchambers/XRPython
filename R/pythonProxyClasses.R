#' Proxy Class for Python Lists
#' 
#' This class is a proxy for ordinary list objects in Python.  All the
#' standard Python methods for such objects (e.g., \code{append()}) are
#' available, but methods for R functions such as \code{]} are not
#' implemented because Python operators do not behave functionally.
#' Instead, additional methods are defined for the proxy lists, e.g.,
#' \code{el(i)}.
list_Python <- setRefClass("list_Python", contains = c("ProxyClassObject"))
list_Python <- XR::setProxyClass("list", module = "",
    evaluatorClass = "PythonInterface", language = "Python", proxyObjectClass = "PythonObject",
    methods = list()
    )

list_Python$methods(
initialize = function (..., evaluator, .serverObject) 
{
    if (missing(evaluator)) 
        evaluator <- XR::getInterface("PythonInterface")
    if (missing(.serverObject)) {
        NULL
        .serverObject <- evaluator$New("list", "", ...)
    }
    if (is(.serverObject, "ProxyClassObject")) 
        proxy <- .serverObject$.proxyObject
    else proxy <- .serverObject
    .proxyObject <<- proxy
    .ev <<- evaluator
},

ServerClassInfo = function () 
list(ServerClass = "list", ServerModule = "", language = "Python", 
    evaluatorClass = "PythonInterface", proxyFields = NULL, proxyMethods = c("initialize", 
    "ServerClassInfo", "append", "count", "extend", "index", 
    "insert", "pop", "remove", "reverse", "sort"), proxyContains = character(0), 
    proxyObjectClass = "PythonObject"),

append = function (..., .ev = XRPython::RPython(), .get = NA) 
{
    "Python Documentation: L.append(object) -- append object to end"
    .ev$MethodCall(.proxyObject, "append", ..., .get = .get)
},

count = function (..., .ev = XRPython::RPython(), .get = NA) 
{
    "Python Documentation: L.count(value) -> integer -- return number of occurrences of value"
    .ev$MethodCall(.proxyObject, "count", ..., .get = .get)
},

extend = function (..., .ev = XRPython::RPython(), .get = NA) 
{
    "Python Documentation: L.extend(iterable) -- extend list by appending elements from the iterable"
    .ev$MethodCall(.proxyObject, "extend", ..., .get = .get)
},

index = function (..., .ev = XRPython::RPython(), .get = NA) 
{
    "Python Documentation: L.index(value, [start, [stop]]) -> integer -- return first index of value.\nRaises ValueError if the value is not present."
    .ev$MethodCall(.proxyObject, "index", ..., .get = .get)
},

insert = function (..., .ev = XRPython::RPython(), .get = NA) 
{
    "Python Documentation: L.insert(index, object) -- insert object before index"
    .ev$MethodCall(.proxyObject, "insert", ..., .get = .get)
},

pop = function (..., .ev = XRPython::RPython(), .get = NA) 
{
    "Python Documentation: L.pop([index]) -> item -- remove and return item at index (default last).\nRaises IndexError if list is empty or index is out of range."
    .ev$MethodCall(.proxyObject, "pop", ..., .get = .get)
},

remove = function (..., .ev = XRPython::RPython(), .get = NA) 
{
    "Python Documentation: L.remove(value) -- remove first occurrence of value.\nRaises ValueError if the value is not present."
    .ev$MethodCall(.proxyObject, "remove", ..., .get = .get)
},

reverse = function (..., .ev = XRPython::RPython(), .get = NA) 
{
    "Python Documentation: L.reverse() -- reverse *IN PLACE*"
    .ev$MethodCall(.proxyObject, "reverse", ..., .get = .get)
},

sort = function (..., .ev = XRPython::RPython(), .get = NA) 
{
    "Python Documentation: L.sort(cmp=None, key=None, reverse=False) -- stable sort *IN PLACE*;\ncmp(x, y) -> -1, 0, 1"
    .ev$MethodCall(.proxyObject, "sort", ..., .get = .get)
})


### additional methods for the list_Python proxy class

list_Python$methods(
    el = function(i) {
        'Extract an element from the list (zero based indexing).
The index will be coerced to integer (unless a proxy).'
        if(is.numeric(i))
            i <- as.integer(i)
        Eval("%s[%s]", .self, i)
    }
)
#' Proxy Class for Python Dictionaries
#' 
#' This class is a proxy for ordinary dictionary objects in Python.  All the
#' standard Python methods for such objects (e.g., \code{keys()}) are
#' available, but methods for R functions are not implemented.
dict_Python <- setRefClass("dict_Python", contains = c("ProxyClassObject"))
dict_Python <- XR::setProxyClass("dict", module = "",
    evaluatorClass = "PythonInterface", language = "Python", proxyObjectClass = "PythonObject",
    methods = list()
    )

dict_Python$methods(
initialize = function (..., evaluator, .serverObject) 
{
    if (missing(evaluator)) 
        evaluator <- XR::getInterface("PythonInterface")
    if (missing(.serverObject)) {
        NULL
        .serverObject <- evaluator$New("dict", "", ...)
    }
    if (is(.serverObject, "ProxyClassObject")) 
        proxy <- .serverObject$.proxyObject
    else proxy <- .serverObject
    .proxyObject <<- proxy
    .ev <<- evaluator
},

ServerClassInfo = function () 
list(ServerClass = "dict", ServerModule = "", language = "Python", 
    evaluatorClass = "PythonInterface", proxyFields = NULL, proxyMethods = c("initialize", 
    "ServerClassInfo", "clear", "copy", "fromkeys", "get", "has_key", 
    "items", "iteritems", "iterkeys", "itervalues", "keys", "pop", 
    "popitem", "setdefault", "update", "values", "viewitems", 
    "viewkeys", "viewvalues"), proxyContains = character(0), 
    proxyObjectClass = "PythonObject"),

clear = function (..., .ev = XRPython::RPython(), .get = NA) 
{
    "Python Documentation: D.clear() -> None.  Remove all items from D."
    .ev$MethodCall(.proxyObject, "clear", ..., .get = .get)
},

copy = function (..., .ev = XRPython::RPython(), .get = NA) 
{
    "Python Documentation: D.copy() -> a shallow copy of D"
    .ev$MethodCall(.proxyObject, "copy", ..., .get = .get)
},

fromkeys = function (..., .ev = XRPython::RPython(), .get = NA) 
{
    "Python Documentation: dict.fromkeys(S[,v]) -> New dict with keys from S and values equal to v.\nv defaults to None."
    .ev$MethodCall(.proxyObject, "fromkeys", ..., .get = .get)
},

get = function (..., .ev = XRPython::RPython(), .get = NA) 
{
    "Python Documentation: D.get(k[,d]) -> D[k] if k in D, else d.  d defaults to None."
    .ev$MethodCall(.proxyObject, "get", ..., .get = .get)
},

has_key = function (..., .ev = XRPython::RPython(), .get = NA) 
{
    "Python Documentation: D.has_key(k) -> True if D has a key k, else False"
    .ev$MethodCall(.proxyObject, "has_key", ..., .get = .get)
},

items = function (..., .ev = XRPython::RPython(), .get = NA) 
{
    "Python Documentation: D.items() -> list of D's (key, value) pairs, as 2-tuples"
    .ev$MethodCall(.proxyObject, "items", ..., .get = .get)
},

iteritems = function (..., .ev = XRPython::RPython(), .get = NA) 
{
    "Python Documentation: D.iteritems() -> an iterator over the (key, value) items of D"
    .ev$MethodCall(.proxyObject, "iteritems", ..., .get = .get)
},

iterkeys = function (..., .ev = XRPython::RPython(), .get = NA) 
{
    "Python Documentation: D.iterkeys() -> an iterator over the keys of D"
    .ev$MethodCall(.proxyObject, "iterkeys", ..., .get = .get)
},

itervalues = function (..., .ev = XRPython::RPython(), .get = NA) 
{
    "Python Documentation: D.itervalues() -> an iterator over the values of D"
    .ev$MethodCall(.proxyObject, "itervalues", ..., .get = .get)
},

keys = function (..., .ev = XRPython::RPython(), .get = NA) 
{
    "Python Documentation: D.keys() -> list of D's keys"
    .ev$MethodCall(.proxyObject, "keys", ..., .get = .get)
},

pop = function (..., .ev = XRPython::RPython(), .get = NA) 
{
    "Python Documentation: D.pop(k[,d]) -> v, remove specified key and return the corresponding value.\nIf key is not found, d is returned if given, otherwise KeyError is raised"
    .ev$MethodCall(.proxyObject, "pop", ..., .get = .get)
},

popitem = function (..., .ev = XRPython::RPython(), .get = NA) 
{
    "Python Documentation: D.popitem() -> (k, v), remove and return some (key, value) pair as a\n2-tuple; but raise KeyError if D is empty."
    .ev$MethodCall(.proxyObject, "popitem", ..., .get = .get)
},

setdefault = function (..., .ev = XRPython::RPython(), .get = NA) 
{
    "Python Documentation: D.setdefault(k[,d]) -> D.get(k,d), also set D[k]=d if k not in D"
    .ev$MethodCall(.proxyObject, "setdefault", ..., .get = .get)
},

update = function (..., .ev = XRPython::RPython(), .get = NA) 
{
    "Python Documentation: D.update([E, ]**F) -> None.  Update D from dict/iterable E and F.\nIf E present and has a .keys() method, does:     for k in E: D[k] = E[k]\nIf E present and lacks .keys() method, does:     for (k, v) in E: D[k] = v\nIn either case, this is followed by: for k in F: D[k] = F[k]"
    .ev$MethodCall(.proxyObject, "update", ..., .get = .get)
},

values = function (..., .ev = XRPython::RPython(), .get = NA) 
{
    "Python Documentation: D.values() -> list of D's values"
    .ev$MethodCall(.proxyObject, "values", ..., .get = .get)
},

viewitems = function (..., .ev = XRPython::RPython(), .get = NA) 
{
    "Python Documentation: D.viewitems() -> a set-like object providing a view on D's items"
    .ev$MethodCall(.proxyObject, "viewitems", ..., .get = .get)
},

viewkeys = function (..., .ev = XRPython::RPython(), .get = NA) 
{
    "Python Documentation: D.viewkeys() -> a set-like object providing a view on D's keys"
    .ev$MethodCall(.proxyObject, "viewkeys", ..., .get = .get)
},

viewvalues = function (..., .ev = XRPython::RPython(), .get = NA) 
{
    "Python Documentation: D.viewvalues() -> an object providing a view on D's values"
    .ev$MethodCall(.proxyObject, "viewvalues", ..., .get = .get)
})


