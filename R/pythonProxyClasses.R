### Proxy Classes for some basic Python classes: list, dict

list_Python <- XR::setProxyClass("list", module = "",
    evaluatorClass = "PythonInterface", language = "Python",
    fields = character(), methods = list()
    )

list_Python$methods(
ServerClassInfo = function ()
{
    list(ServerClass = "list", ServerModule = "", language = "Python",
        evaluatorClass = "PythonInterface")
},

append = function (..., .ev = XRPython::RPython())
{
    .ev$MethodCall(.proxyObject, "append", ...)
},

count = function (..., .ev = XRPython::RPython())
{
    .ev$MethodCall(.proxyObject, "count", ...)
},

extend = function (..., .ev = XRPython::RPython())
{
    .ev$MethodCall(.proxyObject, "extend", ...)
},

index = function (..., .ev = XRPython::RPython())
{
    .ev$MethodCall(.proxyObject, "index", ...)
},

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

insert = function (..., .ev = XRPython::RPython())
{
    .ev$MethodCall(.proxyObject, "insert", ...)
},

pop = function (..., .ev = XRPython::RPython())
{
    .ev$MethodCall(.proxyObject, "pop", ...)
},

remove = function (..., .ev = XRPython::RPython())
{
    .ev$MethodCall(.proxyObject, "remove", ...)
},

reverse = function (..., .ev = XRPython::RPython())
{
    .ev$MethodCall(.proxyObject, "reverse", ...)
},

sort = function (..., .ev = XRPython::RPython())
{
    .ev$MethodCall(.proxyObject, "sort", ...)
})


dict_Python <- XR::setProxyClass("dict", module = "",
    evaluatorClass = "PythonInterface", language = "Python",
    fields = character(), methods = list()
    )

dict_Python$methods(
ServerClassInfo = function ()
{
    list(ServerClass = "dict", ServerModule = "", language = "Python",
        evaluatorClass = "PythonInterface")
},

clear = function (..., .ev = XRPython::RPython())
{
    .ev$MethodCall(.proxyObject, "clear", ...)
},

copy = function (..., .ev = XRPython::RPython())
{
    .ev$MethodCall(.proxyObject, "copy", ...)
},

fromkeys = function (..., .ev = XRPython::RPython())
{
    .ev$MethodCall(.proxyObject, "fromkeys", ...)
},

get = function (..., .ev = XRPython::RPython())
{
    .ev$MethodCall(.proxyObject, "get", ...)
},

has_key = function (..., .ev = XRPython::RPython())
{
    .ev$MethodCall(.proxyObject, "has_key", ...)
},

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

items = function (..., .ev = XRPython::RPython())
{
    .ev$MethodCall(.proxyObject, "items", ...)
},

iteritems = function (..., .ev = XRPython::RPython())
{
    .ev$MethodCall(.proxyObject, "iteritems", ...)
},

iterkeys = function (..., .ev = XRPython::RPython())
{
    .ev$MethodCall(.proxyObject, "iterkeys", ...)
},

itervalues = function (..., .ev = XRPython::RPython())
{
    .ev$MethodCall(.proxyObject, "itervalues", ...)
},

keys = function (..., .ev = XRPython::RPython())
{
    .ev$MethodCall(.proxyObject, "keys", ...)
},

pop = function (..., .ev = XRPython::RPython())
{
    .ev$MethodCall(.proxyObject, "pop", ...)
},

popitem = function (..., .ev = XRPython::RPython())
{
    .ev$MethodCall(.proxyObject, "popitem", ...)
},

setdefault = function (..., .ev = XRPython::RPython())
{
    .ev$MethodCall(.proxyObject, "setdefault", ...)
},

update = function (..., .ev = XRPython::RPython())
{
    .ev$MethodCall(.proxyObject, "update", ...)
},

values = function (..., .ev = XRPython::RPython())
{
    .ev$MethodCall(.proxyObject, "values", ...)
},

viewitems = function (..., .ev = XRPython::RPython())
{
    .ev$MethodCall(.proxyObject, "viewitems", ...)
},

viewkeys = function (..., .ev = XRPython::RPython())
{
    .ev$MethodCall(.proxyObject, "viewkeys", ...)
},

viewvalues = function (..., .ev = XRPython::RPython())
{
    .ev$MethodCall(.proxyObject, "viewvalues", ...)
})


