import types
import re
import inspect
import copy
import sys
import json
import pickle
## just for the XMLAbstractDocument example method
import xml.etree.ElementTree

from json import loads as objectFromJSON

def getMethods(cl):
    ''' Returns a list of the method names, given an available class
object. See classStructure() to get fields as well, from an
instance of the class.'''
    attrs = dir(cl)
    methods = []
    for name in attrs:
        if re.match("[a-zA-Z]", name) and isinstance(getattr(cl, name), types.MethodType):
            methods.append(name)
    return methods

def classStructure(obj, getAll = False):
    ''' Returns a dictionary with elements "fields" and "methods",
containing the names of the fields (members) and methods
in a Python class, plus the python class and module as strings.
Buit-in object types are detected by a missing module attribute, but all 
attributes will be reported as methods, even if they are really 
hidden "fields".
Argument "obj" should be an instance of the class.
The class object itself is an alternative, but Python being Python, the
class object will not have fields if these are set by the initialization
method, and there is no guarantee that all instances will have the same
fields.  If "getAll" is True, get all members, otherwise only those whose
names start with a letter (excludes operator and function overloads.)'''
    fields = []
    fieldTypes = []
    methods = []
    methodArgs = {}
    builtIn = not hasattr(obj, "__module__")
    for el in dir(obj):
        member = getattr(obj, el)
        if getAll or re.search("^[a-zA-Z]", el):
            if builtIn or isinstance(member, types.MethodType):
                methods.append(el)
            else:
                fieldTypes.append(member.__class__.__name__)
                fields.append(el)
    pyClass = obj.__class__.__name__
    if builtIn:
        module = ""
    else:
        module = obj.__module__
    return dict(pyClass = pyClass, module = module,
                fields = fields, fieldTypes = fieldTypes, methods = methods, methodArgs = methodArgs)

### Utilities get_items, set_items for sequence methods for "[", "[<-"
### Not currently used
def get_items(object, items):
    value = []
    if not isinstance(items, list):
        items = [ items ]
    for i in items:
        value.append(object[i])
    return value

def set_items(object, items, value):
    j = 0
    k = 0
    if not isinstance(items, list):
        items = [ items ]
    if not isinstance(value, list):
        value = [ value ]
    for i in items:
        object[ items[j] ] = value[k]
        j += 1
        k += 1
        if k >= len(value):
            k = 0
    return value

def arglist_for_R(f):
    ip = inspect.getargspec(f)
    args = ip.args
    if ip.varargs != None or ip.keywords != None:
        args.append("...")
    return  args

def function_for_R(f):
    ## some methods, etc are builtin:  we treat those as f(...)
    if not ( inspect.isfunction(f) or inspect.ismethod(f) ):
        return {'args' : [ ], 'nopt' : 0 , 'dots' : True }
    ip = inspect.getargspec(f)
    args = ip.args
    if inspect.ismethod(f):
        args.pop(0) # drop "self"
    if ip.defaults is None:
        nopt = 0
    else:
        nopt = len(ip.defaults)
    dots =  ip.varargs != None or ip.keywords != None
    return {'args' : args, 'nopt' : nopt, 'dots' : dots}

proxyForm = {'.RClass' : 'AssignedProxy', '.package' : 'XR', '.type' : 'character'}
errorForm = {'.RClass' : 'InterfaceError', '.package' : 'XR', '.type' : 'S4'}
warningForm = {'.RClass' : 'InterfaceWarning', '.package' : 'XR', '.type' : 'S4'}
scalarTypes = { 'NoneType' : 0, 'int' : 1, 'float' : 2, 'str' : 3, 'bool' : 4}

_for_R = {"R_None": None}

recent_keys = []
max_save = 30

def error_for_R(message):
    value = copy.copy(errorForm)
    value["message"] = message
    return toR(value)

def warning_for_R(message, valueComputed):
    value = copy.copy(warningForm)
    value["message"] = message
    value["value"] = valueComputed
    return toR(value)

def module_class(obj):
    '''Returns the dot-separated module and class string for obj'''
    if hasattr(obj, "__module__"):
        return obj.__module__ + '.' + obj.__class__.__name__
    else:
        return obj.__class__.__name__

def store_for_R(obj, this_key, args_from_R):
    global _for_R
    global recent_keys
    if obj is None:
        return "R_None"
    for key in args_from_R:
        if _for_R[key] is obj:
            return key
    for key in recent_keys:
        if _for_R[key] is obj:
            return key
    key = this_key
    _for_R[key] = obj
    recent_keys.insert(0, key)
    if len(recent_keys) > max_save:
        del recent_keys[max_save]
    return key

def del_for_R(key):
    global _for_R
    if _for_R.has_key(key):
        del _for_R[key]
        del_from(recent_keys, key)
        return True
    else:
        return False

def del_from(keys, key):
    i = 0
    for thisKey in keys:
        if thisKey == key:
            del keys[i]
            return True
        i += 1
    return False

def descr_for_R(key):
    global _for_R
    if not _for_R.has_key(key):
        return "Not a currently valid key"
    obj = _for_R[key]
    typeName = type(obj).__name__
    if typeName == "instance":
        clName = obj.__class__.__name__
        mdName = obj.__module__
        return "Python object of class " + clName + ", module " + mdName
    else:
        return "Python object of type " + typeName

def pickle_for_R(key, fileName):
    global _for_R
    if _for_R.has_key(key):
        try:
            ff = open(fileName, "a")
        except:
            return error_for_R("Can't open file {0} for serializing: {1}".format(fileName, sys.exc_value))
        value = None
        try:
            pickle.dump(_for_R[key], ff)
            ff.close()
        except:
            value = error_for_R("Error in Python serialize of {0}: {1}".format(key, sys.exc_value))
        return value
    return error_for_R("Python key not found for serializing: " + key)

## open the serializing file and return the list to which the serialized
## objects will be appended
def start_unpickle(file):
    _for_R["unserializeFile"] = open(file, "r")
    return [ ]

def end_unpickle():
    _for_R["unserializeFile"].close

def unpickle_for_R(obj):
    value = True
    try:
        obj.append(pickle.load(_for_R["unserializeFile"]))
    except EOFError :
        value = False
    return value

def value_for_R(expr, key, send):
    code = None
    if key == "":
        what = "command"
        comp = 'exec'
    else:
        what = "expression"
        comp = 'eval'
    try:
        code = compile(expr, "from R", comp)
    except:
        return error_for_R("Compile error in {0} \"{1}\": {2}".format(what, expr, sys.exc_value))
    obj = None
    try:
        obj = eval(code, globals(), _for_R)
    except:
        return error_for_R("Evaluation error in {0} \"{1}\": {2}".format(what, expr, sys.exc_value))
    if key == "":
        return 'null'
    return proxy_or_object(obj, send, key)

def proxy_or_object(obj, send, key):
    typeName = type(obj).__name__
    if typeName == 'instance': # old-style class
        typeName = obj.__class__.__name__
    if send == True or (send is None and scalarTypes.has_key(typeName) ) :
        return toR(obj, typeName) 
    key = store_for_R(obj, key, [])
    value = copy.copy(proxyForm)
    value['.Data'] = key
    if hasattr(obj, "__module__"):
        value['module'] = obj.__module__
    if hasattr(obj, '__len__') and not isinstance(obj, type): #? are there other traps?
        value['size'] =  len(obj)
    else:
        value['size' ] = -1
    value['serverClass'] = typeName
    return toR(value)


def R_Object(Class, package = "", type = "", value = {}):
    value['.RClass'] = Class
    value['.package'] = package
    if type != "":
        value['.type'] = type
    return value


def toR_list(obj):
    value = "[ " # empty list
    for i in range(len(obj)):
        if i > 0:
            value = "{0}, {1}".format(value, toR(obj[i]))
        else:
            value = "[ {0}".format(toR(obj[i]))
    return "{0}]".format(value)

def toR_dict(obj):
    value = "{ " # in case empty
    okeys = obj.keys()
    for i in range(len(okeys)):
        keyi = okeys[i]
        if i > 0:
            value = "{0}, {1} : {2}".format(value, toR(keyi), toR(obj[keyi]))
        else:
            value = "{0}{1} : {2}".format(value, toR(keyi), toR(obj[keyi]))
    return "{0}{1}".format(value, '}')

def toR_class(obj, typename, module):
    value = R_Object("from_Python", "XRPython")
    value["serverClass"] = typename
    value["module"] = module
    structure = classStructure(obj)
    fields = { }
    for fld in structure["fields"]:
        fields[fld] = getattr(obj, fld)
    value["fields"] = fields
    return toR_dict(value)


toR_methods = {'dict' : toR_dict, "list" : toR_list}

def toR(obj, typename = ""):
    if typename == "":
        typename = type(obj).__name__
    if typename == 'instance': # old-style class
        typename = obj.__class__.__name__
    isClassObject = hasattr(obj, "__module__")
    baseTypename = typename
    if isClassObject:
        module = obj.__module__
        typename = "{0}.{1}".format(module, typename)
    else:
        module = ""
    if toR_methods.has_key(typename):
        f = toR_methods[typename]
        return f(obj)
    if scalarTypes.has_key(typename) or not isClassObject:
        return json.dumps(obj)
    ## return from_Python object
    return toR_class(obj, baseTypename, module)

def set_toR(what, method):
    toR_methods[what] = method # should check that method is a valid function
    return True

## return a typed vector that R will convert to that class of object

def vectorR(data, typeR = None, missing = [ ]):
    if typeR is None:
        typeR = inferType(data)
    return { ".RClass" : "vector_R", ".type" : "S4", ".package" : "XR",
             "type" : typeR, "data" : data, "missing" : missing }

def inferType(data) :
    if(len(data) == 0):
        return "list"
    x = data[0]
    if isinstance(x, int):
        return "integer"
    elif isinstance(x, float):
        return "numeric"
    elif isinstance(x, str):
        return "character"
    elif isinstance(x, bool):
        return "logical"
    return "list"
