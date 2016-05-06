### additional methods for the list_Python proxy class

list_Python$methods(
    el = function(i) {
        'Extract an element from the list (zero based indexing).
The index will be coerced to integer (unless a proxy).'
        if(is.numeric(i))
            i <- as.integer(i)
        .ev$Eval("%s[%s]", .self, i)
    }
)
