require(XRPython)

tlist <- setRefClass("tlist", contains = "list_Python",
                     fields = c(time = "POSIXt"))

ll <- tlist(.serverObject=list_Python(1:10), time = Sys.time())

ll

ll$serverObject

l2 <- list_Python(letters[1:5])
ll$serverObject <- l2
ll
ll$serverObject
unlist(pythonGet(ll))
ll$time

## named arguments with no .serverObject are all taken
## as arguments to the server call, so time must be set separately
l1 <- tlist(1:10)
l1$time <- ll$time

l1
