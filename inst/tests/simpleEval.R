###HIDE
ev <- XRPython::RPython()
###SHOW
xx <- ev$Eval("[1, %s, 5]", pi)
xx
