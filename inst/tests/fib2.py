def fib2(n):
    if n < 2: return n
    return fib2(n-1) + fib2(n-2)

def fib3(n): #note cross-ref to fib2()
    if n < 2: return n
    return fib2(n-1) + fib2(n-2)

