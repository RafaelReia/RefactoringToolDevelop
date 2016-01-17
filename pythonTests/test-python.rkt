#lang python

if ( 1 < 2):
  False
else:
  True


print True if (1 < 2) else False

def fib(n):
  if n == 0: return 0
  elif n == 1: return 1
  else: return fib(n-1) + fib(n-2)



def ackermann(m,n):
    if m == 0: return n+1
    elif m > 0 and n == 0: return ackermann(m-1,1)
    else: return ackermann(m-1, ackermann(m,n-1))

print ackermann(1,1)

def testExtract(a, b, c):
    a + b + c

