x <- 1
y <- 3

fun <- function(a,b)
{a <- a+b
return(a*a)
}

fun(a = x, b = y)
x #1
y #3

library(gtools)
macro <- defmacro(a, b,
                  expr = {a <- a+b
                  return(a*a)})
macro(x,y)
x
y
