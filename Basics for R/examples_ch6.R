kids <- c("Jack","Jill")
ages <- c(12,20)
d <- data.frame(kids, ages, stringsAsFactors = FALSE)
print(d[[1]])
# loop example basic
  x <- c(1,2,3,4,5)
  for (n in x) { print (n * n)}
  # functions
  f1 <-function(a,b) { return (a+b) }
  f2 <- function(a,b) { return (a -b) }
  f <- f1
  a <- f(3,2)
  print(a)