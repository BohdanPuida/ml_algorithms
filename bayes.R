train <- iris[1:100, ]
cv <- iris[101:150, ]
tapply(train[,5])

D <- dim(train)[1]
D
a <- c()

b <- unlist(by(train, train[,5], nrow))
b
apply(cv, 1, function(x)
  )