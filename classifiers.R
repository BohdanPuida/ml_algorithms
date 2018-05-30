nb <- function(data, train, cv, relevant_type) {
  a <- by(train[, -relevant_type], train[, relevant_type], function(x) { # group by relevant type
    
    b <- as.data.frame(x) # from list (lol) to df
    d <- apply(b, 1, function(y) apply(cv, 1, function(z) as.numeric(`==`(y, z))))  #check whether train data is consistent with cv (say no to loops)
    l <- apply(d, 1, function(x) colSums(matrix(x, nrow = nrow(b))))  
    l[which(l == 0)] <- 1 # log(0) error
    l <- matrix(c(l), nrow(cv), ncol(cv), T)
    l <- l/nrow(b)
    s <- nrow(b)/ nrow(train)
    s <- log(l) + log(s)
    s <- apply(s, 1, sum)
    s
  }
  )
  v <- names(a)
  a <- unlist(a)
  n <- length(unique(data[, relevant_type]))*nrow(cv)-length(a)
  #a <- append(a, rep(0, n))
  a <- as.data.frame(matrix(c(a), length(unique(data[, relevant_type])),  nrow(cv), T))
  rownames(a) <- v
  a <- t(a)
  a <- apply(a, 1, function(x) names(x)[which(max(x) == x)][1])
  a
  

}
iris1 <- as.data.frame(apply(iris[,-5], 2, function(x) as.integer(x))) ##simple discretization
iris1 <- cbind(iris1, iris[, 5]) #add Species to discretisized data set
iris1 <- sample_n(iris1,150) # random shuffle
colnames(iris1)[5] <- 'Species' # add name to relevant type

train <- iris1[1:30, ]
cv <- iris1[31:150, -5]
dim(train)
cv
train
train1 <- sample_n(train, 130)
train2 <- sample_n(train, 130)
train3 <- sample_n(train, 130)
train3

library(stringr)
help(sample_n)

train1 <- sample_n(train, 100, T)
train1
train2 <- sample_n(train, 100, T)
cv
train <- iris[1:100, ]
b <- nb (iris, train, cv,  5)
l <- nb(iris, train2, cv, 5)
b
l
all.equal(l, h, check.attributes = F)

b

h <- as.character(iris[rownames(cv), 5])
h

menagerie[,18]
menagerie_true  <- menagerie[, 2:18]
menagerie1 <- sample_n(menagerie_true, 101)
menagerie_true

menagerie1
train <- menagerie1[1:60, ]
dim(train)
cv <- menagerie1[61:101, -17]
train
cv
v <- c(1:10)
predictions <- matrix(c(0), nrow = 10, ncol = nrow(cv), T)
by(menagerie1, menagerie1[,17], function(x) x)
l <- sapply(v, function(x)  {
  sam <- sample_n(train, 60, T)
  predictions[x, ] <- nb(menagerie_true, sam, cv, 17)
  })

  tewyutuetrwut
