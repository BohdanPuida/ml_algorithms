install.packages("varhandle")
library(varhandle)
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

train <- iris1[1:100, ]
cv <- iris1[131:150, -5]
cv <- iris1[101:130, -5]
dim(train)
cv
train
train1 <- sample_n(train, 130)
train2 <- sample_n(train, 130)
train3 <- sample_n(train, 130)
train3


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
train <- menagerie1[1:70, ]
dim(train)
cv <- menagerie1[91:101, -17]
cv <- menagerie1[71:90, -17]
train
cv
v <- c(1:10)
predictions <- matrix(c(0), nrow = 10, ncol = nrow(cv), T)

l <- sapply(v, function(x)  {
  sam <- sample_n(menagerie1, 90, T)
  predictions[x, ] <- nb(menagerie_true, sam, cv, 17)
})
l
s <- apply(l, 1, function(x) names(sort(table(x), decreasing = T))[1])

r <- as.character(menagerie_true[rownames(cv), 17])
s
r
mis <- all.equal(r, s, check.attributes = F)
mis <- ifelse(mis == T,  0, as.numeric(str_extract(mis, "[0-9]+")))
acc <- ((nrow(cv) - mis) / nrow(cv)) * 100
acc
##########################################Cross validation




iris1
train <- iris1[1:100, ]
cv <- iris1[101:120, -5]
n <- 9
nr <- nrow(train)
splitted <- split(train, rep(1:ceiling(nr/n), each=n, length.out=nr))


options(warn=-1)


s <- as.data.frame(lapply(splitted, function(x) {
  x <- as.numeric(rownames(x))
  
  nb(menagerie1, train[-x, ], cv, 17)
}
  ))
s
s <- apply(s, 1, function(x) names(sort(table(x), decreasing = T))[1])
r <- as.character(menagerie_true[rownames(cv), 17])
s
r
mis <- all.equal(r, s, check.attributes = F)
mis <- ifelse(mis == T,  0, as.numeric(str_extract(mis, "[0-9]+")))
acc <- ((nrow(cv) - mis) / nrow(cv)) * 100
acc

#################Weighted voting cv #################################


iris1
train <- iris1[1:90, ]
cv <- iris1[91:120, -5]
test <- iris1[121:150, -5]
n <- 10
nr <- nrow(train)
splitted <- split(train, rep(1:ceiling(nr/n), each=n, length.out=nr))


options(warn=-1)


s <- as.data.frame(lapply(splitted, function(x) {
  x <- as.numeric(rownames(x))
  
  nb(iris, train[-x, ], cv, 5)
}
))
s
r <- as.character(iris[rownames(cv), 5])
s
r


stat_weights <- apply(s, 2, function(x) {
  x <- as.character(x)
  mis <- all.equal(r, x, check.attributes = F)
  mis <- ifelse(mis == T,  0, as.numeric(str_extract(mis, "[0-9]+")))
  acc <- ((nrow(cv) - mis) / nrow(cv)) * 100
  acc
  })

stat_weights



s
#zooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

iris1 <- as.data.frame(apply(iris[,-5], 2, function(x) as.integer(x))) ##simple discretization
iris1 <- cbind(iris1, iris[, 5]) #add Species to discretisized data set
iris1 <- sample_n(iris1,150) # random shuffle
colnames(iris1)[5] <- 'Species' # add name to relevant type

train <- iris1[1:70, ]
cv <- iris1[131:150, -5]



menagerie[,18]
menagerie_true  <- menagerie[, 2:18]
menagerie1 <- sample_n(menagerie_true, 101)
menagerie_true

menagerie1
train <- menagerie1[1:70, ]
cv <- menagerie1[91:101, -17]
train
cv
n <- 7
nr <- nrow(train)
splitted <- split(train, rep(1:ceiling(nr/n), each=n, length.out=nr))


options(warn=-1)


s <- as.data.frame(lapply(splitted, function(x) {
  x <- as.numeric(rownames(x))
  
  nb(menagerie_true, train[-x, ], cv, 17)
}
))
s
r <- as.character(menagerie_true[rownames(cv), 17])
r
s


##########################First cv, then test #################
stat_weights <- apply(s, 2, function(x) {
  x <- as.character(x)
  mis <- all.equal(r, x, check.attributes = F)
  mis <- ifelse(mis == T,  0, as.numeric(str_extract(mis, "[0-9]+")))
  acc <- ((nrow(cv) - mis) / nrow(cv)) * 100
  acc
})
###################Carefully#########################################

stat_weights


names(s) <- stat_weights
s <- vapply (
  FUN.VALUE = vector(mode = "character", length = 1), 
  FUN = function(x) names(x)[which(max(x) == x)][1],
  X = apply(s, 1, function(x) 
    by (
      data = x <- data.frame(x, names(s)), 
      INDICES = x[1], 
      FUN = function(y) sum(rep(1, times = nrow(y[1])) * unfactor(y[2]))
    )
  )
)

s
r
s <- all.equal(s, r, check.attributes = F)
s
s <- str_extract(s, "[0-9]+")

acc <- ((nrow(cv) - as.numeric(s) + 1) / nrow(cv)) * 100
acc
####################################################################### Bagging weighted

menagerie[,18]
menagerie_true  <- menagerie[, 2:18]
menagerie1 <- sample_n(menagerie_true, 101)
menagerie_true

menagerie1
train <- menagerie1[1:70, ]
dim(train)
cv <- menagerie1[92:101, -17]
train
cv
v <- c(1:10)
predictions <- matrix(c(0), nrow = 10, ncol = nrow(cv), T)

l <- sapply(v, function(x)  {
  sam <- sample_n(train, 100, T)
  predictions[x, ] <- nb(menagerie_true, sam, cv, 17)
})

r <- as.character(menagerie_true[rownames(cv), 17])
r
s <- l
s
r

stat_weights <- apply(s, 2, function(x) {
  x <- as.character(x)
  mis <- all.equal(r, x, check.attributes = F)
  mis <- ifelse(mis == T,  0, as.numeric(str_extract(mis, "[0-9]+")))
  acc <- ((nrow(cv) - mis) / nrow(cv)) * 100
  acc
})
stat_weights






# Assign weights

names(s) <- stat_weights
#Calculate summarised prediction

a <- vapply (
  FUN.VALUE = vector(mode = "character", length = 1), 
  FUN = function(x) names(x)[which(max(x) == x)][1],
  X = apply(s, 1, function(x) 
    by (
      data = x <- data.frame(x, names(s)), 
      INDICES = x[1], 
      FUN = function(y) sum(rep(1, times = nrow(y[1])) * unfactor(y[2]))
      )
    )
  )
a
r
a <- all.equal(a, r, check.attributes = F)
a
a <- str_extract(a, "[0-9]+")
a
acc <- ((nrow(cv) - as.numeric(a) ) / nrow(cv)) * 100
acc




##############################################################
