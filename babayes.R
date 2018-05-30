library(dplyr)
library(stringr)
library(arules)
library(discretization)
library(ggplot2)
library(plotly)

iris1 <- as.data.frame(apply(iris[,-5], 2, function(x) as.integer(x))) ##simple discretization
iris1 <- cbind(iris1, iris[, 5]) #add Species to discretisized data set
iris1 <- sample_n(iris1,150) # random shuffle
colnames(iris1)[5] <- 'Species' # add name to relevant type

wine1 <-  as.data.frame(apply(wine[, 1:13], 2, function(x) as.integer(x)))
wine1 <- sample_n(wine1, 178)
wine1



dim(menagerie)


View(menagerie)

menagerie1 <- sample_n(menagerie, 101)








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
a <- append(a, rep(0, n))
a <- as.data.frame(matrix(c(a), length(unique(data[, relevant_type])),  nrow(cv), T))
rownames(a) <- v
a <- t(a)
a <- apply(a, 1, function(x) names(x)[which(max(x) == x)])
b <- as.character(data[rownames(cv), relevant_type])
mismatches <- all.equal(a, b, check.attributes = F)
mismatches <- ifelse(mismatches == T,  0, as.numeric(str_extract(mismatches, "[0-9]+")))
acc <- ((nrow(cv) - mismatches) / nrow(cv)) * 100
acc
}


prct <- list(c(1:15), c(1:30), c(1:45), c(1:60), c(1:75), c(1:90), c(1:105), c(1:120), c(1:135))
prct1 <- rev(prct)
prct <- list(c(1:17), c(1:34), c(1:51), c(1:68), c(1:85), c(1:102), c(1:119), c(1:136), c(1:153))
prct1 <- rev(prct)

prct <- list(c(1:10), c(1:20), c(1:30), c(1:40), c(1:50), c(1:60), c(1:70), c(1:80), c(1:90))
prct1 <- rev(prct)
prct1
j <- c()
for(i in 1:9) {
 j[i] <- nb(wine, wine1[prct[i][[1]], ], wine1[prct[i][[1]], -1 ], 1)  
}
for(i in 1:9) {
  j[i] <- nb(iris, iris1[prct[i][[1]], ], iris1[prct[i][[1]], -5 ], 5)  
}
for(i in 1:9) {
  j[i] <- nb(menagerie, menagerie1[prct[i][[1]], ], menagerie1[prct[i][[1]], -18 ], 18)  
}
menagerie1
j
train_part <- c(seq(10,90,10))
accuracy <- c(j)
a <- data.frame(train_part, accuracy)

p <- ggplotly(ggplot(a, aes(x=train_part, y=accuracy)) + geom_line() + 
                ggtitle("Naive Bayes for zoo "))
p
p <- ggplot(a, aes(x=train_part, y=accuracy)) + geom_line() + 
                ggtitle("Naive Bayes for iris ")
p <- ggplotly(ggplot(a, aes(x=train_part, y=accuracy)) + geom_line() + 
                ggtitle("Naive Bayes for wine "))
p
p <- ggplot(a, aes(x=train_part, y=accuracy)) + geom_line() + 
  ggtitle("Naive Bayes for wine ")
ggplotly(p)
api_create(p, filename = "NB-wine")
api_create(p, filename = "NB-iris")
#########################################################################################################Palyground








a
v <- names(a)
a <- unlist(a)
n <- length(unique(iris[, 5]))*nrow(cv)-length(a)
a <- append(a, rep(0, n))
a <- as.data.frame(matrix(c(a), length(unique(iris[, 5])),  nrow(cv), T))
rownames(a) <- v
a <- t(a)
a <- apply(a, 1, function(x) names(x)[which(max(x) == x)])
b <- as.character(iris[rownames(cv), 5])
mismatches <- all.equal(a, b, check.attributes = F)
mismatches <- ifelse(mismatches == T,  0, as.numeric(str_extract(mismatches, "[0-9]+")))
acc <- ((nrow(cv) - mismatches) / nrow(cv)) * 100
acc

f <- iris1[2:20, ]
dim(f)
cv <- iris1[140:150, -5]
cv
f <- sample_n(f, 19, T)
f
a <- by(f[, -5], f[, 5], function(x) {
  b <- as.data.frame(x) # from list (lol) to df
  d <- apply(b, 1, function(y) apply(cv, 1, function(z) as.numeric(`==`(y, z))))  #check whether train data is consistent with cv (say no to loops)
  l <- apply(d, 1, function(x) colSums(matrix(x, nrow = nrow(b))))  
  l[which(l == 0)] <- 1 # log(0) error
  l <- matrix(c(l), nrow(cv), ncol(cv), T)
  l <- l/nrow(b)
  s <- nrow(b)/ nrow(f)
  s <- log(l) + log(s)
  s <- apply(s, 1, sum)
  s
  
} 

)
v <- names(a)
a <- unlist(a)
n <- length(unique(iris[, 5]))*nrow(cv)-length(a)
a <- append(a, rep(0, n))
a <- as.data.frame(matrix(c(a), length(unique(iris[, 5])),  nrow(cv), T))
rownames(a) <- v
a <- t(a)
a <- apply(a, 1, function(x) names(x)[which(max(x) == x)])
b <- as.character(iris[rownames(cv), 5])
a
b
mismatches <- all.equal(a, b, check.attributes = F)
mismatches <- ifelse(mismatches == T,  0, as.numeric(str_extract(mismatches, "[0-9]+")))
acc <- ((nrow(cv) - mismatches) / nrow(cv)) * 100
acc




########################################################################
prct <- list(c(1:15), c(1:30), c(1:45), c(1:60), c(1:75), c(1:90), c(1:105), c(1:120), c(1:135))
prct1 <- rev(prct)

