menagerie[,18]
menagerie_true  <- menagerie[, 2:18]
menagerie1 <- sample_n(menagerie_true, 101)
menagerie_true

menagerie1
train <- menagerie1[1:70, ]
dim(train)
cv <- menagerie1[71:91, -17]
train
cv
v <- c(1:10)
predictions <- matrix(c(0), nrow = 10, ncol = nrow(cv), T)

l <- sapply(v, function(x)  {
  sam <- sample_n(train, 100, T)
  predictions[x, ] <- nb(iris, sam, cv, 5)
})
l
r <- as.character(iris[rownames(cv), 5])
r
s <- l

s
r









s

names(s)
names(s) <- weights
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










###############Bagging dynamically weighted ############# 
weights <- c(rep(1, 10))
w <- list()
for (i in 1:nrow(s)) 
w[[i]] <- which(s[i, ]!= r[i])
  

w <- lapply(w, function(x) {if(is.integer0(x) == T) {x <- 0}
       else { x <- x }})
w
for(i in 1:length(w)) {
  if(w[[i]] == 0) {
    next
  }
 weights[c(w[[i]])] <- weights[c(w[[i]])] / 2
}
weights
























####################################cv dynamically weighted ######################
menagerie[,18]
menagerie_true  <- menagerie[, 2:18]
menagerie1 <- sample_n(menagerie_true, 101)
menagerie_true

menagerie1
train <- menagerie1[1:70, ]
cv <- menagerie1[91:101, -17]
train
cv
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
r
s


##########################First cv, then test #################
weights <- c(rep(1, 10))
w <- list()
for (i in 1:nrow(s)) 
  w[[i]] <- which(s[i, ]!= r[i])


w <- lapply(w, function(x) {if(is.integer0(x) == T) {x <- 0}
  else { x <- x }})
w
for(i in 1:length(w)) {
  if(w[[i]] == 0) {
    next
  }
  weights[c(w[[i]])] <- weights[c(w[[i]])] / 2
}
weights
###################Carefully#########################################




names(s) <- weights
s <-  vapply (
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
apply(s, 1, function(x) 
  by (
    data = x <- data.frame(x, names(s)), 
    INDICES = x[1], 
    FUN = function(y) sum(rep(1, times = nrow(y[1])) * unfactor(y[2]))
  )
)
s
r
s <- all.equal(s, r, check.attributes = F)
s
s <- str_extract(s, "[0-9]+")
s
acc <- ((nrow(cv) - as.numeric(s) + 1) / nrow(cv)) * 100
acc

