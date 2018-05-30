##################Bayes##############################
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


###########Unweighted bagging

######Zoo prep#####
menagerie[,18]
menagerie_true  <- menagerie[, 2:18]
menagerie1 <- sample_n(menagerie_true, 101)

train <- menagerie1[1:70, ]
cv <- menagerie1[71:90, -17]
test <- menagerie1[91:101, -17]

#####################end zoo prep
#############iris prep ###################
iris1 <- as.data.frame(apply(iris[,-5], 2, function(x) as.integer(x))) ##simple discretization
iris1 <- cbind(iris1, iris[, 5]) #add Species to discretisized data set
iris1 <- sample_n(iris1,150) # random shuffle
colnames(iris1)[5] <- 'Species' # add name to relevant type

train <- iris1[1:70, ]
cv <- iris1[71:130, -5]
test <- iris1[131:150, -5]
############end iris prep ##################
##################unweighted bagging ######################
unweighted_bagging <- function(data, train, cv, relevant_type) {
v <- c(1:10)
predictions <- matrix(c(0), nrow = 10, ncol = nrow(cv), T)
l <- sapply(v, function(x)  {
  sam <- sample_n(train, dim(train)[1], T)
  predictions[x, ] <- nb(data, sam, cv, relevant_type)
})
s <- apply(l, 1, function(x) names(sort(table(x), decreasing = T))[1])
r <- as.character(data [rownames(cv), relevant_type])
mis <- all.equal(r, s, check.attributes = F)
mis <- ifelse(mis == T,  0, as.numeric(str_extract(mis, "[0-9]+")))
acc <- ((nrow(cv) - mis) / nrow(cv)) * 100
acc
}
unweighted_bagging(iris, train, cv, 5)


##################unweighted cv ######################
unweighted_cv <- function(data, train, cv, relevant_type) {
n <- 0.1 * nrow(train)
nr <- nrow(train)
splitted <- split(train, rep(1:ceiling(nr/n), each=n, length.out=nr))
options(warn=-1)
s <- as.data.frame(lapply(splitted, function(x) {
  x <- as.numeric(rownames(x))
  
  nb(data, train[-x, ], cv, relevant_type)
}
))
s <- apply(s, 1, function(x) names(sort(table(x), decreasing = T))[1])
r <- as.character(data[rownames(cv), relevant_type])
mis <- all.equal(r, s, check.attributes = F)
mis <- ifelse(mis == T,  0, as.numeric(str_extract(mis, "[0-9]+")))
acc <- ((nrow(cv) - mis) / nrow(cv)) * 100
acc
}
unweighted_cv(iris, train, cv, 5)


###############Weighted voting cv
weighted_voting_cv <- function(data, train, cv, test, relevant_type) {
n <- nrow(train) * 0.1
nr <- nrow(train)
splitted <- split(train, rep(1:ceiling(nr/n), each=n, length.out=nr))
options(warn=-1)
s <- as.data.frame(lapply(splitted, function(x) {
  x <- as.numeric(rownames(x))
  nb(data, train[-x, ], cv, relevant_type)
}
))
r <- as.character(data[rownames(cv), relevant_type])
##########################First cv, then test #################
stat_weights <- apply(s, 2, function(x) {
  x <- as.character(x)
  mis <- all.equal(r, x, check.attributes = F)
  mis <- ifelse(mis == T,  0, as.numeric(str_extract(mis, "[0-9]+")))
  acc <- ((nrow(cv) - mis) / nrow(cv)) * 100
  acc
})
###################Carefully#########################################
s1 <- as.data.frame(lapply(splitted, function(x) {
  x <- as.numeric(rownames(x))
  nb(data, train[-x, ], test, relevant_type)
}
))
names(s1) <- stat_weights
s1 <- vapply (
  FUN.VALUE = vector(mode = "character", length = 1), 
  FUN = function(x) names(x)[which(max(x) == x)][1],
  X = apply(s1, 1, function(x) 
    by (
      data = x <- data.frame(x, names(s1)), 
      INDICES = x[1], 
      FUN = function(y) sum(rep(1, times = nrow(y[1])) * unfactor(y[2]))
    )
  )
)
r1 <- as.character(data[rownames(test), relevant_type])
s1 <- all.equal(s1, r1, check.attributes = F)
s1 <- str_extract(s1, "[0-9]+")
acc <- ((nrow(test) - as.numeric(s1) + 1) / nrow(test)) * 100

r1
}
weighted_voting_cv(iris, train, cv, test, 5)



############Weighted voting bagging


weighted_voting_bagging <- function(data, train, cv, test, relevant_type) {

v <- c(1:10)
predictions <- matrix(c(0), nrow = 10, ncol = nrow(cv), T)
s <- sapply(v, function(x)  {
  sam <- sample_n(train, dim(train)[1], T)
  predictions[x, ] <- nb(data, sam, cv, relevant_type)
})

r <- as.character(data[rownames(cv), relevant_type])




stat_weights <- apply(s, 2, function(x) {
  x <- as.character(x)
  mis <- all.equal(r, x, check.attributes = F)
  mis <- ifelse(mis == T,  0, as.numeric(str_extract(mis, "[0-9]+")))
  acc <- ((nrow(cv) - mis) / nrow(cv)) * 100
  acc
})
predictions <- matrix(c(0), nrow = 10, ncol = nrow(test), T)
s1 <- sapply(v, function(x)  {
  sam <- sample_n(train, dim(train)[1], T)
  predictions[x, ] <- nb(data, sam, test, relevant_type)
})
names(s1) <- stat_weights
a <- vapply (
  FUN.VALUE = vector(mode = "character", length = 1), 
  FUN = function(x) names(x)[which(max(x) == x)][1],
  X = apply(s1, 1, function(x) 
    by (
      data = x <- data.frame(x, names(s1)), 
      INDICES = x[1], 
      FUN = function(y) sum(rep(1, times = nrow(y[1])) * unfactor(y[2]))
    )
  )
)
r1 <- as.character(data[rownames(test), relevant_type])
a <- all.equal(a, r1, check.attributes = F)
a <- str_extract(a, "[0-9]+")
acc <- ((nrow(test) - as.numeric(a) ) / nrow(test)) * 100
acc
}

weighted_voting_bagging(iris, train, cv, test, 5)


##########################Dynamical wighted cv
dweighted_voting_cv <- function(data, train, cv, test, relevant_type) {
  n <- nrow(train) * 0.1
  nr <- nrow(train)
  splitted <- split(train, rep(1:ceiling(nr/n), each=n, length.out=nr))
  options(warn=-1)
  s <- as.data.frame(lapply(splitted, function(x) {
    x <- as.numeric(rownames(x))
    nb(data, train[-x, ], cv, relevant_type)
  }
  ))
  r <- as.character(data[rownames(cv), relevant_type])
  ##########################First cv, then test #################
  weights <- c(rep(1, 10))
  w <- list()
  for (i in 1:nrow(s)) 
    w[[i]] <- which(s[i, ]!= r[i])
  
  
  w <- lapply(w, function(x) {if(is.integer0(x) == T) {x <- 0}
    else { x <- x }})
  
  for(i in 1:length(w)) {
    if(w[[i]] == 0) {
      next
    }
    weights[c(w[[i]])] <- weights[c(w[[i]])] / 2
  }
  ###################Carefully#########################################
  s1 <- as.data.frame(lapply(splitted, function(x) {
    x <- as.numeric(rownames(x))
    nb(data, train[-x, ], test, relevant_type)
  }
  ))
  names(s1) <- weights
  s1 <-weighted_voting_bagging <- function(data, train, cv, test, relevant_type) {
    
    v <- c(1:10)
    predictions <- matrix(c(0), nrow = 10, ncol = nrow(cv), T)
    s <- sapply(v, function(x)  {
      sam <- sample_n(train, dim(train)[1], T)
      predictions[x, ] <- nb(data, sam, cv, relevant_type)
    })
    
    r <- as.character(data[rownames(cv), relevant_type])
    
    
    
    
    stat_weights <- apply(s, 2, function(x) {
      x <- as.character(x)
      mis <- all.equal(r, x, check.attributes = F)
      mis <- ifelse(mis == T,  0, as.numeric(str_extract(mis, "[0-9]+")))
      acc <- ((nrow(cv) - mis) / nrow(cv)) * 100
      acc
    })
    predictions <- matrix(c(0), nrow = 10, ncol = nrow(test), T)
    s1 <- sapply(v, function(x)  {
      sam <- sample_n(train, dim(train)[1], T)
      predictions[x, ] <- nb(data, sam, test, relevant_type)
    })
    names(s1) <- stat_weights
    a <- vapply (
      FUN.VALUE = vector(mode = "character", length = 1), 
      FUN = function(x) names(x)[which(max(x) == x)][1],
      X = apply(s1, 1, function(x) 
        by (
          data = x <- data.frame(x, names(s1)), 
          INDICES = x[1], 
          FUN = function(y) sum(rep(1, times = nrow(y[1])) * unfactor(y[2]))
        )
      )
    )

  r1 <- as.character(data[rownames(test), relevant_type])
  s1 <- all.equal(s1, r1, check.attributes = F)
  s1 <- str_extract(s1, "[0-9]+")
  acc <- ((nrow(test) - as.numeric(s1) + 1) / nrow(test)) * 100
  acc
}}

dweighted_voting_cv(iris, train, cv, test, 5)


###################################Dynamical voted bagging
dweighted_voting_bagging <- function(data, train, cv, test, relevant_type) {
  
  v <- c(1:10)
  predictions <- matrix(c(0), nrow = 10, ncol = nrow(cv), T)
  s <- sapply(v, function(x)  {
    sam <- sample_n(train, dim(train)[1], T)
    predictions[x, ] <- nb(data, sam, cv, relevant_type)
  })
  
  r <- as.character(data[rownames(cv), relevant_type])
  
  
  
  
  weights <- c(rep(1, 10))
  w <- list()
  for (i in 1:nrow(s)) 
    w[[i]] <- which(s[i, ]!= r[i])
  
  
  w <- lapply(w, function(x) {if(is.integer0(x) == T) {x <- 0}
    else { x <- x }})
  
  for(i in 1:length(w)) {
    if(w[[i]] == 0) {
      next
    }
    weights[c(w[[i]])] <- weights[c(w[[i]])] / 2
  }
  predictions <- matrix(c(0), nrow = 10, ncol = nrow(test), T)
  s1 <- sapply(v, function(x)  {
    sam <- sample_n(train, dim(train)[1], T)
    predictions[x, ] <- nb(data, sam, test, relevant_type)
  })
  weights <- weights + 0.5
  names(s1) <- weights
  a <- vapply (
    FUN.VALUE = vector(mode = "character", length = 1), 
    FUN = function(x) names(x)[which(max(x) == x)][1],
    X = apply(s1, 1, function(x) 
      by (
        data = x <- data.frame(x, names(s1)), 
        INDICES = x[1], 
        FUN = function(y) sum(rep(1, times = nrow(y[1])) * unfactor(y[2]))
      )
    )
  )
  r1 <- as.character(data[rownames(test), relevant_type])
  a <- all.equal(a, r1, check.attributes = F)
  a <- str_extract(a, "[0-9]+")
  acc <- ((nrow(test) - as.numeric(a) ) / nrow(test)) * 100
  acc
}






