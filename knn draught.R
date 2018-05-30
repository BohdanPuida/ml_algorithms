iris1 <- sample_n(iris, 150)
wine1 <- sample_n(wine, 178)
train <- iris1[1:135, 1:4]
train <- wine1[1:130, 2:13]
train
cv <- wine1[131:178, 2:13]
cv
train <- apply(train, 2, minimax)
cv <- apply(cv, 2, minimax)
distances <- apply(train, 1, function(x) apply(cv, 1, function(y) euclid(x, y) ))
distances
neighbours <- as.matrix(apply(distances, 1, function(x) as.numeric(names(sort(x)[1:7])))) ## k from kNN
neighbours
if( 1 == 1) neighbours <- t(neighbours)
neighbours_classes <- apply(neighbours, 2, function(x) wine[x, 1] )
neighbours_classes
if( 1 == 1 ) { 
  neighbours_class <- as.character(neighbours_classes) 
} else {
    neighbours_class <- apply(neighbours_classes, 2, function(x) 
    ifelse( length(unique(x)) == 1, x, names(sort(table(x), decreasing = T))[1] #the most common element
    )
  )
}
neighbours_class
length(neighbours_class)
dim(cv)
true_positive_cv

true_positive_cv <- as.character(wine[rownames(cv),1])
length(true_positive_cv)
mismatches <- all.equal(as.character(neighbours_class), true_positive_cv, check.attributes=F)
mismatches
mismatches <- ifelse(mismatches == T,  0, as.numeric(str_extract(mismatches, "[0-9]+")))
acc <- ((dim(cv)[1] - mismatches) / dim(cv)[1]) * 100
acc
unweighted_minimax_7nn(wine, wine1[1:130, 2:13], wine1[131:178, 2:13], 1 )
################## WEIGHTED KNN ##################################
min_distances <- as.matrix(apply(distances, 1, function(x) (sort(x)[1:5])))
min_distances
neighbours <- as.matrix(apply(distances, 1, function(x) as.numeric(names(sort(x)[1:5])))) ## k from kNN
neighbours
neighbours_classes <- apply(neighbours, 2, function(x) iris[x, 5] )
neighbours_classes
a <- list()
for(i in 1:ncol(neighbours_classes)){a[[i]] <- data.frame(min_distances[,i],neighbours_classes[,i])}
a
a <- lapply(a, function(x) by(x, x[[2]], function(y) sum(1/y[[1]]^2)))
a
b <- iris[as.numeric(colnames(neighbours_classes)), 5]
b
a <- lapply(a, function(x) ifelse(length(x) >1, rownames(sort(x))[1], rownames(x)))
a
a <- unlist(a)
a
b <-   as.character(b)
b
f <- all.equal(a, b)
f
f <- as.numeric(str_extract(f, "[0-9]+"))
f
acc <- ((dim(neighbours_classes)[2]-f)/dim(neighbours_classes)[2]) *100
acc
weighted_none_3nn(iris, iris1[1:135, 1:4], iris1[136:150, 1:4], 5)