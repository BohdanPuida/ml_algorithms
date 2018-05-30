knn2 <- function(k) {
  function(abs=none) {
    function(is_weighted) { 
     function(data, train, cv, relevant_type ) {
       train <- apply(train, 2, abs)
       cv <- apply(cv, 2, abs)
       distances <- apply(train, 1, function(x) apply(cv, 1, function(y) euclid(x, y) ))
       neighbours <- as.matrix(apply(distances, 1, function(x) as.numeric(names(sort(x)[1:k])))) ## k from kNN
       
       if( k == 1) {neighbours <- t(neighbours)}
       neighbours_classes <- apply(neighbours, 2, function(x) iris[x, 5] )
       
       if( k == 1 ) { 
         neighbours_class <- as.character(neighbours_classes) 
       } else {
         neighbours_class <- apply(neighbours_classes, 2, function(x) 
           ifelse( length(unique(x)) == 1, x, names(table(x))[1] #the most common element
           )
         )
       }
       
       true_positive_cv <- as.character(data[rownames(cv),relevant_type])
       mismatches <- all.equal(neighbours_class, true_positive_cv, check.attributes=F)
       mismatches <- ifelse(mismatches == T,  0, as.numeric(str_extract(mismatches, "[0-9]+")))
       acc <- ((dim(cv)[1] - mismatches) / dim(cv)[1]) * 100
       acc
       
       
       
      
       
       
       }
  }
 }
}

n <- c(1,3,5,7,9)
n
ab <- c('none', 'minimax', 'deviation')
ab
prct <- list(c(1:15), c(1:30), c(1:45), c(1:60), c(1:75), c(1:90), c(1:105), c(1:120), c(1:135))
prct1 <- rev(prct)

prct2 <- list(c(1:17), c(1:(2*17)), )

a <- as.vector(prct[1])
class(a[[1]])
a[[1]]
as.numeric(a)
knn2(5)(minimax)(F)(iris, iris1[1:100,1:4], iris1[101:150, 1:4], 5)
knn2(5)(minimax)(F)(iris, iris1[c(list(c(1:100))),1:4], iris1[101:150, 1:4], 5)
for(i in n){
  for(j in ab) {
    for(k in 1:9 ){
      sprintf("%f", i)
      cat(" Neighbours", i, "Norm", j, "Train part",k, "Acc ", 
              knn2(i)(j)(F)(iris, iris1[prct[k][[1]],1:4], iris1[prct[k][[1]], 1:4], 5) ,'\n')
    }
  }
}
for(i in n){
  for(j in ab) {
    for(k in 1:9 ){
      sprintf("%f", i)
      cat(" Neighbours", i, "Norm", j, "Train part",k, "Acc ", 
          knn2(i)(j)(F)(wine, iris1[prct[k][[1]],1:4], iris1[prct[k][[1]], 1:4], 5) ,'\n')
    }
  }
}
dim(wine)
zoo

iris1 <- prcomp(iris[,1:4],center = TRUE) $x[, 1:2]
iris1 <- as.data.frame(iris1)
iris1 <- sample_n(iris, 150)
iris1

minimax <- function(x) (x - min(x)) / (max(x) - min(x)) 
deviation <- function(x) (x - mean(x)) / sd(x)
none <- function(x) x
euclid <- function(x, y) sqrt( sum((x - y) ^ 2 ) )
iris1 <- sample_n(iris, 150)



a <- prcomp(menagerie[,2:17], T, T)
View(a$x[,1:2])
as.data.frame(a$x[,1:3])  %>% mutate(Type= menagerie[,18]) -> a
View(a)
install.packages("ggfortify")
library(ggfortify)
library(plotly)

autoplot(prcomp(df), data = iris, colour = 'Species', loadings = TRUE)
ggplotly(autoplot((prcomp(iris[,1:4])), data = iris, colour = 'Species', 
                  loadings = TRUE))
ggplotly(autoplot(prcomp(iris[,1:4]), data = iris, colour = 'Species',
                  loadings = TRUE, loadings.colour = 'blue',
                  loadings.label = TRUE, loadings.label.size = 3))
library(cluster)
ggplotly(autoplot(fanny(iris[-5], 3), frame = TRUE, type = 'norm', frame.type = 'norm'))
prcomp(iris)
prcomp(iris[,1:4],center = TRUE) $x[, 1:2]




library(ggplot2)
ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) + geom_point(size=3)
ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species, shape=Species)) + geom_point(size=3)
ggplot(iris, aes(x=Species,y=Sepal.Length)) + geom_boxplot()
ggplot(mtcars, aes(x=wt, y=mpg, color=as.factor(cyl))) + geom_line()
sprintf('a')
