replace_by_mean <- function(x) {
  x[is.na(x)]=mean(x,na.rm = T)
  round(x,1)
}



shuffle <- function(frame) {
  library(dplyr)
  shuffledDataFrame <- sample_n(frame, nrow(frame))
  return(shuffledDataFrame)
}

iris$Species <- factor(iris$Species, c(0,1,2))
MyIris <- shuffle(iris)
irisTest <- head(MyIris, n=50)
irisLearn <- tail(MyIris, n=100)
plot(iris$Sepal.Length ~ iris$Sepal.Width, 
     main="Scatterplot", 
     xlab="Sepal Width", 
     ylab="Sepal Length", 
     pch=20, col = "#3D59AB", 
     font = 2, font.sub = 2, font.main = 2, font.lab=2, font.axis = 2)

hist(iris$Petal.Length, 
     col = "#3D59AB", 
     main = "Petal Length Histogram",
     xlab="Petal Length",
     breaks = 25)




