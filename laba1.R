install.packages("RColorBrewer")
library(RColorBrewer)
iris <- read.table(file = 'iris', sep = ",")
names(iris) <- c ("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
wine <- read.table(file = 'wine', sep = ",")
names(wine) <- c("Alcohol", "MalicAcid", "Ash", "AshAlcanity", "Magnesium", 
                 "TotalFenols", "Flavanoids", "NonFlavanoidFenols", "Proanthocyanins", "Color intensity",
                 "Hue", "OD280/OD315", "Proline")
cancer <- read.table(file="cancer", sep=",", na.strings="?")
names(cancer) <- c("Sample code number","Clump Thickness","Uniformity of Cell Size","Uniformity of Cell Shape",
                   "Marginal Adhesion","Single Epithelial Cell Size","Bare Nuclei","Bland Chromatin","Normal Nucleoli"
                   ,"Mitoses","Class")
which(is.na(cancer))
View(cancer)
unique(spam$V58)
menagerie

shuffle <- function(frame) {
  library(dplyr)
  shuffledDataFrame <- sample_n(frame, nrow(frame))
  return(shuffledDataFrame)
}
splitD <- function(q, type, n) {
  library(caTools)
  set.seed(121)
  split <-  sample.split(q[[type]], SplitRatio = n)
  return(list(train = subset(q, split==T), test = subset(q, split==F)))
}
factorize <- function(x, type, z) {
y = x
cnames = colnames(y)
y = cbind(y, y[,ncol(y)])
colnames(y) = c(cnames,"Class1")

levels(y$Class) <- 1:z
return(y)
}
replace_by_mean <- function(x) {
 
  x[is.na(x)]=mean(x,na.rm = T)
  round(x,1)
  
}
impute <- function(x, type, f) {
ind = c(which(sapply(x, function(y) which(class(y)=="numeric") ==1 || which(class(y)=="integer")) == 1))
y = as.data.frame(do.call("rbind" , by(x[, ind], x[type], function(piece) sapply(piece, f))))
v = as.data.frame(x[,-ind])
y = as.data.frame(append(y, v))
colnames(y) = colnames(x)
return(y)
}
preprocess <- function(df, type, f) {
     df <- shuffle(df)
     df <-  impute(df, type, f)
     return(df)
}

menagerie <- zoo
menagerie <- preprocess(menagerie, "type", replace_by_mean)
menagerie
splitD(iris2, "Species", 0.7)
preprocess(wine, "Alcohol", replace_by_mean )
splitD(wine,"Alcohol", 0.7)


iris2 <- preprocess(iris, "Species", replace_by_mean)
iris2
cancer2 <- preprocess(cancer, "Class", replace_by_mean)
cancer2
which(is.na(cancer2) ==T)
preprocess(spam, 'V58', replace_by_mean)
impute(zoo, 'type', replace_by_mean )






iris2 <- iris
preprocess(iris2, iris2["Species"], replace_by_mean)



y = iris2
y
colnames(iris2)
cnames = colnames(y)
cnames
y = cbind(y, y[,ncol(y)])
y
colnames(y) = c(cnames, "Class")
levels(y$Class) <- c(1:length(levels(iris$Species)))
levels(y$Class)
y
preprocess(y, "Species", replace_by_mean)

y = cancer
colnames(cancer)
cnames = colnames(y)
y = cbind(y, y[,ncol(y)])
colnames(y) = c(cnames, "Class1")
levels(y$Class1) <- c(1:length(levels(cancer$Class)))
y <- preprocess(y, "Class", replace_by_mean)

which(is.na(y)==T)

View(y)
y=wine
colnames(wine)
cnames = colnames(y)
y = cbind(y, y[,ncol(y)])
colnames(y) = c(cnames, "Class")
levels(y$Class) <- c(1:length(levels(wine$Alcohol)))
y <- preprocess(y, "Class", replace_by_mean)
y









plot(iris2$Sepal.Length ~ iris2$Sepal.Width,
     main="Scatterplot", 
     xlab="Sepal Width", 
     ylab="Sepal Length", 
     pch=20, col = "#3D59AB", 
     font = 2, font.sub = 2, font.main = 2, font.lab=2, font.axis = 2)

x <- hist(iris$Sepal.Length)
segments(x0=x$mids-0.25, x1=x$mids+0.25, y0=x$counts, y1=x$counts, lw=4, col="red")
plot(Sepal.Width ~ Sepal.Length, data=iris, col=brewer.pal(3, "Set2")[iris$Species])
legend(x=6.5, y=4.5, legend=levels(iris$Species), col=brewer.pal(3, "Set2"), pch=1)
hist(iris$Petal.Length, 
     col = "#3D59AB", 
     main = "Petal Length Histogram",
     xlab="Petal Length",
     breaks = 25)
