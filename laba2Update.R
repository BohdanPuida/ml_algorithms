install.packages("RcppArmadillo")
getwd()
install.packages("markdown")
setwd("/home/bogan")
zoo <- read.table(file = 'zoo', sep = ",")
names(zoo) <- c ( "animal name", 
                  "hair", "feathers", "eggs", "milk", 
                  "airborne", "aquatic", "predator", "toothed",	"backbone",	"breathes", 
                  "venomous",	"fins",	"legs",	"tail",	"domestic", "catsize", "type" )
View(zoo)

menagerie <- zoo
menagerie$type <- factor(menagerie$type, 1:7, 
                         c("mammal", "bird", "reptilia", "fish", 
                           "amphibias","insect", "other"))

findS <- function(table, id) {
  do.call("rbind", by(table, table[id], function(piece) 
    sapply(piece, function(item) 
      ifelse(length(unique(item)) > 1, '?', item))))
  }

findS(menagerie[2, ncol(menagerie)], menagerie["type"])

View((function(table, id) do.call("rbind", by(table, table[id], function(piece) sapply(piece, function(item)
    ifelse(length(unique(item)) > 1, '?', item))))) (menagerie, "type"), "S hypothesis")


s <- findS(menagerie, "type")
View(s[,2:17], "S hypothesis")


candidateElimination <- function(table, type, id) {
  s = findS(table, type)
  cI = s[id, 2:(ncol(table)-1)]
  cI1 = cI
  nE = menagerie[menagerie[type]!=id, 2:(ncol(table)-1)]
  nE = as.matrix(nE)
  cI = as.matrix(cI)
  w = c()
  a = which ( t ( as.matrix(apply(nE, 2 ,function(item) 
    if (length(unique(item)) > 1) w <- append(w, 1)
    else w <- append(w, 0)))) > 0)
  cI[a] = '?' 
  cI = as.data.frame(cI)
  G = cI
  return (list(S = cI1, G = G))
}
a <- candidateElimination(menagerie, "type", "mammal")
View(a$S, "S hypothesis")
View(t(a$G), "G hypothesis")
library(sets)


a$S <- a$S[-which(a$S == '?')]
a$G <- a$G[-which(a$G == '?')]
if(which(colnames(a$S)==colnames(a$G))) a$S <- a$S[ -which(colnames(a$S)==colnames(a$G))]
av <- 2^as.set(as.vector(seq(1,ncol(a$S),1)))
av <- sapply(av, function(item)  a$S[c(as.vector(as.numeric(item)))])[-1]
bb <- 2^as.set(as.vector(seq(1,ncol(a$G),1)))
bb <- sapply(bb, function(item)  a$G[c(as.vector(as.numeric(item)))])[-1]
VS <- lapply( av, function(item) item <- as.data.frame(c(item,bb)))
VS
View(menagerie)

f <- log(2/3,2)*(-2/3)
g <- log(1/3, 2)*(1/3)
g <- f-g
g
1-0.5*g-0.5*g
f <- log(3/4,2)*(-3/4)
f
g <- log(1/4, 2)*(1/4)
g
g <- f-g
g
0.666666666666666*0.81
1-0.54
a <- log((2/20)*(1/20)*(2/20)*(1/20)*(3/6))
b <- log((1/2)*(2/20)*(4/20)*(2/20)*(3/20))
a
b
b-a
e <- 2.71828182845904

(e^-11.29)/((e^-11.29)+(e^-8.8049))



x <- 10
f1 <- function(x) {
  function() {
    x + 10
  }
}
f1(1)()
`+`(1, `*`(2, 3))
f2 <- function(a, b) {
  a+b
}
f2(10)


j <- function(x) {
  y = 2
  function() {
    c(x, y)
  }
}
k <- j(1)
k
k()
rm(j, k, y)
d <- c(1:10)
`+`(d)
install.packages("shiny-incubator")


library(devtools)

install_github('shiny-incubator', 'rstudio')
library(shinyIncubator)
help("matrixInput")


















library(rpart)
library(rattle)
library(RColorBrewer)
library(rpart.plot)
install.packages("rattle")

train$Date2 <- as.numeric(unclass(as.POSIXct(train$Dates)))
test$Date2 <- as.numeric(unclass(as.POSIXct(test$Dates)))

print("Values Created")

tree <- rpart(iris$type~ .,
              data = menagerie,
              method = "class",
              control = rpart.control(minsplit = 200,cp=0)
)
class(tree)
rpart.plot(tree)
fancyRpartPlot(tree)

# sum(predict(object = tree,newdata = sampleTrain,type = "class")!=sampleTrain$Category)

predicted <- predict(object = tree,newdata = test)
final <- data.frame(Id = test$Id , predicted)
colnames(final)  <- c("Id",levels(train$Category))

write.csv(final,file = "my_sub.csv",row.names = FALSE,quote = F)
