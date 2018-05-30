library(dplyr)
library(ggplot2)
library(stringr)
library(magrittr)
library(plotly)
#####Functional monster######

knn2 <- function(k) {
  function(abs=none) {
    function(is_weighted) { 
      function(data, train, cv, relevant_type ) {
        train <- apply(train, 2, abs)
        cv <- apply(cv, 2, abs)
        distances <- apply(train, 1, function(x) apply(cv, 1, function(y) euclid(x, y) ))
        neighbours <- as.matrix(apply(distances, 1, function(x) as.numeric(names(sort(x)[1:k])))) ## k from kNN
        
        if( k == 1) {neighbours <- t(neighbours)}
        neighbours_classes <- apply(neighbours, 2, function(x) data[x, relevant_type] )
        
        if( k == 1 ) { 
          neighbours_class <- as.character(neighbours_classes) 
        } else {
          neighbours_class <- apply(neighbours_classes, 2, function(x) 
            ifelse( length(unique(x)) == 1, x, names(sort(table(x), decreasing = T))[1] 
            )
          )
        }
        if (is_weighted == T) {
          min_distances <- as.matrix(apply(distances, 1, function(x) (sort(x)[1:k])))
          neighbours <- as.matrix(apply(distances, 1, function(x) as.numeric(names(sort(x)[1:k])))) ## k from kNN
          neighbours_classes <- apply(neighbours, 2, function(x) data[x, relevant_type] )
          a <- list()
          for(i in 1:ncol(neighbours_classes)){a[[i]] <- data.frame(min_distances[,i],neighbours_classes[,i])}
          a <- lapply(a, function(x) by(x, x[[2]], function(y) sum(1/y[[1]]^2)))
          b <- data[as.numeric(colnames(neighbours_classes)), relevant_type]
          a <- lapply(a, function(x) ifelse(length(x) >1, rownames(sort(x))[1], rownames(x)))
          a <- unlist(a)
          b <- as.character(b)
          f <- all.equal(a, b)
          f <- as.numeric(str_extract(f, "[0-9]+"))
          acc <- ((dim(neighbours_classes)[2]-f)/dim(neighbours_classes)[2]) *100
          return(acc)
        } else {
        true_positive_cv <- as.character(data[rownames(cv),relevant_type])
        mismatches <- all.equal(neighbours_class, true_positive_cv, check.attributes=F)
        mismatches <- ifelse(mismatches == T,  0, as.numeric(str_extract(mismatches, "[0-9]+")))
        acc <- ((dim(cv)[1] - mismatches) / dim(cv)[1]) * 100
        return(acc)
        }
      }
    }
  }
}
#####Settings########
iris1 <- sample_n(iris, 150)
nn5 <- knn2(5)
nn1 <- knn2(1)
nn3 <- knn2(3)
nn7 <- knn2(7)
nn9 <- knn2(9)

minimax <- function(x) (x - min(x))/(max(x) - min(x))
euclid <- function(x, y) sqrt(sum( (x - y) ^ 2))
deviation <- function(x) (x - mean(x)) / sd(x)
none <- function(x) x
###1###abs####
minimax_1nn <- nn1(minimax)
none_1nn <- nn1(none)
deviation_1nn <- nn1(deviation)
####3#####abs######
minimax_3nn <- nn3(minimax)
none_3nn <- nn3(none)
deviation_3nn <- nn3(deviation)
#####5#####abs########
minimax_5nn <- nn5(minimax)
none_5nn <- nn5(none)
deviation_5nn <- nn5(deviation)
#####7#####abs########
minimax_7nn <- nn7(minimax)
none_7nn <- nn7(none)
deviation_7nn <- nn7(deviation)
#####9#####abs########
minimax_9nn <- nn9(minimax)
none_9nn <- nn9(none)
deviation_9nn <- nn9(deviation)


##3##weighted
weighted_minimax_3nn <- minimax_3nn(T)
weighted_deviation_3nn <- deviation_3nn(T)
weighted_none_3nn <- none_3nn(T)
##5##weighted
weighted_minimax_5nn <- minimax_5nn(T)
weighted_deviation_5nn <- deviation_5nn(T)
weighted_none_5nn <- none_5nn(T)
##7##weighted
weighted_minimax_7nn <- minimax_7nn(T)
weighted_deviation_7nn <- deviation_7nn(T)
weighted_none_7nn <- none_7nn(T)
##9##weighted
weighted_minimax_9nn <- minimax_9nn(T)
weighted_deviation_9nn <- deviation_9nn(T)
weighted_none_9nn <- none_9nn(T)

###1###unweighted####
unweighted_minimax_1nn <- minimax_1nn(F)
unweighted_deviation_1nn <- deviation_1nn(F)
unweighted_none_1nn <- none_1nn(F)
###3###unweighted####
unweighted_minimax_3nn <- minimax_3nn(F)
unweighted_deviation_3nn <- deviation_3nn(F)
unweighted_none_3nn <- none_3nn(F)
###5###unweighted####
unweighted_minimax_5nn <- minimax_5nn(F)
unweighted_deviation_5nn <- deviation_5nn(F)
unweighted_none_5nn <- none_5nn(F)
###7###unweighted####
unweighted_minimax_7nn <- minimax_7nn(F)
unweighted_deviation_7nn <- deviation_7nn(F)
unweighted_none_7nn <- none_7nn(F)
###9###unweighted####
unweighted_minimax_9nn <- minimax_9nn(F)
unweighted_deviation_9nn <- deviation_9nn(F)
unweighted_none_9nn <- none_9nn(F)


####Iris#################
prct <- list(c(1:15), c(1:30), c(1:45), c(1:60), c(1:75), c(1:90), c(1:105), c(1:120), c(1:135))
prct1 <- rev(prct)
wm3 <- c()
wm5 <- c()
wm7 <- c()
wm9 <- c()
um1 <- c()
um3 <- c()
um5 <- c()
um7 <- c()
um9 <- c()
wd3 <- c()
wd5 <- c()
wd7 <- c()
wd9 <- c()
ud1 <- c()
ud3 <- c()
ud5 <- c()
ud7 <- c()
ud9 <- c()
wn3 <- c()
wn5 <- c()
wn7 <- c()
wn9 <- c()
un1 <- c()
un3 <- c()
un5 <- c()
un7 <- c()
un9 <- c()
#############I hate loops###
for(k in 1:9 ){
      cat("Weighted minimax 3nn", "Train part",k,"0% Acc ", 
          weighted_minimax_3nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5) ,'\n')
  wm3[k] <- weighted_minimax_3nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5)
  cat("Weighted minimax 5nn", "Train part",k,"0% Acc ", 
      weighted_minimax_5nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5) ,'\n')
  wm5[k] <- weighted_minimax_5nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5)
  cat("Weighted minimax 7nn", "Train part",k,"0% Acc ", 
      weighted_minimax_7nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5) ,'\n')
  wm7[k] <- weighted_minimax_7nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5)
  cat("Weighted minimax 9nn", "Train part",k,"0% Acc ", 
      weighted_minimax_9nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5) ,'\n')
  wm9[k] <- weighted_minimax_9nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5)
  cat("unWeighted minimax 1nn", "Train part",k,"0% Acc ", 
      unweighted_minimax_1nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5) ,'\n')
  um1[k] <- unweighted_minimax_1nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5)
  cat("unWeighted minimax 3nn", "Train part",k,"0% Acc ", 
      unweighted_minimax_3nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5) ,'\n')
  um3[k] <- unweighted_minimax_3nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5)
  cat("unWeighted minimax 5nn", "Train part",k,"0% Acc ", 
      unweighted_minimax_5nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5) ,'\n')
  um5[k] <- unweighted_minimax_5nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5)
  cat("unWeighted minimax 7nn", "Train part",k,"0% Acc ", 
      unweighted_minimax_7nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5) ,'\n')
  um7[k] <- unweighted_minimax_7nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5)
  cat("unWeighted minimax 9nn", "Train part",k,"0% Acc ", 
      unweighted_minimax_9nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5) ,'\n')
  um9[k] <- unweighted_minimax_9nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5)
  cat("Weighted deviation 3nn", "Train part",k,"0% Acc ", 
      weighted_deviation_3nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5) ,'\n')
  wd3[k] <- weighted_deviation_3nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5)
  cat("Weighted deviation 5nn", "Train part",k,"0% Acc ", 
      weighted_deviation_5nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5) ,'\n')
  wd5[k] <- weighted_deviation_5nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5)
  cat("Weighted deviation 7nn", "Train part",k,"0% Acc ", 
      weighted_deviation_7nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5) ,'\n')
  wd7[k] <- weighted_deviation_7nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5)
  cat("Weighted deviation 9nn", "Train part",k,"0% Acc ", 
      weighted_deviation_9nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5) ,'\n')
  wd9[k] <- weighted_deviation_9nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5)
  cat("unWeighted deviation 1nn", "Train part",k,"0% Acc ", 
      unweighted_deviation_1nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5) ,'\n')
  ud1[k] <- unweighted_deviation_1nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5)
  cat("unWeighted deviation 3nn", "Train part",k,"0% Acc ", 
      unweighted_deviation_3nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5) ,'\n')
  ud3[k] <- unweighted_deviation_3nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5)
  cat("unWeighted deviation 5nn", "Train part",k,"0% Acc ", 
      unweighted_deviation_5nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5) ,'\n')
  ud5[k] <- unweighted_deviation_5nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5)
  cat("unWeighted deviation 7nn", "Train part",k,"0% Acc ", 
      unweighted_deviation_7nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5) ,'\n')
  ud7[k] <- unweighted_deviation_7nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5)
  cat("unWeighted deviation 9nn", "Train part",k,"0% Acc ", 
      unweighted_deviation_9nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5) ,'\n')
  ud9[k] <- unweighted_deviation_9nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5)
  cat("Weighted none 3nn", "Train part",k,"0% Acc ", 
      weighted_none_3nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5) ,'\n')
  wn3[k] <- weighted_none_3nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5)
  cat("Weighted none 5nn", "Train part",k,"0% Acc ", 
      weighted_none_5nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5) ,'\n')
  wn5[k] <- weighted_none_5nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5)
  cat("Weighted none 7nn", "Train part",k,"0% Acc ", 
      weighted_none_7nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5) ,'\n')
  wn7[k] <- weighted_none_7nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5)
  cat("Weighted none 9nn", "Train part",k,"0% Acc ", 
      weighted_none_9nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5) ,'\n')
  wn9[k] <- weighted_none_9nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5)
  cat("unWeighted none 1nn", "Train part",k,"0% Acc ", 
      unweighted_none_1nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5) ,'\n')
  un1[k] <- unweighted_none_1nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5)
  cat("unWeighted none 3nn", "Train part",k,"0% Acc ", 
      unweighted_none_3nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5) ,'\n')
  un3[k] <- unweighted_none_3nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5)
  cat("unWeighted none 5nn", "Train part",k,"0% Acc ", 
      unweighted_none_5nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5) ,'\n')
  un5[k] <- unweighted_none_5nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5)
  cat("unWeighted none 7nn", "Train part",k,"0% Acc ", 
      unweighted_none_7nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5) ,'\n')
  un7[k] <- unweighted_none_7nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5)
  cat("unWeighted none 9nn", "Train part",k,"0% Acc ", 
      unweighted_none_9nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5) ,'\n')
  un9[k] <- unweighted_none_9nn(iris, iris1[prct[k][[1]],1:4], iris1[prct1[k][[1]], 1:4], 5)
  
}

###Plots#####################

part <- c(rep(seq(10,90,10), 5))
acc <- c(um1, um3, um5, um7, um9)
type <- c(rep("um1", 9), rep("um3", 9), rep("um5",9), rep("um7",9), rep("um9", 9))
a <- data.frame(part, acc, type)

p <- ggplotly(ggplot(a, aes(x=part, y=acc, col= type)) + geom_line())
p
api_create(p, filename = "unweighted-minimax-knn-plain-graph")
part <- c(rep(seq(10,90,10), 15))
acc <- c(um1, um3, um5, um7, um9, ud1, ud3, ud5, ud7, ud9, un1, un3, un5, un7, un9)
abs <- c(rep('minimax', 45), rep('deviation', 45), rep('none', 45))
type <- c(rep('u1', 9), rep('u3', 9), rep('u5', 9), rep('u7', 9), rep('u9', 9))
type <- rep(type, 3)
b <- data.frame(part, acc, abs, type)

p <- b %>%
  plot_ly(
    x = ~part, 
    y = ~acc, 
    color = ~abs, 
    frame = ~type, 
    text = ~abs, 

    hoverinfo = "text",
    type = 'scatter',
    mode = 'lines'
  ) 
p <- p %>% 
  animation_opts(
    1000, easing = "elastic", redraw = FALSE
  )
p <- p %>% 
  animation_button(
    x = 1, xanchor = "right", y = 0, yanchor = "bottom"
  )
p
api_create(p, filename = "unweighted-knn-animation-iris")


acc <- c( wm3,  wm5, wm7, wm9,
         wd3,  wd5,  wd7,  wd9,
           wn3, wn5,  wn7,  wn9)
abs <- c(rep('minimax', 36), rep('deviation', 36), rep('none', 36))
part <- c(rep(seq(10,90,10), 12))
type <- c(rep('w3', 9), rep('w5', 9), rep('w7', 9), rep('w9', 9))
type <- rep(type, 3)
n <- data.frame(part,acc,abs,type)
p <- n %>%
  plot_ly(
    x = ~part, 
    y = ~acc, 
    color = ~abs, 
    frame = ~type, 
    text = ~abs, 
    
    hoverinfo = "text",
    type = 'scatter',
    mode = 'lines'
  ) 
p <- p %>% 
  animation_opts(
    1000, easing = "elastic", redraw = FALSE
  )
p <- p %>% 
  animation_button(
    x = 1, xanchor = "right", y = 0, yanchor = "bottom"
  )
p
library(cluster)
ggplotly(autoplot(fanny(iris[-5], 3), frame = TRUE, type = 'norm', frame.type = 'norm'))
api_create(p, filename = "weighted-knn-animation-iris")


Sys.setenv("plotly_username"="ggml")
Sys.setenv("plotly_api_key"="FTM4DcyzmbAY5CA4SeJy")




View(wine)
View(wine1)
####Wine#################
wine1 <- sample_n(wine, 178)
wine1
prct <- list(c(1:17), c(1:34), c(1:51), c(1:68), c(1:85), c(1:102), c(1:119), c(1:136), c(1:153))
prct1 <- rev(prct)
wm3 <- c()
wm5 <- c()
wm7 <- c()
wm9 <- c()
um1 <- c()
um3 <- c()
um5 <- c()
um7 <- c()
um9 <- c()
wd3 <- c()
wd5 <- c()
wd7 <- c()
wd9 <- c()
ud1 <- c()
ud3 <- c()
ud5 <- c()
ud7 <- c()
ud9 <- c()
wn3 <- c()
wn5 <- c()
wn7 <- c()
wn9 <- c()
un1 <- c()
un3 <- c()
un5 <- c()
un7 <- c()
un9 <- c()
#############I hate loops###
for(k in 1:9 ){
  cat("Weighted minimax 3nn", "Train part",k,"0% Acc ",
      weighted_minimax_3nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1) ,'\n')
  wm3[k] <- weighted_minimax_3nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1)
  cat("Weighted minimax 5nn", "Train part",k,"0% Acc ",
      weighted_minimax_5nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1) ,'\n')
  wm5[k] <- weighted_minimax_5nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1)
  cat("Weighted minimax 7nn", "Train part",k,"0% Acc ",
      weighted_minimax_7nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1) ,'\n')
  wm7[k] <- weighted_minimax_7nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1)
  cat("Weighted minimax 9nn", "Train part",k,"0% Acc ",
      weighted_minimax_9nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1) ,'\n')
  wm9[k] <- weighted_minimax_9nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1)
  cat("unWeighted minimax 1nn", "Train part",k,"0% Acc ",
      unweighted_minimax_1nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1) ,'\n')
  um1[k] <- unweighted_minimax_1nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1)
  cat("unWeighted minimax 3nn", "Train part",k,"0% Acc ",
      unweighted_minimax_3nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1) ,'\n')
  um3[k] <- unweighted_minimax_3nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1)
  cat("unWeighted minimax 5nn", "Train part",k,"0% Acc ",
      unweighted_minimax_5nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1) ,'\n')
  um5[k] <- unweighted_minimax_5nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1)
  cat("unWeighted minimax 7nn", "Train part",k,"0% Acc ",
      unweighted_minimax_7nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1) ,'\n')
  um7[k] <- unweighted_minimax_7nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1)
  cat("unWeighted minimax 9nn", "Train part",k,"0% Acc ",
      unweighted_minimax_9nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1) ,'\n')
  um9[k] <- unweighted_minimax_9nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1)
  cat("Weighted deviation 3nn", "Train part",k,"0% Acc ",
      weighted_deviation_3nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1) ,'\n')
  wd3[k] <- weighted_deviation_3nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1)
  cat("Weighted deviation 5nn", "Train part",k,"0% Acc ",
      weighted_deviation_5nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1) ,'\n')
  wd5[k] <- weighted_deviation_5nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1)
  cat("Weighted deviation 7nn", "Train part",k,"0% Acc ",
      weighted_deviation_7nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1) ,'\n')
  wd7[k] <- weighted_deviation_7nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1)
  cat("Weighted deviation 9nn", "Train part",k,"0% Acc ",
      weighted_deviation_9nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1) ,'\n')
  wd9[k] <- weighted_deviation_9nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1)
  cat("unWeighted deviation 1nn", "Train part",k,"0% Acc ",
      unweighted_deviation_1nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1) ,'\n')
  ud1[k] <- unweighted_deviation_1nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1)
  cat("unWeighted deviation 3nn", "Train part",k,"0% Acc ",
      unweighted_deviation_3nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1) ,'\n')
  ud3[k] <- unweighted_deviation_3nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1)
  cat("unWeighted deviation 5nn", "Train part",k,"0% Acc ",
      unweighted_deviation_5nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1) ,'\n')
  ud5[k] <- unweighted_deviation_5nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1)
  cat("unWeighted deviation 7nn", "Train part",k,"0% Acc ",
      unweighted_deviation_7nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1) ,'\n')
  ud7[k] <- unweighted_deviation_7nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1)
  cat("unWeighted deviation 9nn", "Train part",k,"0% Acc ",
      unweighted_deviation_9nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1) ,'\n')
  ud9[k] <- unweighted_deviation_9nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1)
  cat("Weighted none 3nn", "Train part",k,"0% Acc ",
      weighted_none_3nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1) ,'\n')
  wn3[k] <- weighted_none_3nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1)
  cat("Weighted none 5nn", "Train part",k,"0% Acc ",
      weighted_none_5nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1) ,'\n')
  wn5[k] <- weighted_none_5nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1)
  cat("Weighted none 7nn", "Train part",k,"0% Acc ",
      weighted_none_7nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1) ,'\n')
  wn7[k] <- weighted_none_7nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1)
  cat("Weighted none 9nn", "Train part",k,"0% Acc ",
      weighted_none_9nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1) ,'\n')
  wn9[k] <- weighted_none_9nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1)
  cat("unWeighted none 1nn", "Train part",k,"0% Acc ",
      unweighted_none_1nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1) ,'\n')
  un1[k] <- unweighted_none_1nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1)
  cat("unWeighted none 3nn", "Train part",k,"0% Acc ",
      unweighted_none_3nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1) ,'\n')
  un3[k] <- unweighted_none_3nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1)
  cat("unWeighted none 5nn", "Train part",k,"0% Acc ",
      unweighted_none_5nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1) ,'\n')
  un5[k] <- unweighted_none_5nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1)
  cat("unWeighted none 7nn", "Train part",k,"0% Acc ",
      unweighted_none_7nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1) ,'\n')
  un7[k] <- unweighted_none_7nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1)
  cat("unWeighted none 9nn", "Train part",k,"0% Acc ",
      unweighted_none_9nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1) ,'\n')
  un9[k] <- unweighted_none_9nn(wine, wine1[prct[k][[1]],2:13], wine1[prct1[k][[1]], 2:13], 1)
  
}








unweighted_minimax_5nn(wine, wine1[prct[7][[1]],2:13], wine1[prct1[7][[1]], 2:13], 1)
wm5
wm7
wm9
wm3
um5
um9
acc <- c( wm3,  wm5, wm7, wm9,
          wd3,  wd5,  wd7,  wd9,
          wn3, wn5,  wn7,  wn9)
abs <- c(rep('minimax', 36), rep('deviation', 36), rep('none', 36))
part <- c(rep(seq(10,90,10), 12))
type <- c(rep('w3', 9), rep('w5', 9), rep('w7', 9), rep('w9', 9))
type <- rep(type, 3)
n <- data.frame(part,acc,abs,type)
p <- n %>%
  plot_ly(
    x = ~part, 
    y = ~acc, 
    color = ~abs, 
    frame = ~type, 
    text = ~acc, 
    
    hoverinfo = "text",
    type = 'scatter',
    mode = 'lines'
  ) 
p <- p %>% 
  animation_opts(
    1000, easing = "elastic", redraw = FALSE
  )
p <- p %>% 
  animation_button(
    x = 1, xanchor = "right", y = 0, yanchor = "bottom"
  )
p
api_create(p, filename = "weighted-knn-animation-wine")
View(wine)


part <- c(rep(seq(10,90,10), 15))
acc <- c(um1, um3, um5, um7, um9, ud1, ud3, ud5, ud7, ud9, un1, un3, un5, un7, un9)
abs <- c(rep('minimax', 45), rep('deviation', 45), rep('none', 45))
type <- c(rep('u1', 9), rep('u3', 9), rep('u5', 9), rep('u7', 9), rep('u9', 9))
type <- rep(type, 3)
b <- data.frame(part, acc, abs, type)

p <- b %>%
  plot_ly(
    x = ~part, 
    y = ~acc, 
    color = ~abs, 
    frame = ~type, 
    text = ~abs, 
    
    hoverinfo = "text",
    type = 'scatter',
    mode = 'lines'
  ) 
p <- p %>% 
  animation_opts(
    1000, easing = "elastic", redraw = FALSE
  )
p <- p %>% 
  animation_button(
    x = 1, xanchor = "right", y = 0, yanchor = "bottom"
  )
p
api_create(p, filename = "unweighted-knn-animation-wine")
