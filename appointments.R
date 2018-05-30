install.packages("tm")
library(tm)
library(dplyr)
library(lazyeval)


options(header = F, stringsAsFactors = F, encodeFile = "latin1")
setwd("/home/bogan/Рабочий стол/PREMIER2013")

conn <- file("20130503.txt",open="r")
linn <-readLines(conn)
for (i in 1:length(linn)){
  print(linn[i])
}
close(conn)
linn
library(purrr)

myF <- function(x) {
  ifelse(x %>% unique %>% length > 1, '?', x)
  }
findS <- function(df, a) {
df %>% 
  split(.[a]) %>% 
  map(map, ~ifelse(length(unique(.)) > 1, '?', . ))
}
as.data.frame(findS(menagerie, 'type'))
