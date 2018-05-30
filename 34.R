help(wellPanel)
help("switch")
library(Rcpp)
Rcpp::cppFunction()
install.packages("DT")
library(DT)
install.packages('rhandsontable')
library(rhandsontable)
library(ggplot2, dplyr)
DF = data.frame(integer = 1:10,
                numeric = rnorm(10),
                logical = rep(TRUE, 10), 
                character = LETTERS[1:10],
                factor = factor(letters[1:10], levels = letters[10:1], 
                                ordered = TRUE),
                factor_allow = factor(letters[1:10], levels = letters[10:1], 
                                      ordered = TRUE),
                date = seq(from = Sys.Date(), by = "days", length.out = 10),
                stringsAsFactors = FALSE)

rhandsontable(DF, width = 600, height = 300) %>%
  hot_col("factor_allow", allowInvalid = F)

as.matrix(DF)
hungariansafe_cc(DF)

DF <- as.matrix(DF)

DF <-  data.frame(programmer = rep(0, 4),
                  designer = rep(0, 4),
                  scientist = rep(0, 4),
                  researcher = rep(0, 4) )
rownames(DF) <- c('Jim', 'John', 'Bill', 'Tom')
