cost <- matrix(1:6 , 3, 2, T)
tryCatch(indM <- hungariansafe_cc(cost), error = function(e) e)

cost <- matrix(c(1, 2, 2, 4), 2, 2, T)
cost
indM <- hungarian_cc(cost)
indM
min.cost <- sum(indM * cost)
min.cost

revenues <- matrix(seq(1, 4)) %*% seq(1, 4)
revenues
cost <- 100 - revenues
cost
indM <- hungarian_cc(cost)
indM
max.revenue <- sum(indM * revenues)
max.revenue

cost <- matrix(rnorm(100), 10, 10)
cost
indM <- hungarian_cc(cost)
indM
min.cost <- sum(indM * cost)
min.cost




a <- c(10000, 0, 0, 0, 0, 0, 0, 0, 0)
cost <- matrix(a, 3, 3, T)
cost
b <- hungariansafe_cc(cost)
sum(b * cost)
l <- hungariansafe_cc(max(cost) - cost)
l
sum(l * cost)
