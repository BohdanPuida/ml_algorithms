h <- 10^9

testDF <- data.frame(a = 1:h, b = h:1) 
testDF1 <- data.frame(a = 1:h, b = h:1) 

system.time( for(row in 1:nrow(testDF)) { testDF[row, 3] <- testDF[row, 1] + testDF[row, 2] } )

system.time(testDF1 %>% mutate(c = a + b) -> testDF1)

hungariansafe_cc(matrix(seq(-5, 5 - 0.001, 0.001), 100, 100, T))
length(seq(-5, 5 - 0.001, 0.001))

lp.transport
lp

costs <- matrix (10000, 8, 5)
costs[4,1] <- costs[-4,5] <- 0
costs[1,2] <- costs[2,3] <- costs[3,4] <- 7
costs[1,3] <- costs[2,4] <- 7.7
costs[5,1] <- costs[7,3] <- 8
costs[1,4] <- 8.4
costs[6,2] <- 9
costs[8,4] <- 10
costs[4,2:4] <- c(.7, 1.4, 2.1)

row.signs <- rep ("<", 8)
row.rhs <- c(200, 300, 350, 200, 100, 50, 100, 150)
col.signs <- rep (">", 5)
col.rhs <- c(250, 100, 400, 500, 200)

lp.transport (costs, "min", row.signs, row.rhs, col.signs, col.rhs)
lp.transport (costs, "min", row.signs, row.rhs, col.signs, col.rhs)$solution



mincost <- function(ex_matrix){
  
  if(sum(is.na(ex_matrix)) > 0)
    stop("Your matrix has NA values")
  
  Demand = as.vector(ex_matrix[nrow(ex_matrix),-ncol(ex_matrix)])
  Supply = as.vector(ex_matrix[-nrow(ex_matrix),ncol(ex_matrix)])
  High_Values = max(ex_matrix) + 999999999
  Alloc_Matrix = ex_matrix[-nrow(ex_matrix),-ncol(ex_matrix)]
  ex_matrix = Alloc_Matrix
  Alloc_Matrix[,] = 0
  Total_Cost = 0
  Total_alloc = 0
  
  while(sum(Supply) != 0 & sum(Demand) != 0)
  {
    tc = which.min(apply(ex_matrix, MARGIN = 2, min))  
    tr = which.min(apply(ex_matrix, MARGIN=1, min)) 
    
    min_curr = min(Demand[tc], Supply[tr])
    
    Demand[tc]=Demand[tc] - min_curr
    Supply[tr]=Supply[tr] - min_curr
    Alloc_Matrix[tr,tc]=min_curr
    Total_Cost=Total_Cost+(min_curr*ex_matrix[tr,tc])
    
    if(Demand[tc]==0)
    {
      ex_matrix[,tc]=rep(High_Values,nrow(ex_matrix))
    }else if(Demand[tc]==Supply[tr])
    {
      ex_matrix[tr,]=rep(High_Values,ncol(ex_matrix))
      ex_matrix[,tc]=rep(High_Values,nrow(ex_matrix))
    }else{
      ex_matrix[tr,]=rep(High_Values,ncol(ex_matrix))
    }
    Total_alloc=Total_alloc+1
  }
  
  output=list()
  output$Alloc_Matrix=Alloc_Matrix
  output$Total_Cost=Total_Cost

  if(sum(Demand) != 0)
    output$Dummy_demand = sum(Demand)
  else if(sum(Supply) != 0)
    output$Dummy_supply = sum(Supply)
  
  if(Total_alloc < (nrow(Alloc_Matrix) + ncol(Alloc_Matrix)-1))
    warning("Degenracy in Transporation Problem Occurred")
  return(output)
}



nwc <- function(ex_matrix){
  
  if(sum(is.na(ex_matrix))>0)
    stop("Your matrix has NA values")
  
  Alloc_Matrix=ex_matrix[-nrow(ex_matrix),-ncol(ex_matrix)]
  Alloc_Matrix[,]=0
  tr=1
  tc=1
  Total_Cost=0
  Total_alloc=0
  colnames(ex_matrix)[ncol(ex_matrix)]="Supply"
  while(sum(ex_matrix[nrow(ex_matrix),]) != 0 & sum(ex_matrix[,ncol(ex_matrix)]) != 0)
  {
    min_curr=min(ex_matrix[tr,ncol(ex_matrix)],ex_matrix[nrow(ex_matrix),tc])
    ex_matrix[tr,ncol(ex_matrix)]=ex_matrix[tr,ncol(ex_matrix)] - min_curr
    ex_matrix[nrow(ex_matrix),tc]=ex_matrix[nrow(ex_matrix),tc] - min_curr
    Alloc_Matrix[tr,tc]=min_curr
    Total_Cost=Total_Cost+(min_curr*ex_matrix[tr,tc])
    if(ex_matrix[nrow(ex_matrix),tc]==0)
    {
      tc=tc+1
    }else if(ex_matrix[tr,ncol(ex_matrix)]==ex_matrix[nrow(ex_matrix),tc])
    {
      tr=tr+1
      tc=tc+1
    }else{
      tr=tr+1
    }
    ex_matrix[nrow(ex_matrix),ncol(ex_matrix)]=sum(ex_matrix$Supply[-nrow(ex_matrix)])
    Total_alloc=Total_alloc+1
  }
  
  output=list()
  output$Alloc_Matrix=Alloc_Matrix
  output$Total_Cost=Total_Cost
  
  #If Supply and Demand are not same
  if(sum(ex_matrix[nrow(ex_matrix),]) != 0)
    output$Dummy_demand=sum(ex_matrix[nrow(ex_matrix),])
  else if(sum(ex_matrix[,ncol(ex_matrix)]) != 0)
    output$Dummy_supply=sum(ex_matrix[,ncol(ex_matrix)])
  
  if(Total_alloc < (nrow(Alloc_Matrix) + ncol(Alloc_Matrix)-1))
    warning("Degenracy in Transporation Problem Occurred")
  
  return(output)
}



ex_matrix=data.frame(M1=c(13,10,25,17,210),M2=c(25,19,10,24,240),
                     M3=c(8,18,15,18,110),M4=c(13,5,14,13,80),M5=c(20,12,18,19,170),
                     Supply=c(430,150,100,130,810),
                     row.names = c("W1","W2","W3","W4","Demand"))
ex_matrix
mincost(ex_matrix)
nwc(ex_matrix)
m <- a$Alloc_Matrix
m
library(lpSolve)
costs <- ex_matrix[1:4, 1:5]
costs <- as.matrix(costs)

row.signs <- rep ("<", 4)
row.rhs <- c(430,150,100,130)
col.signs <- rep (">", 5)
col.rhs <- c(210, 240, 110, 80, 170)

lp.transport (costs, "min", row.signs, row.rhs, col.signs, col.rhs)$objval

n <- lp.transport (costs, "min", row.signs, row.rhs, col.signs, col.rhs)$solution
n <- as.data.frame(n)
n

help(lp.transport)
simplex_max

supply <- ex_matrix$Supply
n <- ex_matrix[nrow(ex_matrix),]
supply
demands <- n
nrow(m)+ncol(m) - 1 < length(which(m == 0))
which(m == 0)
m
U <- rep(0, nrow(m))
V <- rep(0, ncol(m))
U
V
C <- ex_matrix[1:nrow(m), 1:ncol(m)]
C
m
V[]
revenues <- matrix(seq(1, 4)) %*% seq(1, 4)
revenues
a <- matrix(c(1:20), 4, 5, T)
a
length(a)

assign.costs <- matrix(c(2, 7, 7, 2, 7, 7, 3, 2, 7, 2, 8, 10, 1, 9, 8, 2), 4, 4, T)
lp.assign(assign.costs)$objval
lp.assign(assign.costs)$solution
