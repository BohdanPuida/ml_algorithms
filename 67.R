help(lp)
f.obj <- c(1, 9, 1)
f.con <- matrix (c(1, 2, 3, 3, 2, 2), nrow=2, byrow=TRUE)
f.dir <- c("<=", "<=")
f.rhs <- c(9, 15)

a <- lp ("max", f.obj, f.con, f.dir, f.rhs)
a <- a$objval
colnames(a) <- c('Gain')
rownames(a) <- NULL
a
a <- as.matrix(a)
a <- lp ("max", f.obj, f.con, f.dir, f.rhs)$solution
a
names(a) <- c('Bullet1', 'Bullet2', 'Bullet3')
a
a <- c(0)
names(a) <- c('Gain')
a
help("matrixInput")
