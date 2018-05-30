#
f.obj <- c(6, 2, 2.5, 4)
f.con <- matrix (c(5,1,0,2,4,2,2,1,1,0,2,1), ncol=4, byrow=TRUE)
f.obj
f.con
f.dir <- c("<=", "<=", "<=")
f.rhs <- c(1000, 600, 150)
#
# Now run.
#
lp ("max", f.obj, f.con, f.dir, f.rhs)
## Not run: Success: the objective function is 40.5
lp ("max", f.obj, f.con, f.dir, f.rhs)$solution
