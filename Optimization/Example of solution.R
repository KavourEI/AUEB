library(lpSolve)
library(lpSolveAPI)

f.obj <- c(40,30,-105/100)
f.con <- matrix(c(0.5,0.3,0,0.3,0.4,0,0.3,0.1,0,100,80,-1,160,130,-1,0,0,1,1,0,0,0,1,0,0,0,1), nrow = 9, byrow = TRUE)
f.dir <- c("<=","<=","<=","<=","<=","<=",">=",">=",">=")
f.rhs <- c(1800,1000,700,300000,300000,200000,0,0,0)

result <- lp("max",f.obj,f.con,f.dir,f.rhs, all.int =TRUE)

result$solution
result$objval



f.obj <- c(40,30)
f.con <- matrix(c(0.5,0.3,0.3,0.4,0.3,0.1,100,80,160,130,1,0,0,1), nrow = 7, byrow = TRUE)
f.dir <- c("<=","<=","<=","<=","<=",">=",">=")
f.rhs <- c(1800,1000,700,300000,300000,0,0)

result <- lp("max",f.obj,f.con,f.dir,f.rhs, all.int =TRUE)

result$solution
result$objval
