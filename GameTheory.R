#############################################

# Player II¡¯s game #

library(lpSolveAPI)
lprec <- make.lp(0, 5)
lp.control(lprec, sense = "minimize") #  can change sense to  "maximize"

set.objfn(lprec, c(0, 0, 0, 0, 1))

add.constraint(lprec, c(-1, 2, -1, -2, 1), ">=", 0)
add.constraint(lprec, c(-3,-2, -5, -4, 1), ">=", 0)
add.constraint(lprec, c(0, -4, 2, 3, 1), ">=", 0)
add.constraint(lprec, c(-2, -3, 2, 4, 1), ">=", 0)
add.constraint(lprec, c(1, 1, 1, 1, 0), "=", 1)

set.bounds(lprec, lower = c(0, 0, 0, 0, -Inf))
RowNames <- c("Row1", "Row2", "Row3", "Row4", "Row5")
ColNames <- c("x1", "x2", "x3", "x4", "v")
dimnames(lprec) <- list(RowNames, ColNames)

solve(lprec) # http://lpsolve.sourceforge.net/5.5/solve.htm
get.objective(lprec)
get.variables(lprec)
get.constraints(lprec)

lprec

#############################################

# Player 2 game #

library(lpSolveAPI)
lprec <- make.lp(0, 5)
lp.control(lprec, sense = "minimize") #  can change sense to  "maximize"

set.objfn(lprec, c(0, 0, 0, 0, 1))

add.constraint(lprec, c(-4, -5,-5,-8, 1), ">=", 0)
add.constraint(lprec, c(-6,-8,-8, -9, 1), ">=", 0)
add.constraint(lprec, c(-5, -7, -5, -4, 1), ">=", 0)
add.constraint(lprec, c(-6, -2, -5, -5, 1), ">=", 0)
add.constraint(lprec, c(1, 1, 1, 1, 0), "=", 1)

set.bounds(lprec, lower = c(0, 0, 0, 0, -Inf))
RowNames <- c("Row1", "Row2", "Row3", "Row4", "Row5")
ColNames <- c("x1", "x2", "x3", "x4", "v")
dimnames(lprec) <- list(RowNames, ColNames)

solve(lprec) # http://lpsolve.sourceforge.net/5.5/solve.htm
get.objective(lprec)
get.variables(lprec)
get.constraints(lprec)

lprec


#############################################

# Player 2's game #

library(lpSolveAPI)
lprec <- make.lp(0, 3)
lp.control(lprec, sense = "minimize") #  can change sense to  "maximize"

set.objfn(lprec, c(0, 0, 1))

add.constraint(lprec, c(-3, -8, 1), ">=", 0)
add.constraint(lprec, c(-7, -1, 1), ">=", 0)
add.constraint(lprec, c(1, 1, 0), "=", 1)

set.bounds(lprec, lower = c(0, 0,-Inf))

solve(lprec) # http://lpsolve.sourceforge.net/5.5/solve.htm
get.objective(lprec)
get.variables(lprec)
get.constraints(lprec)

lprec



#############################################

# Hiding and searching game
# Player 1's game #

library(lpSolveAPI)
lprec <- make.lp(0, 5)
lp.control(lprec, sense = "maximize") #  can change sense to  "maximize"

set.objfn(lprec, c(0, 0, 0, 0, 1))

add.constraint(lprec, c(-1, 0, 1, 0, 1), "<=", 0)
add.constraint(lprec, c(0,-1, 0, 1, 1), "<=", 0)
add.constraint(lprec, c(1, 0, -1, 0, 1), "<=", 0)
add.constraint(lprec, c(0, 1, 0, -1, 1), "<=", 0)
add.constraint(lprec, c(1, 1, 1, 1, 0), "=", 1)

set.bounds(lprec, lower = c(0, 0, 0, 0, -Inf))
RowNames <- c("Row1", "Row2", "Row3", "Row4", "Row5")
ColNames <- c("x1", "x2", "x3", "x4", "v")
dimnames(lprec) <- list(RowNames, ColNames)

solve(lprec) # http://lpsolve.sourceforge.net/5.5/solve.htm
get.objective(lprec)
get.variables(lprec)
get.constraints(lprec)

lprec

#############################################

# Player 1's game #

library(lpSolveAPI)
lprec <- make.lp(0, 3)
lp.control(lprec, sense = "maximize") #  can change sense to  "maximize"

set.objfn(lprec, c(0, 0, 1))

add.constraint(lprec, c(-5, -6, 1), "<=", 0)
add.constraint(lprec, c(-7, -2, 1), "<=", 0)
add.constraint(lprec, c(1, 1, 0), "=", 1)

set.bounds(lprec, lower = c(0, 0,-Inf))

solve(lprec) # http://lpsolve.sourceforge.net/5.5/solve.htm
get.objective(lprec)
get.variables(lprec)
get.constraints(lprec)

lprec



#############################################

# Player 1's game #
# coins show game 2 players

library(lpSolveAPI)
lprec <- make.lp(0, 3)
lp.control(lprec, sense = "maximize") #  can change sense to  "maximize"

set.objfn(lprec, c(0, 0, 1))

add.constraint(lprec, c(-200, 105, 1), "<=", 0)
add.constraint(lprec, c(105, -10, 1), "<=", 0)
add.constraint(lprec, c(1, 1, 0), "=", 1)

set.bounds(lprec, lower = c(0, 0,-Inf))

solve(lprec) # http://lpsolve.sourceforge.net/5.5/solve.htm
get.objective(lprec)
get.variables(lprec)
get.constraints(lprec)

lprec

# Player 2's game #

library(lpSolveAPI)
lprec <- make.lp(0, 3)
lp.control(lprec, sense = "minimize") #  can change sense to  "maximize"

set.objfn(lprec, c(0, 0, 1))

add.constraint(lprec, c(-200, 105, 1), ">=", 0)
add.constraint(lprec, c(105, -10, 1), ">=", 0)
add.constraint(lprec, c(1, 1, 0), "=", 1)

set.bounds(lprec, lower = c(0, 0,-Inf))

solve(lprec) # http://lpsolve.sourceforge.net/5.5/solve.htm
get.objective(lprec)
get.variables(lprec)
get.constraints(lprec)

lprec
