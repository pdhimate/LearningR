### Linear programming

## Maximize z = 3s + 2t aka objective function
# constraint 2s + t <= 100
# constraint s + t <= 80
# constraint s <= 80
# constraint s,t >=0

# plot graph for constraints on an online plotting tool

# point 40,20 satisfies the all teh constraints
# hence max z = 3(40) + 2(20) = 160

## Minimize z = 1470-12x-19y aka objective function
# constraint 3x+6y <= 540
# constraint 5x+5y <=450
# constraint 4x+8y <=480
# constraint x,y >=0

# plot them and we see that 60, 30 point satisfies all the conditions.
# hence min z = 1470-12(60)-19(30) = 180

## minmize z = 10f1 + 3f2
# constraints 3f1 + 2f2 >= 60
# constraints 7f1 + 2f2 >= 84
# constraints 3f1 + 6f2 >= 72

# after plotting we check for interesection of all lines, 
# one of those point satisfies all the constraints
# if multiple points satisfy the constrainsts, 
# for each, substitue in our object function and compare values
# the one that gives minimum value is our point.
# plot has points 18,3 and 6,21 that satisfy constraints, out of which 6,21 give min value.


# SIT718  - Two Variable LP programming 

# The following script provide solutions for two examples used in week 7

# 7.5 - Toy Company Problem
library(lpSolveAPI)

toyCompanyModel <- make.lp(0, 2)
lp.control(toyCompanyModel, sense= "maximize")

set.objfn(toyCompanyModel, c(3,2))

add.constraint(toyCompanyModel, c(2,1), "<=", 100)
add.constraint(toyCompanyModel, c(1,1), "<=", 80) 
set.bounds(toyCompanyModel, lower = c(0,0), columns = c(1, 2))
set.bounds(toyCompanyModel, upper = 40, columns = 1)

RowNames <- c("Constraint 1", "Constraint 2")

ColNames <- c("Soldiers", "Trains")

dimnames(toyCompanyModel) <- list(RowNames, ColNames)

solve(toyCompanyModel) # http://lpsolve.sourceforge.net/5.5/solve.htm

get.objective(toyCompanyModel)

get.variables(toyCompanyModel)

get.constraints(toyCompanyModel) 

toyCompanyModel

##############################

# 7.6 - Assembly Line Problem

library(lpSolveAPI)

assemblyModel <- make.lp(0, 2) # two variables

lp.control(assemblyModel, sense= "maximize")

set.objfn(assemblyModel, c(12,19))

add.constraint(assemblyModel, c(3,6), "<=", 540)

add.constraint(assemblyModel, c(5,5), "<=", 450) 

add.constraint(assemblyModel, c(4,8), "<=", 480) 

set.bounds(assemblyModel, lower = c(0,0), columns = c(1, 2))

# set.bounds(assemblyModel, upper = 40, columns = 1)

RowNames <- c("Constraint 1", "Constraint 2")

ColNames <- c("Smart-1", "Smart-2")

dimnames(assemblyModel) <- list(RowNames, ColNames)

solve(assemblyModel) # http://lpsolve.sourceforge.net/5.5/solve.htm

get.objective(assemblyModel)

get.variables(assemblyModel)

get.constraints(assemblyModel) 

assemblyModel

##############################

