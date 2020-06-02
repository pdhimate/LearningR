### supply demand - one supply not connected to 1 demand ####
### oil refinery ######
library(lpSolveAPI)

# model and objective
lpModel <- make.lp(6, 9) # 6 constraints and 9 decision variables
lp.control(lpModel, sense = "minimize") 

# objective functions
set.objfn(lpModel, c(1.2,1.8,99999,3,1,.8,2,2.5,1.2)) # 9 decisions variables coeffients

# constraints LHS equations
set.row(lpModel, 1, rep(1,3), indices = c(1:3))
set.row(lpModel, 2, rep(1,3), indices = c(4:6))
set.row(lpModel, 3, rep(1,3), indices = c(7:9))
set.row(lpModel, 4, rep(1,3), indices = c(1,4,7))
set.row(lpModel, 5, rep(1,3), indices = c(2,5,8))
set.row(lpModel, 6, rep(1,3), indices = c(3,6,9))

# constraints RHS values
set.rhs(lpModel, c(6,5,8,4,8,7))
set.constr.type(lpModel, c("<=","<=","<=",">=",">=",">="))

# bounds
set.type(lpModel, c(1:9), "binary")
set.bounds(lpModel, lower = rep(0, 9), upper = rep(Inf, 9))

write.lp(lpModel, filename = "test.lp")  #Use write.lp to print out larger LPs.
lpModel
solve(lpModel)

get.objective(lpModel)
get.variables(lpModel)
get.constraints(lpModel)


### supply demand - multi period production ####
library(lpSolveAPI)

# model and objective
lpModel <- make.lp(11, 7) # 11 constraints and 7 decision variables
lp.control(lpModel, sense = "minimize") 

# objective functions
set.objfn(lpModel, c(20,22,24,26,3.5,3.5,3.5)) # 7 decisions variables coeffients

# constraints LHS equations
set.row(lpModel, 1, c(1,-1), indices = c(1,5))
set.row(lpModel, 2, c(1,1,-1), indices = c(2,5,6))
set.row(lpModel, 3, c(1,1,-1), indices = c(3,6,7))
set.row(lpModel, 4, c(1,1), indices = c(4,7))
set.row(lpModel, 5, c(1), indices = c(1))
set.row(lpModel, 6, c(1), indices = c(2))
set.row(lpModel, 7, c(1), indices = c(3))
set.row(lpModel, 8, c(1), indices = c(4))
set.row(lpModel, 9, c(1), indices = c(5))
set.row(lpModel, 10, c(1), indices = c(6))
set.row(lpModel, 11, c(1), indices = c(7))

# constraints RHS values
set.rhs(lpModel, c(300,400,450,250,400,400,400,400,100,100,100))
set.constr.type(lpModel, c("=","=","=","=","<=","<=","<=","<=","<=","<=","<="))

# bounds
set.type(lpModel, c(1:7), "real")
set.bounds(lpModel, lower = rep(0, 7), upper = rep(Inf, 7))

write.lp(lpModel, filename = "test.lp")  #Use write.lp to print out larger LPs.
lpModel
solve(lpModel)

get.objective(lpModel)
get.variables(lpModel)
get.constraints(lpModel)


### single period prod, 3 products same raw ####
library(lpSolveAPI)

# model and objective
lpModel <- make.lp(3,3) # 3 constraints and 3 decision variables
lp.control(lpModel, sense = "maximize") 

# objective functions
set.objfn(lpModel, c(10,20,40)) # 3 decisions variables coeffients

# constraints LHS equations
set.row(lpModel, 1, c(-3,7,7), indices = c(1,2,3))
set.row(lpModel, 2, c(1), indices = c(2))
set.row(lpModel, 3, c(2,4,6), indices = c(1,2,3))

# constraints RHS values
set.rhs(lpModel, c(0,200,2000))
set.constr.type(lpModel, c("<=",">=","<="))

# bounds
set.type(lpModel, c(1:3), "real")
set.bounds(lpModel, lower = rep(0, 3), upper = rep(Inf, 3))

write.lp(lpModel, filename = "test.lp")  #Use write.lp to print out larger LPs.
lpModel
solve(lpModel)

get.objective(lpModel)
get.variables(lpModel)
get.constraints(lpModel)

###  swimming relay - 4 swimmers 4 strokes, each must swim 1 stroke ###
### binary model since OR ing is done in constraints and constraints have RHS = 1

library(lpSolveAPI)

# model and objective
lpModel <- make.lp(8, 16) # 12 constraints and 16 decision variables
lp.control(lpModel, sense = "minimize") 

# objective functions
set.objfn(lpModel, c(54,54,51,53,51,57,52,52,50,53,54,56,56,54,55,53)) # 16 decisions variables coeffients

# constraints LHS equations
set.row(lpModel, 1, rep(1,4), indices = c(1:4))
set.row(lpModel, 2, rep(1,4), indices = c(5:8))
set.row(lpModel, 3, rep(1,4), indices = c(9:12))
set.row(lpModel, 4, rep(1,4), indices = c(13:16))
set.row(lpModel, 5, rep(1,4), indices = c(1,5,9,13))
set.row(lpModel, 6, rep(1,4), indices = c(2,6,10,14))
set.row(lpModel, 7, rep(1,4), indices = c(3,7,11,15))
set.row(lpModel, 8, rep(1,4), indices = c(4,8,12,16))

# constraints RHS values
set.rhs(lpModel, rep(1,8))
set.constr.type(lpModel, rep("=", 8))

# bounds
set.type(lpModel, c(1:16), "binary")
set.bounds(lpModel, lower = rep(0, 16), upper = rep(Inf, 16))

write.lp(lpModel, filename = "test.lp")  #Use write.lp to print out larger LPs.
lpModel
solve(lpModel)

get.objective(lpModel)
get.variables(lpModel)
get.constraints(lpModel)


###  house hold chores - 4 kids 4 chores, each must do 1 chore ###
### binary model since OR ing is done in constraints and constraints have RHS = 1

library(lpSolveAPI)

# model and objective
lpModel <- make.lp(8, 16) # 12 constraints and 16 decision variables
lp.control(lpModel, sense = "minimize") 

# objective functions
set.objfn(lpModel, c(5,10,9,14,9,15,10,10,10,12,8,12,16,11,13,15)) # 16 decisions variables coeffients

# constraints LHS equations
set.row(lpModel, 1, rep(1,4), indices = c(1:4))
set.row(lpModel, 2, rep(1,4), indices = c(5:8))
set.row(lpModel, 3, rep(1,4), indices = c(9:12))
set.row(lpModel, 4, rep(1,4), indices = c(13:16))
set.row(lpModel, 5, rep(1,4), indices = c(1,5,9,13))
set.row(lpModel, 6, rep(1,4), indices = c(2,6,10,14))
set.row(lpModel, 7, rep(1,4), indices = c(3,7,11,15))
set.row(lpModel, 8, rep(1,4), indices = c(4,8,12,16))

# constraints RHS values
set.rhs(lpModel, rep(1,8))
set.constr.type(lpModel, rep("=", 8))

# bounds
set.type(lpModel, c(1:16), "binary")
set.bounds(lpModel, lower = rep(0, 16), upper = rep(Inf, 16))

write.lp(lpModel, filename = "test.lp")  #Use write.lp to print out larger LPs.
lpModel
solve(lpModel)

get.objective(lpModel)
get.variables(lpModel)
get.constraints(lpModel)

### supply demand - one supply not connected to 1 demand ####
### cars. Excees demand and hence 1 dummy source with penalty for no delivery ######
library(lpSolveAPI)

# model and objective
lpModel <- make.lp(6, 16) # 6 constraints and 16 decision variables
lp.control(lpModel, sense = "minimize") 

# objective functions
set.objfn(lpModel, c(100,99999,200,220,120,150,0,0,350,300,350,300,350,300,350,300)) # 16 decisions variables coeffients

# constraints LHS equations
set.row(lpModel, 1, c(1,1), indices = c(1,2))
set.row(lpModel, 2, c(1,1), indices = c(3,4))
set.row(lpModel, 3, c(1,1), indices = c(5,6))
set.row(lpModel, 4, c(1,1), indices = c(7,8))
set.row(lpModel, 5, c(1,1,1,1), indices = c(1,3,5,7))
set.row(lpModel, 6, c(1,1,1,1), indices = c(2,4,6,8))

# constraints RHS values
set.rhs(lpModel, c(1000,1300,1200,200,2300,1400))
set.constr.type(lpModel, c("<=","<=","<=","=","=","="))

# bounds
set.type(lpModel, c(1:16), "real")
set.bounds(lpModel, lower = rep(0, 16), upper = rep(Inf, 16))

write.lp(lpModel, filename = "test.lp")  #Use write.lp to print out larger LPs.
lpModel
solve(lpModel)

get.objective(lpModel)
get.variables(lpModel)
get.constraints(lpModel)


### 3 constituents of 2 beverages ####
library(lpSolveAPI)

# model and objective
lpModel <- make.lp(4,2) # 4 constraints and 2 decision variables
lp.control(lpModel, sense = "minimize") 

# objective functions
set.objfn(lpModel, c(5, 6)) # 2 decisions variables coeffients

# constraints LHS equations
set.row(lpModel, 1, c(.06,.04), indices = c(1,2))
set.row(lpModel, 2, c(.04,.06), indices = c(1,2))
set.row(lpModel, 3, c(.03,.08), indices = c(1,2))
set.row(lpModel, 4, c(1,1), indices = c(1,2))

# constraints RHS values
set.rhs(lpModel, c(4.5,5,6,100))
set.constr.type(lpModel, c(">=",">=","<=", ">="))

# bounds
set.type(lpModel, c(1:2), "real")
set.bounds(lpModel, lower = rep(0, 2), upper = rep(Inf, 2))

write.lp(lpModel, filename = "test.lp")  #Use write.lp to print out larger LPs.
lpModel
solve(lpModel)

get.objective(lpModel)
get.variables(lpModel)
get.constraints(lpModel)
