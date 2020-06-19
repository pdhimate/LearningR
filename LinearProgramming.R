##########################################
# Linear programming practise
##########################################

##### Toy Company ########

library(lpSolveAPI)

# model and objective
lpModel <- make.lp(3, 2) # 3 constraints and 2 decision variables
lp.control(lpModel, sense = "maximize") # max the profit

# objective functions
set.objfn(lpModel, c(3, 2)) # 2 decisions variables coeffients

# constraints LHS equations
set.row(lpModel, 1, c(1), indices = c(1))
set.row(lpModel, 2, c(2, 1), indices = c(1, 2))
set.row(lpModel, 3, c(1, 1), indices = c(1, 2))

# constraints RHS values
set.rhs(lpModel, c(40, 100, 80))
set.constr.type(lpModel, c(">=",	"<=",	"<="))

# bounds
set.type(lpModel, c(1:2), "real")
set.bounds(lpModel, lower = rep(0, 2), upper = rep(Inf, 2))

# write.lp(multiPeriodModel, filename="test.lp")  #Use write.lp to print out larger LPs.
lpModel
solve(lpModel)

objvalue <- get.objective(lpModel)
objvalue
solution <- get.variables(lpModel)
solution

##### Toy Company - assembly line workstations ########
## Toy company assembly line 3 workstations, trains and soilders
library(lpSolveAPI)

# model and objective
lpModel <- make.lp(3, 2) # 3 constraints and 2 decision variables
lp.control(lpModel, sense = "minimize") # min idle time of workstations

# objective functions
set.objfn(lpModel, c(-12, -19)) # 2 decisions variables coeffients

# constraints LHS equations
set.row(lpModel, 1, c(3,6), indices = c(1,2))
set.row(lpModel, 2, c(5,5), indices = c(1,2))
set.row(lpModel, 3, c(4,8), indices = c(1,2))

# constraints RHS values
set.rhs(lpModel, c(540, 450, 480))
set.constr.type(lpModel, c("<=",	"<=",	"<="))

# bounds
set.type(lpModel, c(1:2), "real")
set.bounds(lpModel, lower = rep(0, 2), upper = rep(Inf, 2))

# write.lp(multiPeriodModel, filename="test.lp")  #Use write.lp to print out larger LPs.
lpModel
solve(lpModel)
get.objective(lpModel)
get.variables(lpModel)

##### Farmer Dave - decision ########

library(lpSolveAPI)

# model and objective
lpModel <- make.lp(3, 2) # 3 constraints and 2 decision variables
lp.control(lpModel, sense = "minimize")

# objective functions
set.objfn(lpModel, c(10, 3)) # 2 decisions variables coeffients

# constraints LHS equations
set.row(lpModel, 1, c(3, 2), indices = c(1, 2))
set.row(lpModel, 2, c(7, 2), indices = c(1, 2))
set.row(lpModel, 3, c(3, 6), indices = c(1, 2))

# constraints RHS values
set.rhs(lpModel, c(60, 84, 72))
set.constr.type(lpModel, c(">=", ">=",	">="))

# bounds
set.type(lpModel, c(1:2), "real")
set.bounds(lpModel, lower = rep(0, 2), upper = rep(Inf, 2))

# write.lp(multiPeriodModel, filename="test.lp")  #Use write.lp to print out larger LPs.
lpModel
solve(lpModel)

get.objective(lpModel)
get.variables(lpModel)
get.constraints(lpModel)

##### Farmer Vicky -magic feed cost ########

library(lpSolveAPI)

# model and objective
lpModel <- make.lp(3, 2) # 3 constraints and 2 decision variables
lp.control(lpModel, sense = "minimize")

# objective functions
set.objfn(lpModel, c(0.3, 0.9)) # 2 decisions variables coeffients

# constraints LHS equations
set.row(lpModel, 1, c(1, 1), indices = c(1, 2))
set.row(lpModel, 2, c(0.21, -0.3), indices = c(1, 2))
set.row(lpModel, 3, c(0.03, -0.01), indices = c(1, 2))

# constraints RHS values
set.rhs(lpModel, c(800, 0, 0))
set.constr.type(lpModel, c(">=", "<=",	">="))

# bounds
set.type(lpModel, c(1:2), "real")
set.bounds(lpModel, lower = rep(0, 2), upper = rep(Inf, 2))

# write.lp(multiPeriodModel, filename="test.lp")  #Use write.lp to print out larger LPs.
lpModel
solve(lpModel)

get.objective(lpModel)
get.variables(lpModel)
get.constraints(lpModel)


##### Bank loan- multiple variables (more than 2) ########

library(lpSolveAPI)

# model and objective
lpModel <- make.lp(4, 5) # 4 constraints and 5 decision variables
lp.control(lpModel, sense = "maximize")

# objective functions
set.objfn(lpModel, c(0.06497, 0.011468, 0.014814, 0.06875, 0.0505)) # 5 decisions variables coeffients

# constraints LHS equations
set.row(lpModel, 1, rep(1, 5), indices = c(1:5))
set.row(lpModel, 2, c(-0.04, -0.04,-0.04, 0.6, 0.6), indices = c(1:5))
set.row(lpModel, 3, c(-0.5, -0.5, 0.5), indices = c(1, 2, 3))
set.row(lpModel, 4, c(0.06, 0.03,-0.01,-0.01, 0.005), indices = c(1:5))

# constraints RHS values
set.rhs(lpModel, c(12, 0, 0, 0))
set.constr.type(lpModel, c("<=", ">=",	">=", "<="))

# bounds
set.type(lpModel, c(1:5), "real")
set.bounds(lpModel, lower = rep(0, 5), upper = rep(Inf, 5))

# write.lp(multiPeriodModel, filename="test.lp")  #Use write.lp to print out larger LPs.
lpModel
solve(lpModel)

get.objective(lpModel)
get.variables(lpModel)
get.constraints(lpModel)



##### Clothing production- multiple variables (more than 2) - single period production ########

library(lpSolveAPI)

# model and objective
clothModel <- make.lp(8, 8) # 8 constraints and 8 decision variables
lp.control(clothModel, sense = "maximize")

# objective functions
set.objfn(clothModel, c(30, 40, 20, 10,-15,-20, -10, -8)) # 8 decisions variables coeffients

# constraints LHS equations
set.row(clothModel, 1, c(0.3, 0.3,	0.25,	0.15), indices = c(1:4))
set.row(clothModel, 2, c(0.25,	0.35,	0.3,	0.1), indices = c(1:4))
set.row(clothModel, 3, c(0.45,	0.5,	0.4,	0.22), indices = c(1, 2, 3, 4))
set.row(clothModel, 4, c(0.15,	0.15,	0.1,	0.05), indices = c(1:4))
set.row(clothModel, 5, c(1, 1), indices = c(1, 5))
set.row(clothModel, 6, c(1, 1), indices = c(2, 6))
set.row(clothModel, 7, c(1, 1), indices = c(3, 7))
set.row(clothModel, 8, c(1, 1), indices = c(4, 8))

# constraints RHS values
set.rhs(clothModel, c(1000, 1000, 1000, 1000, 800, 750, 600, 500))
set.constr.type(clothModel, c("<=", "<=", "<=", "<=" , "=",	"=", "=",	"="))

# bounds
set.type(clothModel, c(1:8), "real")
set.bounds(clothModel, lower = rep(0, 8), upper = rep(Inf, 8))

# write.lp(clothModel, filename="test.lp")  #Use write.lp to print out larger LPs.
clothModel
solve(clothModel)

get.objective(clothModel)
get.variables(clothModel)
get.constraints(clothModel)


##### multi period Production - table production over 6 months########

library(lpSolveAPI)

# model and objective
lpModel <- make.lp(6, 12) # 6 constraints and 12 decision variables
lp.control(lpModel, sense = "minimize") # min prod costs

# objective functions
set.objfn(lpModel, c(50, 45, 55, 48, 52, 50, 8, 8, 8, 8, 8, 8)) # 12 decisions variables coeffients

# constraints LHS equations
set.row(lpModel, 1, c(1, -1), indices = c(1, 7))
set.row(lpModel, 2, c(1, 1,-1), indices = c(2, 7, 8))
set.row(lpModel, 3, c(1, 1, -1), indices = c(3, 8, 9))
set.row(lpModel, 4, c(1, 1, -1), indices = c(4, 9, 10))
set.row(lpModel, 5, c(1, 1, -1), indices = c(5, 10, 11))
set.row(lpModel, 6, c(1, 1), indices = c(6, 11))

# constraints RHS values
set.rhs(lpModel, c(100, 250, 190, 140, 220, 110))
set.constr.type(lpModel, c("=", "=", "=", "=" , "=",	"="))

# bounds
set.type(lpModel, c(1:12), "real")
set.bounds(lpModel, lower = rep(0, 12), upper = rep(Inf, 12))

write.lp(lpModel, filename = "test.lp")  #Use write.lp to print out larger LPs.
lpModel
solve(lpModel)

get.objective(lpModel)
get.variables(lpModel)
get.constraints(lpModel)

##### excess supply problem - elec production 3 plants 4 cities + 1 dummy city (for excess supply) ########

library(lpSolveAPI)

# model and objective
lpModel <- make.lp(8, 15) # 8 constraints and 15 decision variables
lp.control(lpModel, sense = "minimize") # min supply costs

# objective functions
set.objfn(lpModel, c(8,	6, 10,	9	, 0,	9,	12,	13,	7	, 0,	14,	9,	16,	5	, 0)) # 15 decisions variables coeffients

# constraints LHS equations
set.row(lpModel, 1, c(1, 1, 1), indices = c(1, 6, 11))
set.row(lpModel, 2, c(1, 1, 1), indices = c(2, 7, 12))
set.row(lpModel, 3, c(1, 1, 1), indices = c(3, 8, 13))
set.row(lpModel, 4, c(1, 1, 1), indices = c(4, 9, 14))
set.row(lpModel, 5, c(1, 1, 1), indices = c(5, 10, 15))
set.row(lpModel, 6, rep(1, 5), indices = c(1:5))
set.row(lpModel, 7, rep(1, 5), indices = c(6:10))
set.row(lpModel, 8, rep(1, 5), indices = c(11:15))

# constraints RHS values
set.rhs(lpModel, c(45, 70, 30, 55, 10, 72, 60, 78))
set.constr.type(lpModel, c(">=", ">=", ">=", ">=" , ">=", "<=", "<=", "<="))

# bounds
set.type(lpModel, c(1:15), "real")
set.bounds(lpModel, lower = rep(0, 15), upper = rep(Inf, 15))

write.lp(lpModel, filename = "test.lp")  #Use write.lp to print out larger LPs.
lpModel
solve(lpModel)

get.objective(lpModel)
get.variables(lpModel)
get.constraints(lpModel)

### oil supply = demand 3 cities 3 plants###

library(lpSolveAPI)

# model and objective
lpModel <- make.lp(6, 9) # 6 constraints and 9 decision variables
lp.control(lpModel, sense = "minimize") # min supply costs

# objective functions
set.objfn(lpModel, c(.12,.18,999,.3,.1,.8,.2,.25,.12)) # 9 decisions variables coeffients

# constraints LHS equations
set.row(lpModel, 1, c(1, 1, 1), indices = c(1:3))
set.row(lpModel, 2, c(1, 1, 1), indices = c(4:6))
set.row(lpModel, 3, c(1, 1, 1), indices = c(7:9))
set.row(lpModel, 4, c(1, 1, 1), indices = c(1, 4, 7))
set.row(lpModel, 5, c(1, 1, 1), indices = c(2,5,8))
set.row(lpModel, 6, rep(1, 3), indices = c(3,6,9))

# constraints RHS values
set.rhs(lpModel, c(6000000, 5000000, 8000000, 4000000, 8000000, 7000000))
set.constr.type(lpModel, c("<=", "<=", "<=", ">=" , ">=", ">="))

# bounds
set.type(lpModel, c(1:9), "real")
set.bounds(lpModel, lower = rep(0, 9), upper = rep(Inf, 9))

write.lp(lpModel, filename = "test.lp")  #Use write.lp to print out larger LPs.
lpModel
solve(lpModel)

get.objective(lpModel)
get.variables(lpModel)
get.constraints(lpModel)

###  multi period production ###

library(lpSolveAPI)

# model and objective
lpModel <- make.lp(11, 8) # 11 constraints and 8 decision variables
lp.control(lpModel, sense = "minimize") # min supply costs

# objective functions
set.objfn(lpModel, c(20,22,24,26,3.5,3.5,3.5,3.5)) # 8 decisions variables coeffients

# constraints LHS equations
set.row(lpModel, 1, c(1, -1), indices = c(1,5))
set.row(lpModel, 2, c(1, 1, -1), indices = c(2,5,6))
set.row(lpModel, 3, c(1, 1, -1), indices = c(3,6,7))
set.row(lpModel, 4, c(1, 1), indices = c(4, 7))
set.row(lpModel, 5, c(1), indices = c(5))
set.row(lpModel, 6, c(1), indices = c(6))
set.row(lpModel, 7, c(1), indices = c(7))
set.row(lpModel, 8, c(1), indices = c(1))
set.row(lpModel, 9, c(1), indices = c(2))
set.row(lpModel, 10, c(1), indices = c(3))
set.row(lpModel, 11, c(1), indices = c(4))

# constraints RHS values
set.rhs(lpModel, c(300,400,450,250,100,100,100,400,400,400,400))
set.constr.type(lpModel, c("=", "=", "=", "=" , "<=", "<=", "<=", "<=", "<=", "<=", "<="))

# bounds
set.type(lpModel, c(1:8), "real")
set.bounds(lpModel, lower = rep(0, 8), upper = rep(Inf, 8))

write.lp(lpModel, filename = "test.lp")  #Use write.lp to print out larger LPs.
lpModel
solve(lpModel)

get.objective(lpModel)
get.variables(lpModel)
get.constraints(lpModel)


###  single period supply demand - 3 car factories supplying to 2 cities ###

library(lpSolveAPI)

# model and objective
lpModel <- make.lp(5, 6) # 5 constraints and 6 decision variables
lp.control(lpModel, sense = "minimize") # min supply costs

# objective functions
set.objfn(lpModel, c(80,215,100,108,102,68)) # 6 decisions variables coeffients

# constraints LHS equations
set.row(lpModel, 1, c(1, 1), indices = c(1,2))
set.row(lpModel, 2, c(1, 1), indices = c(3,4))
set.row(lpModel, 3, c(1, 1), indices = c(5,6))
set.row(lpModel, 4, c(1, 1, 1), indices = c(1,3, 5))
set.row(lpModel, 5, c(1, 1, 1), indices = c(2,4,6))

# constraints RHS values
set.rhs(lpModel, c(1000,1500,1200,2300,1400))
set.constr.type(lpModel, c("<=", "<=", "<=", ">=", ">="))

# bounds
set.type(lpModel, c(1:6), "real")
set.bounds(lpModel, lower = rep(0, 6), upper = rep(Inf, 6))

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

