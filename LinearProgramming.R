##########################################
# Linear programming practise
##########################################

##### Toy Company ########

library(lpSolveAPI)

# model and objective
lpModel <- make.lp(3, 2) # 3 constraints and 2 decision variables
lp.control(lpModel, sense= "maximize") # max the profit

# objective functions
set.objfn(lpModel, c(3, 2)) # 2 decisions variables coeffients

# constraints LHS equations
set.row(lpModel, 1, c(1), indices = c(1))
set.row(lpModel, 2, c(2,1), indices = c(1,2))
set.row(lpModel, 3, c(1,1), indices = c(1,2))

# constraints RHS values
set.rhs(lpModel, c(40, 100, 80))
set.constr.type(lpModel, c(">=",	"<=",	"<="))

# bounds
set.type(lpModel, c(1:2),"real")
set.bounds(lpModel, lower = rep(0, 2), upper = rep(Inf, 2))

# write.lp(multiPeriodModel, filename="test.lp")  #Use write.lp to print out larger LPs. 
lpModel
solve(lpModel) 

objvalue<-get.objective(lpModel)
objvalue 
solution<-get.variables(lpModel)
solution  



##### Toy Company - assembly line workstations ########

library(lpSolveAPI)

# model and objective
lpModel <- make.lp(3, 2) # 3 constraints and 2 decision variables
lp.control(lpModel, sense= "maximize") # max the profit

# objective functions
set.objfn(lpModel, c(12, 19)) # 2 decisions variables coeffients

# constraints LHS equations
set.row(lpModel, 1, c(3,6), indices = c(1,2))
set.row(lpModel, 2, c(5,5), indices = c(1,2))
set.row(lpModel, 3, c(4,8), indices = c(1,2))

# constraints RHS values
set.rhs(lpModel, c(540, 450, 580))
set.constr.type(lpModel, c("<=",	"<=",	"<="))

# bounds
set.type(lpModel, c(1:2),"real")
set.bounds(lpModel, lower = rep(0, 2), upper = rep(Inf, 2))

# write.lp(multiPeriodModel, filename="test.lp")  #Use write.lp to print out larger LPs. 
lpModel
solve(lpModel) 

get.objective(lpModel)
get.variables(lpModel)
get.constraints(lpModel)




















