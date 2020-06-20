# import lib, before doing that run the target script
source("AggWaFit718.R")

#######################
# Power mean
#######################

a <- c(11,5,1,11,76,42,11,26,23,18)
sort(a)
median(a)
PM(a, p = -Inf) # approaches minimum function
PM(a, p = -1) # Harmonic mean
PM(a, p = 0) # Geometric mean
PM(a, p = 1) # Arithematic mean
PM(a, p = Inf) # approaches maximum function
skewness(a)
skewness(polyFunc(a,0.5))

# Weighted Power Mean
w <- c(0.4, 0.3, 0.2, 0.1)
a <- c(3,3,7,7)
PM(a, w, -2) # p=-2
PM(c(0.1, .2, .5, .2), c(.5, .3, .1, .1), 0) # weighted geometric mean, p=0
PM(a,w, 1) # weighted arithematic mean, p=1

# Ordered weighted Average : OWA
w <- c(0.4, 0.3, 0.2, 0.1)
a <- c(8,7,6,4)
OWA(a, w)

# Standard Deviation
a <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
sd(a)

#######################
# Minkowski distances
#######################

a <- c(1.0, 2.21, 0.4)
b <- c(0.3,0.18,-0.11)
minkowski(a, b, 2) # p=2, euclidean distance
minkowski(a, b) # p=1, manhattan distance

#######################
# Transformation or scaling
#######################
a <- c(1, 2, 3, 4)
unitscaleFunc(a)
scaleFunc(a, 100, 200)
polyFunc(a, 0.5)

#######################
# Orness
#######################

w <- c(.2, .1, .3)
orness.OWA(w) # = 0.35 indicating the OWA function tends towards lower inputs.

#######################
# Choquet
#######################
x <- c(0.75, 0.45, .19)
# v(1), v(2),v(2,1), v(3),v(3,1),v(3,2),v(3,2,1)
w <- c(.0, .0, .8, .3, .8, .7, 1)
choquet(x, w)

# v(1), v(2),v(2,1), v(3),v(3,1),v(3,2),v(3,2,1)
w <- c(.45, .45, .5, .3, .9, .9, 1)
sachin <- c(18, 16, 10)
courtney <- c(10, 12, 18)
wasim <- c(14, 15, 15)
choquet(sachin, w)
choquet(courtney, w)
choquet(wasim, w)

#######################
# Error measures
#######################

a <- c(90, 40, 69, 31, 39, 44, 21, 81, 25, 52)
b <- c(64, 42, 65, 4, 55, 42, 18, 79, 46, 62)

sse(a, b) # SSE : Sum of Squared Error
sae(a, b)# SAE: Sum of Absolute Errors
mae(a, b)# Average Absolute Error
rmse(a, b)# RMSE
cor(a, b)# Pearson Correlation
cor(a, b, method = "spearman")# Spearman Correlation

#######################
# linear programming
#######################

## Toy Company: Solider and train
library(lpSolveAPI)

# model and objective
lpModel <- make.lp(3, 2) # 3 constraints and 2 decision variables
lp.control(lpModel, sense = "maximize") # max the profit

# objective functions
set.objfn(lpModel, c(3, 2)) # 2 decisions variables coeffients

# constraints LHS equations
set.row(lpModel, 1, c(2, 1), indices = c(1, 2))
set.row(lpModel, 2, c(1, 1), indices = c(1, 2))
set.row(lpModel, 3, c(1), indices = c(1))

# constraints RHS values
set.rhs(lpModel, c(100, 80, 40))
set.constr.type(lpModel, c("<=",	"<=",	">="))

# bounds
set.type(lpModel, c(1:2), "real")
set.bounds(lpModel, lower = rep(0, 2), upper = rep(Inf, 2))

# write.lp(multiPeriodModel, filename="test.lp")  #Use write.lp to print out larger LPs.
lpModel
solve(lpModel)
get.objective(lpModel)
get.variables(lpModel)


## Toy company assembly line 3 workstations, trains and soilders
library(lpSolveAPI)

# model and objective
lpModel <- make.lp(3, 2) # 3 constraints and 2 decision variables
lp.control(lpModel, sense = "minimize") # min idle time of workstations

# objective functions
set.objfn(lpModel, c(-12,-19)) # 2 decisions variables coeffients

# constraints LHS equations
set.row(lpModel, 1, c(3, 6), indices = c(1, 2))
set.row(lpModel, 2, c(5, 5), indices = c(1, 2))
set.row(lpModel, 3, c(4, 8), indices = c(1, 2))

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

# Farmer dave decision of buying feed 1 and feed 2 for nutrients A,B,C

# model and objective
lpModel <- make.lp(3, 2) # 3 constraints and 2 decision variables
lp.control(lpModel, sense = "minimize") # min cost of feed

# objective functions
set.objfn(lpModel, c(10, 3)) # 2 decisions variables coeffients

# constraints LHS equations
set.row(lpModel, 1, c(3, 2), indices = c(1, 2))
set.row(lpModel, 2, c(7, 2), indices = c(1, 2))
set.row(lpModel, 3, c(3, 6), indices = c(1, 2))

# constraints RHS values
set.rhs(lpModel, c(60, 84, 72))
set.constr.type(lpModel, c(">=",	">=",	">="))

# bounds
set.type(lpModel, c(1:2), "real")
set.bounds(lpModel, lower = rep(0, 2), upper = rep(Inf, 2))

# write.lp(multiPeriodModel, filename="test.lp")  #Use write.lp to print out larger LPs.
lpModel
solve(lpModel)
get.objective(lpModel)
get.variables(lpModel)

# Farmer vicky magic feed with 3 nutrientsusing 2 raw materials

# model and objective
lpModel <- make.lp(3, 2) # 3 constraints and 2 decision variables
lp.control(lpModel, sense = "minimize") # min cost of raw materials

# objective functions
set.objfn(lpModel, c(.3, .9)) # 2 decisions variables coeffients

# constraints LHS equations
set.row(lpModel, 1, c(.21,-.3), indices = c(1,2))
set.row(lpModel, 2, c(.03,-.01), indices = c(1,2))
set.row(lpModel, 3, c(1,1), indices = c(1,2))

# constraints RHS values
set.rhs(lpModel, c(0, 0, 800))
set.constr.type(lpModel, c("<=",	">=",	">="))

# bounds
set.type(lpModel, c(1:2), "real")
set.bounds(lpModel, lower = rep(0, 2), upper = rep(Inf, 2))

# write.lp(multiPeriodModel, filename="test.lp")  #Use write.lp to print out larger LPs.
lpModel
solve(lpModel)
get.objective(lpModel)
get.variables(lpModel)

## Bank loans

# model and objective
lpModel <- make.lp(4, 5) # 4 constraints and 5 decision variables
lp.control(lpModel, sense = "maximize")

# objective functions
set.objfn(lpModel, c(0.06497, 0.011468, 0.014814, 0.06875, 0.0505)) # 5 decisions variables coeffients

# constraints LHS equations
set.row(lpModel, 1, rep(1, 5), indices = c(1:5))
set.row(lpModel, 2, c(-0.04, -0.04,-0.04, 0.6, 0.6), indices = c(1:5))
set.row(lpModel, 3, c(-0.5, -0.5, 0.5), indices = c(1, 2, 3))
set.row(lpModel, 4, c(0.06, 0.03,-0.01, 0.01, 0.005), indices = c(1:5))

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




## Supply = demand  electricity 4 cities 3 plants

# model and objective
lpModel <- make.lp(7, 12) # 7 constraints and 12 decision variables
lp.control(lpModel, sense = "minimize")

# objective functions
set.objfn(lpModel, c(8,6,10,9,9,12,13,7,14,9,16,5)) # 12 decisions variables coeffients

# constraints LHS equations
set.row(lpModel, 1, rep(1, 4), indices = c(1:4))
set.row(lpModel, 2, rep(1, 4), indices = c(5:8))
set.row(lpModel, 3, rep(1, 4), indices = c(9:12))
set.row(lpModel, 4, c(1,1,1), indices = c(1,5,9))
set.row(lpModel, 5, c(1,1,1), indices = c(2,6,10))
set.row(lpModel, 6, c(1,1,1), indices = c(3,7,11))
set.row(lpModel, 7, c(1,1,1), indices = c(4,8,12))

# constraints RHS values
set.rhs(lpModel, c(72,50,78,45,70,30,55))
set.constr.type(lpModel, c("<=", "<=",	"<=", ">=", ">=", ">=", ">="))

# bounds
set.type(lpModel, c(1:12), "real")
set.bounds(lpModel, lower = rep(0, 12), upper = rep(Inf, 12))

# write.lp(multiPeriodModel, filename="test.lp")  #Use write.lp to print out larger LPs.
lpModel
solve(lpModel)

get.objective(lpModel)
get.variables(lpModel)
get.constraints(lpModel)


## Supply = demand  cello 4 quaters with overtime and storage costs

# model and objective
lpModel <- make.lp(12, 12) # 12 constraints and 12 decision variables
lp.control(lpModel, sense = "minimize") # min cost of prod plus storage and overtime

# objective functions
set.objfn(lpModel, c(400,400,400,400,450,450,450,450,20,20,20,20)) # 12 decisions variables coeffients

# constraints LHS equations
set.row(lpModel, 1, c(1,1,-1), indices = c(1,5,9))
set.row(lpModel, 2, c(1,1,1,-1), indices = c(2,6,9,10))
set.row(lpModel, 3, c(1,1,1,-1), indices = c(3,7,10,11))
set.row(lpModel, 4, c(1,1,1), indices = c(4,8,11))
set.row(lpModel, 5, c(1), indices = c(1))
set.row(lpModel, 6, c(1), indices = c(2))
set.row(lpModel, 7, c(1), indices = c(3))
set.row(lpModel, 8, c(1), indices = c(4))
set.row(lpModel, 9, c(1), indices = c(5))
set.row(lpModel, 10, c(1), indices = c(6))
set.row(lpModel, 11, c(1), indices = c(7))
set.row(lpModel, 12, c(1), indices = c(8))

# constraints RHS values
set.rhs(lpModel, c(30,60,75,25,40,40,40,40,150,150,150,150))
set.constr.type(lpModel, c("=", "=",	"=", "=", "<=", "<=", "<=", "<=", "<=", "<=", "<=", "<="))

# bounds
set.type(lpModel, c(1:12), "real")
set.bounds(lpModel, lower = rep(0, 12), upper = rep(Inf, 12))

# write.lp(multiPeriodModel, filename="test.lp")  #Use write.lp to print out larger LPs.
lpModel
solve(lpModel)

get.objective(lpModel)
get.variables(lpModel)
get.constraints(lpModel)
# not sure about correctness of answer


## Inventory probelm projected and solved as a Supply = demand  
# 4 quarter production and 4 quarter supply, excess storage cost, excees prodsupply costs 
# and dummy demand point to absord excess supply
# and initial 10 suppli at q1 available at no production cost

# model and objective
lpModel <- make.lp(14, 45) # 14 constraints and 5x9 decision variables
lp.control(lpModel, sense = "minimize")
m <- 99999 # very large cost of supplying from Q2 to Q1 which will prevent solver from considering it feasible
# objective functions
set.objfn(lpModel, c(0,20,40,60, 0, 
                     400,420,440,460,0, 450,470,490,510, 0,
                     m,400,420,440,0, m,450,470,490, 0,
                     m,m,400,420,0, m,m,450,470, 0,
                     m,m,m,400,0, m,m,m,450, 0)) # 45 decisions variables coeffients 0 for dummy demand point

# constraints LHS equations
set.row(lpModel, 1, rep(1, 5), indices = c(1:5))
set.row(lpModel, 2, rep(1, 5), indices = c(6:10))
set.row(lpModel, 3, rep(1, 5), indices = c(11:15))
set.row(lpModel, 4, rep(1, 5), indices = c(16:20))
set.row(lpModel, 5, rep(1, 5), indices = c(21:25))
set.row(lpModel, 6, rep(1, 5), indices = c(26:30))
set.row(lpModel, 7, rep(1, 5), indices = c(31:35))
set.row(lpModel, 8, rep(1, 5), indices = c(36:40))
set.row(lpModel, 9, rep(1, 5), indices = c(41:45))
#  demAND
set.row(lpModel, 10, rep(1,9), indices = c(1,6,11,16,21,26,31,36,41))
set.row(lpModel, 11, rep(1,9), indices = c(2,7,12,17,22,27,32,37,42))
set.row(lpModel, 12, rep(1,9), indices = c(3,8,13,18,23,28,33,38,43))
set.row(lpModel, 13, rep(1,9), indices = c(4,9,14,19,24,29,34,39,44))
set.row(lpModel, 14, rep(1,9), indices = c(5,10,15,20,25,30,35,40,45))

# constraints RHS values
set.rhs(lpModel, c(10,40,150,40,450,40,150,40,150, 40,60,75,25,570))
set.constr.type(lpModel, c("<=","<=","<=","<=","<=","<=","<=","<=","<=", ">=",">=", ">=",">=",">="))

# bounds
set.type(lpModel, c(1:45), "real")
set.bounds(lpModel, lower = rep(0, 45), upper = rep(Inf, 45))

# write.lp(multiPeriodModel, filename="test.lp")  #Use write.lp to print out larger LPs.
lpModel
solve(lpModel)

get.objective(lpModel)
get.variables(lpModel)
get.constraints(lpModel)




#######################
# Game theory
#######################
