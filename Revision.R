# import lib, before doing that run the target script
source("AggWaFit718.R")

#######################
# Power mean
#######################

a <- c(9, 10, 17, 16)
PM(a, p = -Inf) # approaches minimum function
PM(a, p = -1) # Harmonic mean
PM(a, p = 0) # Geometric mean
PM(a, p = 1) # Arithematic mean
PM(a, p = Inf) # approaches maximum function

# Weighted Power Mean
a <- c(0.7, 0.20, 0.5)
w <- c(0.2, 0.25, 0.55)
PM(a, w,-2) # p=-2
PM(c(0.1, .2, .5, .2), c(.5, .3, .1, .1), 0) # weighted geometric mean, p=0
PM(c(.1, .9, .3, .2), c(.3, .6, .05, .05), 1) # weighted arithematic mean, p=1

# Ordered weighted Average : OWA
a <- c(.2, .3, .1, .9)
w <- c(7 / 16, 5 / 16, 3 / 16, 1 / 16)
OWA(a, w)

# Standard Deviation
a <- c(1,2,3,4,5,6,7,8,9,10)
sd(a)

#######################
# Minkowski distances
#######################

a <- c(9, 10, 17, 16)
b <- c(1, 2, 3, 4)
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

w <- c(.2,.1,.3)
orness.OWA(w) # = 0.35 indicating the OWA function tends towards lower inputs.

#######################
# Choquet
#######################
x <- c(0.8,0.3,.4)
# v(1), v(2),v(2,1), v(3),v(3,1),v(3,2),v(3,2,1)
w <- c(.4, .1,.6, .1,.6,.9,1) 
choquet(x,w)

# v(1), v(2),v(2,1), v(3),v(3,1),v(3,2),v(3,2,1)
w <- c(.45, .45,.5, .3,.9,.9,1) 
sachin <- c(18,16,10)
courtney <- c(10,12,18)
wasim <- c(14,15,15)
choquet(sachin,w)
choquet(courtney,w)
choquet(wasim,w)

#######################
# Error measures
#######################

a <- c(90, 40, 69, 31, 39, 44, 21, 81, 25, 52)
b <- c(64, 42, 65, 4, 55, 42, 18, 79, 46, 62)

sse(a,b) # SSE : Sum of Squared Error
sae(a,b)# SAE: Sum of Absolute Errors 
mae(a,b)# Average Absolute Error
rmse(a,b)# RMSE
cor(a,b)# Pearson Correlation
cor(a,b,method="spearman")# Spearman Correlation

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
set.row(lpModel, 1, c(2,1), indices = c(1,2))
set.row(lpModel, 2, c(1,1), indices = c(1,2))
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



# Farmer dave decision of buying feed 1 and feed 2 for nutrients A,B,C

# model and objective
lpModel <- make.lp(3, 2) # 3 constraints and 2 decision variables
lp.control(lpModel, sense = "minimize") # min cost of feed

# objective functions
set.objfn(lpModel, c(10, 3)) # 2 decisions variables coeffients

# constraints LHS equations
set.row(lpModel, 1, c(3,2), indices = c(1,2))
set.row(lpModel, 2, c(7,2), indices = c(1,2))
set.row(lpModel, 3, c(3,6), indices = c(1,2))

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

#######################
# Game theory
#######################
