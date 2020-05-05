## Single period and multi period production models
# SIT718 Prac. 08 - LP with 2+ Variables 

##############################

# Single Period Production Model
# Problem Description: Under 'Your Tasks' in Week 8 Prac


library(lpSolveAPI)

singlePeriodModel <- make.lp(8, 8)

lp.control(singlePeriodModel, sense= "maximize")

set.objfn(singlePeriodModel, c(30,	40,	20,	10,	-15,	-20,	-10,	-8))

# Set rows for 1st columns for capacities
set.row(singlePeriodModel, 1, c(0.3,	0.3,	0.25,	0.15), indices = c(1:4))
set.row(singlePeriodModel, 2, c(0.25,	0.35,	0.3,	0.1), indices = c(1:4))
set.row(singlePeriodModel, 3, c(0.45,	0.5,	0.4,	0.22), indices = c(1:4))
set.row(singlePeriodModel, 4, c(0.15,	0.15,	0.1,	0.05), indices = c(1:4))
# set rows for col1 (profit) and col5 (penalty) and so on
set.row(singlePeriodModel, 5, c(1,1), indices =c(1,5))
set.row(singlePeriodModel, 6, c(1,1), indices =c(2,6))
set.row(singlePeriodModel, 7, c(1,1), indices =c(3,7)) 
set.row(singlePeriodModel, 8, c(1,1), indices =c(4,8)) 
# 8x8 matrix created


# set the agg col (rightmost) (capacities first 4 rows and demand next 4 rows)
set.rhs(singlePeriodModel, c(1000,	1000,	1000,	1000,	800,	750,	600,	500))
# set equality operator for rightmost col
set.constr.type(singlePeriodModel, c("<=",	"<=",	"<=", "<=", "=",	"=",	"=", "="))

set.type(singlePeriodModel, c(1:8),"real") # real numbers in each column
# set upper adn lower bounds for the func
set.bounds(singlePeriodModel, lower = rep(0, 8), upper = rep(Inf, 8))

# write.lp(singlePeriodModel, filename="test.lp")  Use write.lp to print out larger LPs. 
#  It produces a text file, which you can examine with any text editor.

singlePeriodModel # see the rows we have set in the model
solve(singlePeriodModel) # http://lpsolve.sourceforge.net/5.5/solve.htm

objvalue<-get.objective(singlePeriodModel)
objvalue # maximum profit
solution<-get.variables(singlePeriodModel)
solution # values => 1st 4 for no of products for each product. Next 4 for products not made (i.e deficit/penalty) 

################################
# Multiple Period Production Model
# Problem Description: Under 'Your Tasks' in Week 8 Prac

library(lpSolveAPI)

multiPeriodModel <- make.lp(6, 12)

lp.control(multiPeriodModel, sense= "minimize")

# 6 months production costs and 6 months storage costs
set.objfn(multiPeriodModel, c(50,	45,	55,	48,	52,	50,	8,	8,	8,	8,	8,	8))

# produce x1 tables and store -i1 tables
set.row(multiPeriodModel, 1, c(1,-1), indices = c(1,7))
# produce x2 tables and store i1-i2 tables (excess from previosu months)
set.row(multiPeriodModel, 2, c(1,1,-1), indices = c(2,7,8)) # carry over from previous month hence 1,1 
set.row(multiPeriodModel, 3, c(1,1,-1), indices = c(3,8,9))
set.row(multiPeriodModel, 4, c(1,1,-1), indices = c(4,9,10))
set.row(multiPeriodModel, 5, c(1,1,-1), indices = c(5,10,11))
set.row(multiPeriodModel, 6, c(1,1), indices = c(6,11))

# constraints
set.rhs(multiPeriodModel, c(100,	250,	190,	140,	220,	110))
set.constr.type(multiPeriodModel, c("=",	"=",	"=", "=", "=",	"="))

set.type(multiPeriodModel, c(1:12),"real")
set.bounds(multiPeriodModel, lower = rep(0, 12), upper = rep(Inf, 12))

# write.lp(multiPeriodModel, filename="test.lp")  #Use write.lp to print out larger LPs. 
#  It produces a text file, which you can examine with any text editor.
multiPeriodModel # refer to excel file Datasets/Workshop 08 Mapping variables.xlsx
solve(multiPeriodModel) # http://lpsolve.sourceforge.net/5.5/solve.htm

objvalue<-get.objective(multiPeriodModel)
objvalue # optimal production cost (minimum)
solution<-get.variables(multiPeriodModel)
solution # indicates max tables produced in 2nd month 


################################

# Bank Loan Problem 
# Week 8.6 - Bank loans 

library(lpSolveAPI)

bankLoanModel <- make.lp(0, 5)

lp.control(bankLoanModel, sense= "maximize")

set.objfn(bankLoanModel, c(0.06497, 0.011468, 0.014814, 0.06875, 0.0505))

add.constraint(bankLoanModel, c(1, 1, 1, 1, 1), "<=", 12)

add.constraint(bankLoanModel, c(0.4, 0.4, 0.4, -0.6, -0.6), "<=", 0)

add.constraint(bankLoanModel, c(0.5, 0.5, -0.5, 0, 0), "<=", 0)

add.constraint(bankLoanModel, c(0.06, 0.03, -0.01, 0.01, 0.005), "<=", 0)

#set.bounds(bankLoanModel, lower = c(28.6, 18), columns = c(1, 4))

#set.bounds(bankLoanModel, upper = 48.98, columns = 4)

RowNames <- c("Constraint 1", "Constraint 2", "Constraint 3", 'Constraint 4')

ColNames <- c("Personal", "Car", "Home", "Farm","Commercial")

dimnames(bankLoanModel) <- list(RowNames, ColNames)

solve(bankLoanModel) # http://lpsolve.sourceforge.net/5.5/solve.htm

get.objective(bankLoanModel)

get.variables(bankLoanModel)

get.constraints(bankLoanModel) 

bankLoanModel


######################################################################


# Week 8.8 Transportation Problem - Electric Power Plants  
# It is similary to Week 8.11 code

library(lpSolveAPI)

electricityModel <- make.lp(8, 15)

lp.control(electricityModel, sense= "minimize")

# supplies for city1 from 3 powerplants and city2 from 3 powerplants and so on.
# the last three are dummy	varibles to meet the excess supply, actual supply 210 - actual demand	200

# constraints for each city and each powerplant
set.objfn(electricityModel, c(8,6,10,9,9,12,13,7,14,9,16,5,0,0,0)) 
set.row(electricityModel, 1, rep(1,5), indices = c(1:4,13))
set.row(electricityModel, 2, rep(1,5), indices = c(5:8,14))
set.row(electricityModel, 3, rep(1,5), indices = c(9:12,15))
set.row(electricityModel, 4,rep(1,3), indices =c(1,5,9))
set.row(electricityModel, 5,rep(1,3), indices =c(2,6,10))
set.row(electricityModel, 6,rep(1,3), indices =c(3,7,11))
set.row(electricityModel, 7,rep(1,3), indices =c(4,8,12)) 
set.row(electricityModel, 8,rep(1,3), indices =c(13:15)) 

# constraints limits 1st 4 are powerplant supply limits
# last 4 are demand limits for each city and also last is the demand from the dummy city
set.rhs(electricityModel, c(72,	60,	78,	45,	70,	30,	55,10))
set.constr.type(electricityModel, c("<=",	"<=",	"<=", ">=", ">=",	">=",	">=",">="))

set.type(electricityModel, c(1:15),"real")
set.bounds(electricityModel, lower = rep(0, 15), upper = rep(Inf, 15))

electricityModel
solve(electricityModel) # http://lpsolve.sourceforge.net/5.5/solve.htm

objvalue<-get.objective(electricityModel)
objvalue # 
solution<-get.variables(electricityModel)
solution

#write.lp(electricityModel, filename="test.lp")  #Use write.lp to print out larger LPs. 

# todo finish the problem 4 in excel then prohram it










