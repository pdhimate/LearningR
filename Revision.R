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
#######################

#######################
#######################

#######################
#######################

#######################
#######################

#######################
#######################

#######################
#######################

#######################
#######################
