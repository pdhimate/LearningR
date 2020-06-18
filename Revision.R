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
v <- c(.4, .1,.6, .1,.6,.9,1) 
choquet(x,v)

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