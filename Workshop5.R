# Transformations
volleyFilePath <- "Datasets/volley.txt"
volley <- read.table(volleyFilePath)
volley

# 1. Scaling/ Feature scaling: Chnages all values to interval [0,1]
col1 <- volley[, 1]
col1
minCol1 <- min(col1)
maxCol1 <- max(col1)
col1New <- (col1 - minCol1) / (maxCol1 - minCol1)
col1New

my.freq <- function(x) {
  ave(x, x, FUN = seq_along)
}

# line plots vs any other col overlap exactly.
library(plotrix)
plot(col1, volley[, 2], type = "p", col = "red") # x will start from 1 by deafult
plot(col1New, volley[, 2], type = "p", col = "blue") # adds this line to the plot above

hist(col1New)

# 2. Standardised
col1Normalised <- (col1 - mean(col1)) / sd(col1)
col1Normalised
# Again line plot will be exactly the same
plot(col1Normalised, volley[, 2], type = "p", col = "green")

hist(col1Normalised)

# 2.1 Standardised unit interval
col1Normalised2 <- 0.15 *( (col1 - mean(col1)) / sd(col1) ) + 0.5
col1Normalised2
hist(col1Normalised2)
# Again line plot will be exactly the same
plot(col1Normalised2, volley[, 2], type = "p", col = "black")



