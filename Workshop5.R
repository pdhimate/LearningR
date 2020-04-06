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
col1Normalised2 <- 0.15 * ((col1 - mean(col1)) / sd(col1)) + 0.5
col1Normalised2
hist(col1Normalised2)
# Again line plot will be exactly the same
plot(col1Normalised2, volley[, 2], type = "p", col = "black")

# 3. Log / polynomial transformation
col1Log <- log(col1)
col1Log
plot(col1Log, volley[, 2], type = "p", col = "blue")
hist(col1Log)

# =========================
## Matrices
v1 <- c(1, 2, 3)
v2 <- c(10, 11, 12, 13, 14, 15)

# 2 3x3 matrices
rCount <- 3
cCount <- 3
noOfMatrices <- 2
results <- array(c(v1, v2), dim = c(rCount, cCount, 2))
results

# row and col names for matrices
colNames <- c('col1', 'col2', 'col3')
rowNames <- c('row1', 'row2', 'row3')
matrixNames <- c('mat1', 'mat2')

namedMatrices <- array(
  c(v1, v2),
  dim = c(rCount, cCount, 2),
  dimnames = list(rowNames, colNames, matrixNames)
)
namedMatrices

# Access secoond matrix
namedMatrices[, , 2] # row, col, matrix
namedMatrices[, , 'mat2']

# Access 3rd row of second matrix
namedMatrices[3, , 2] # row, col, matrix
namedMatrices['row3', , 'mat2'] # row, col, matrix

# Access 3rd row, 2nd element of second matrix
namedMatrices[3, 2, 2] # row, col, matrix
namedMatrices['row3', 'col2', 'mat2'] # row, col, matrix


# Operations on matrices
matrix1 <- namedMatrices[, , 'mat1']
matrix2 <- namedMatrices[, , 'mat2']
matrix3 <- matrix1 + matrix2
matrix3
matrix3 - matrix1


# ========================================
## Transformations
rawData <- runif(100, 10, 50) # 100 random numbers between 10, 50
rawData

# 1. Negation = Max - num + Min
negatedData <- 50 - rawData + 10
negatedData
plot(negatedData, type = "o" , col = "blue")
lines(rawData, type = "o", col = "red")

# E.G in volley file the first col is sprint time, which needs to be
# negated to get averge score of each player to decide who is best.
volleyFile <- 'Datasets/volley.txt'
volleyTable <- read.table(volleyFile)
volleyTable

# read the table and negate first col
sprintCol <- volleyTable[, 1]
sprintCol
sprintColNegated <- max(sprintCol) - sprintCol + min(sprintCol)
sprintColNegated

# store in a new table/matrix
transformedVolleyTable <- volleyTable
transformedVolleyTable[, 1] <- sprintColNegated
transformedVolleyTableOrdered <-
  transformedVolleyTable[order(transformedVolleyTable$V1, decreasing = TRUE),]
plot(transformedVolleyTableOrdered[, 1])

# 2. Log & polynomial transformation
logtransVolley <- volleyTable
logtransVolley[, 1] <- log10(volleyTable[, 1])
logtransVolleySorted <-
  logtransVolley[order(logtransVolley$V1 , decreasing = FALSE), ]
plot(logtransVolleySorted[, 1])

sqtransVolley <- volleyTable
sqtransVolley[, 1] <- (volleyTable[, 1]) ^ 2
sqtransVolleySorted <-
  sqtransVolley[order(sqtransVolley$V1 , decreasing = FALSE), ]
sqtransVolleySorted[, 1]


