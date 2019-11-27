`# replacing values in vectors
r <- 1:10
r[1] <- 99
r[c(5, 7)] <- 101 # replaces 5th and 7th elements only
r
r[c(6, 8)] <-
  c(11, 22, 33) # error replacement values count does not match the items to replace
r[c(6, 8, 9)] <- c(11, 22, 33)
r

# binding col and row
a <- c(1, 2, 3, 7, 9)
a

# cbind : creates columns
b <- c("Col1", "Col2", "Col3")
b
b <- cbind(b, c(11, 22, 33))
b
b[1,] # entire col
b[1, 1] # col name
b[1, 2] #col value
c <- cbind(a, c(11, 22, 33, 44, 55), 1:5)
c
c[1,]


# rbind : creates rows
r <- rbind(a, c(111, 222, 333, 444, 555))
r
r <- rbind(a, 101:105) # replaces elements in the row
r
r <- rbind(r, 11:15) # appends elements
r


# matrices
m <- array(0, c(3, 4)) # 3x4 matrix with all elements as 0
m
m[1, 2] <- 99
m
m[2,] <- c(1, 2, 3, 4) # replaces a row
m[, 1] <- c(11, 22, 33) # replaces a col
m
m[2:3, 3:4] <- array(-1, c(2, 2)) # replaces a sub matriz within m
m


# tables, reading from file
csvPath <- "Datasets/employees.csv"
tcsv <- read.csv(csvPath) # reads with CSV headers
tcsv
t <-
  read.table(csvPath) # reads without table headers or any consideration to commas, hence just one column in the table
t
t <-
  read.table(csvPath, header = TRUE, sep = ",") # Reads with headers, just like read.csv
t

# tables, writing to file
outputFilePath <- "Datasets/sample.txt"
data <- cbind(c("a", 1, 2), c(3, 2, 5)) # 2 vectors as columns
data
write.table(data, file = outputFilePath) # writes the data including an index column and with quotes around each cell
t2Csv <- read.csv(outputFilePath)
t2Csv
t2 <- read.table(outputFilePath)
t2
t2 + 2 # adds 2 to each numberic cell, give NA for string cells, does not add to index column
t2[2, ] + 3 # adds 3 to the  2nd row


# plotting
x <-
  rnorm(20, sd = 4, mean = 10) # 20 random normal numbers with mean =10 and standard deviation set to 4
x
y <- 4.5 * x - 2.0 + rnorm(20, sd = 5, mean = 0)
y
cor(x, y) # calculates the correlation/covariance
plot(
  x,
  y,
  xlab = "x axis",
  ylab = "y axis",
  main = "Plot title",
  type = 'p'
) # p = points, l = lines, b = both etc

x2 <-
  runif(6, 12, 24) # random uniform distributed numbers between the specified mina and max
x2
y2 <- 2.5 * x2 - 1.0 + runif(6,-5, 5)
y2
points(x2, y2, col = 2) # ??
plot(x2, y2)

x3 <- c(11, 5, 6, 2)
plot(x3)
plot(sort(x3))
hist(x3)


# plotting function like f(t) = t^2
X <- 1:10
plot(X ^ 2, type = 'l')

# or
Y <- array(0, length(X))
for (i in 1:length(X)) {
  Y[i] <- (X[i] ^ 2)
}
plot(X, Y, type = 'l')
