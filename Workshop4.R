# plotting pie charts
slices <- c(600, 300, 150, 100, 200)
slices

label <- c("Housing", "Food", "Cloths", "Entertainment", "Other")
label

pie(slices,
    labels = label,
    main = "Expenditure",
    clockwise = TRUE)

# compute % for each slice of the pie
percentages <- round(slices / sum(slices) * 100)
label <-
  paste(label, percentages, sep = "-") # appends percent to exitings labels
label

label <- paste(label, "%", sep = "")
label


pie(
  slices,
  labels = label,
  col = rainbow(length(label)),
  # assign rainbow colours to the slices
  main = "Expenditure",
  clockwise = TRUE
)

# Using external libraries.
# Ensure that you select both the library and the satement below while running
library(plotrix)
pie3D(
  slices,
  labels = label,
  col = rainbow(length(label)),
  # assign rainbow colours to the slices
  main = "Expenditure",
  # clockwise = TRUE,
  explode = 0.1
)

# Saving plot as a file
png(filename = "Datasets/Outputs/pie_chart.png") # opens a file from current device
pie(slices, labels = label, main = "Expenditure") # plots in the file
dev.off() # Saves the file. Turns off the current device


# plotting bar grahphs
H <- c(7, 12, 28, 3, 41)
M <- c("Mar", "April", "May", "June", "July")

png(filename = "Datasets/Outputs/bar_chart.png")
barplot(
  H,
  names.arg = M,
  xlab = "Month",
  ylab = "Revenue",
  border = "red",
  col = "blue",
  main = "Revenue Chart"
)
dev.off()

# stacked bar chart
regions <- c("East", "West", "North")
colours <- c("green", "orange", "brown") # 1 color for each region
months <- c("Mar", "Apr", "May", "Jun", "Jul")
valsMatrix <-
  matrix(
    c(2, 9, 3, 11, 9, 4, 8, 7, 3, 12, 2, 8, 10, 11, 12),
    nrow = 3,
    ncol = 5,
    byrow = TRUE
  )
png(filename = "Datasets/Outputs/bar_chart_stacked.png")
barplot(
  valsMatrix,
  main = "total revenue",
  names.arg = months,
  xlab = "month",
  ylab = "revenue",
  col = colours
)
legend("topleft", regions, cex = 1, fill = colours) # displays legend on the previous plot
dev.off()

# plotting Histogram (frequecy against numbers)
v <- c(9, 13, 21, 8, 36, 22, 12, 41, 31, 33, 19)
png(filename = "Datasets/Outputs/histogram.png")
hist(
  x = v,
  xlab = "weight",
  col = "yellow",
  xlim = c(1, 50),
  border = "black"
)
dev.off()

# Line graphs
yVals <- c(7, 12, 28, 3, 41)
yVals2 <- c(14, 7, 6, 19, 13)
png(filename = "Datasets/Outputs/line_graph.png")
plot(yVals, type = "o", col = "red") # x will start from 1 by deafult
lines(yVals2, type = "o", col = "blue") # adds this line to the plot above
dev.off()

# correlations : scatter plot
weight <- c(2.62, 2.875, 2.320, 3.215, 3.44, 3.460)
mileage <- c(21.0, 21, 22.8, 21.4, 18.7, 18.1)
cor(weight, mileage) # calculates correlational points to be able to plot
plot(
  weight,
  mileage,
  xlab = "Weight of vehicle",
  ylab = "Mileage",
  xlim = c(2, 5),
  ylim = c(10, 30),
  main = "Weight VS Mileage"
)


## Transforming variables
volleyFilePath <- "Datasets/volley.txt"
volley <- read.table(volleyFilePath)
volley
original <- volley

# subtract each entry in col1 from 51.24
volley[, 1] <-
  51.24 - volley[, 1]
volley

# apply unit transformation on col1
volley[, 1] <-
  (volley[, 1] - min(volley[, 1])) / max(volley[, 1]) - min(volley[, 1])
volley

# transform height variable using standard deviation on col2
volley[, 2] <- (volley[, 2] - mean(volley[, 2] / sd(volley[, 2])))
volley

# Use  the  linear  feature  scaling  technique  to  get  col2
# and the remaining columns, 3 and 4, to range between 0 and 1

# Sorting, Ranking
a <- c(22, 1, 333, 55555, 4444)
sort(a)
rank(a) # ranks of the numbers in a
rank(-a) # decreasing ranks of the numbers in a
# ranking on volleys/sprint times
volley[, 1] <- original[, 1]
volley
volley[, 1] <-
  (length(volley[, 1] - rank(volley[, 1]))) / (length(volley[, 1] - 1)) # gets highest score
volley


# power mean
PM = function(x, p) {
  if (p == 0) {
    prod(x) ^ {
      1 / length(x)
    }
  }
  else{
    mean(x ^ p) ^ {
      1 / p
    }
  }
}

nums <- c(1, 2, 3, 4, 5)
PM(nums, 1) # arithematic mean
PM(nums, 0) # geometric mean
PM(nums, -1) # harmonic mean


AM = function(x){
  PM(x, 1)
}

GM = function(x){
  PM(x, 0)
}

HM = function(x){
  PM(x, -1)
}

# Verifying properties of means
# 1. Symmetry : The order of the numbers doesn’t affect the output.
set <- c(1,2,3,4,5)
shuffledSet <- sample(set) # sample shuffles the vector 
c("AM Symmetry : ",AM(set) == AM(shuffledSet))
c("GM Symmetry : ",GM(set) == GM(shuffledSet))
c("HM Symmetry : ",HM(set) == HM(shuffledSet))

# 2. translation invariant - if we add a constant to every value 
# and take the average, then the output will change by that same constant.
set <- c(1,2,3,4,5)
increasedSet <- set + 3  
c("AM translation invariant : ",AM(set) + 3 == AM(increasedSet))
c("GM translation invariant : ",GM(set) + 3 == GM(increasedSet))
c("HM translation invariant : ",HM(set) + 3 == HM(increasedSet))

# 3. homogeneous - if we multiply all values by a constant, 
# the output will also change by the same factor.
set <- c(1,2,3,4,5)
increasedSet <- set * 5  
c("AM translation invariant : ",AM(set) * 5 == AM(increasedSet))
c("GM translation invariant : ",GM(set) * 5 == GM(increasedSet))
c("HM translation invariant : ",HM(set) * 5 == HM(increasedSet))

# 4. homogeneousidempotent - if all values are the same, 
# then the output will also be the same value. 
set <- c(2,2,2,2,2)
c("AM translation invariant : ",AM(set) == 2)
c("GM translation invariant : ",GM(set) == 2)
c("HM translation invariant : ",HM(set) == 2)




