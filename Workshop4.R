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
label <- paste(label, percentages, sep = "-")
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

# Using external libraries
library(plotrix)
pie3D(
  slices,
  labels = label,
  col = rainbow(length(label)),
  # assign rainbow colours to the slices
  main = "Expenditure",
  clockwise = TRUE,
  explode = 0.2
)

# Saving plot as a file
png(filename = "Datasets/Outputs/pie_chart.png")
pie(slices, labels = label, main = "Expenditure")
dev.off() # Saves the file


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
colours <- c("green", "orange", "brown")
months <- c("Mar", "Apr", "May", "Jun", "Jul")
regions <- c("East", "West", "North")
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
dev.off()

# plotting Histogram
v <- c(9, 13, 21, 8, 36, 22, 12, 41, 31, 33, 19)
png(filename = "Datasets/Outputs/histogram.png")
hist(v,
     xlab = "weight",
     col = "red",
     border = "black")
dev.off()

# Line graphs
yAxis <- c(7, 12, 28, 3, 41)
png(filename = "Datasets/Outputs/line_graph.png")
plot(yAxis, type = "o") # x will start from 1 by deafult
dev.off()

## Transforming variables
volleyFilePath <- "Datasets/volley.txt"
volley <- read.table(volleyFilePath)
volley
original <- volley

volley[, 1] <-
  51.24 - volley[, 1] # subtract each entry in col1 from 51.24
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
