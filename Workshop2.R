arr <- 1:10

# built in functions
mean(arr)
median(arr)
length(arr)
min(arr)
prod(arr)
sum(arr)
sum(arr ^ 2) # squares every element and then sums them

arr2 <- c(5, 4, 3, 2, 1)
arr + arr2
arr * arr2
sum(arr * arr2)
prod(arr) ^ (1 / length(arr2))


# custom functions ==========

my.mean <- function(arr) {
  output <-  sum(arr) / length(arr)
  output
}

my.geometricMean <- function (arr) {
  output <- prod(arr) ^ (1 / length(arr))
  output
}

my.harmonicMean <- function(arr) {
  output <-  length(arr) / sum(arr ^ (-1))
  output
}

my.harmonicMean2 <- function(arr) {
  output <- length(arr) / sum(1 / arr)
  output
}

# conditions
my.arrayLength <- function(arr) {
  if (length(arr) >= 10) {
    "array is larger than 10"
  }
  else if (length(arr) >= 5) {
    "array is larger than 5"
  }
  else {
    "array is less than 5"
  }
}

# custom functions end ===========

mean(arr)
my.mean(arr)
my.geometricMean(arr)
my.harmonicMean(arr)
my.harmonicMean2(arr)


my.harmonicMean(c(9, 13, 2))
my.geometricMean(c(9, 13, 2))

# rounding results
x <- c(0.6, 0.9, 0.26, 0.7)
round(my.geometricMean(x), 3)
round(my.mean(x), 3)
round(my.harmonicMean(x), 3)


# misc
table(arr)
unique(c(1, 2, 3, 3))

# conditions
my.arrayLength(arr)
my.arrayLength(c(1, 2, 3, 4))


# For loop
for (a in arr) {
  print(a)
}

for (i in 1:11) {
  print(arr[i])
}
arr[0]
arr[1]
