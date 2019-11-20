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


# custom functions

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

# custom functions end
mean(arr)
my.mean(arr)
my.geometricMean(arr)
my.harmonicMean(arr)
my.harmonicMean2(arr)
