5 + 2

# Variables
a <- 10
b <- a + 1
the.value <- b
the.value
the.index <- 2
the.value ^ the.index

# arrays and vectors
vector <- c(4, 5, 6, 7, 8)
vector
reverse.vector <- 8:4
reverse.vector

arr <- array(c(3, 4, 5), 6)
arr

arr2 <- array(3:6, 10)
arr2

v1 <- c(1, 6, 7, 9)
v2 <- c(-1, 2, 1, -2)

v1 + v2
v1 - v2
v1 * v2
v1 / v2
v1 ^ v2

v3 <- c(1, 2, 3, 4, 5)
v1 ^ v3 # error: object length is not multiple of shorter object length

v4 <- 1:8
v1 - v4

# remove variables from global environment
rm(a)

# basic arthematic built-in functions
mean(1:6)
mean(v1)
median(1:6)

a <- c(1, 8, 3, 9)
b <- c(2, 2, 1, 1)
d <- c(3, 4, 6, 81, 9)

prod(a)
prod(a) ^ (1 / length(b))
216 ^ (1 / 4)
length(b)
sum(a * b)
sum(a ^ b)
max(a, d, v1 * v2)
min(max(a), max(b))


# functions
meanFunc <-  function(vec) {
  output <- sum(vec) / length(vec)
  output # this goes to the output stream like powershell and hence returned
}


meanFunc(1:6)
mean(1:6)
