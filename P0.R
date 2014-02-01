P0Siumulation <- function(n, i) {
  x <- c(rep(0, n))
  total <- 0
  # n trails
  for (k in 1:n) {
    x[k] <- rxi(i)
    total <- total + x[k]
  }
  cat("EX: ")
  print(total/n)
  print(mean(x))
}

# one trail
rxi <- function(i) {
  # flipe a coin
  m <- sample(0:1, 1) # uniform sample
  # assign p when the coin is tail
  p <- if (m == 0) 0.1 else 0.9
  # i subtrails during each trail
  rbinom(1, i, p) # genearte a RV that counts the num of 
}