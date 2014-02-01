P1aSiumulation <- function(n, k) {
  X <- c(rep(0, n))
  # total <- 0
  # n trails
  for (i in 1:n) {
    X[i] <- P1rxi(k)
    # total <- total + X[k]
  }
  cat("EX: ")
  # print(total/n)
  print(mean(X))
  cat("variance: ")
  print(var(X)) # should be 11/16 k
}

# one trail
P1rxi <- function(k) {
  nHead <- 0 # nonbonus head num
  bHead <- 0 # heads of bonus
  for (i in 1:k) {
    flip <- sample(0:1, 1) # nonbonus flip
    if (flip == 1) {
      nHead <- nHead + 1
      bflip <- sample(0:1, 1) # bonous flip
      # while (bflip == 1) {
      if (bflip == 1) {
        bHead <- bHead + 1
        # bflip <- sample(0:1, 1)
      }
    }
  }
  totHead <- nHead + bHead
}