P2bSiumulation <- function(n, k) {
  head <- matrix(rep(0, 2 * n), nrow = 2, ncol = n, byrow = TRUE)
  # total <- 0
  # n trails
  for (i in 1:n) {
    head[, i] <- P2bxi(k)
    # total <- total + X[k]
  }
  print(head)
  print(cov(head[2,], head[1, ]))
  rho <- cov(head[2, ], head[1, ])/((var(head[2, ]))^(1/2)+(var(head[1, ]))^(1/2))
  cat("rho: ")
  # print(total/n)
  print(rho)
}

# one trail
P2bxi <- function(k) {
  nHead <- 0 # nonbonus head num, Y
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
  totHead <- nHead + bHead # X
  return(c(nHead, totHead))  
}

P2bSiumulation(1000, 10) # n must be enough large