# solve for Pi
# P is trasition matrix
findPi <- function(P) {
  n <- nrow(P)
  imp <- diag(n) - t(P) # I - P'
  imp[n, ] <- rep(1, n) # sum(Pi_i) = 1
  rhs <- c(rep(0, n - 1), 1) # right-hand side 
  pivec <- solve(imp, rhs) # solve Pi
  return(pivec)  
}

mccor <- function(tm, k) {
  length <- nrow(tm)
  Pi <- findPi(tm)
  Pacc <- diag(length)
  avr <- t(c(1: length)) %*% Pi # EXi
  var <- t(c(1: length) ^ 2) %*% Pi - avr ^ 2 # var(Xi)
  corr <- rep(0, k)
  for (i in 1:k) {
    Pacc <- Pacc %*% P # i-step transition matrix
    s <- 0
    for (m in 1: length) {
      for (n in 1: length) {
        s <- s + Pi[m] * Pacc[m, n] * m * n
      }
    }
    corr[i] <- (s - avr * avr)/var # calculate rho
  }
  return(corr)
}

P <- rbind(c(0, 1, 0, 0, 0), c(1/3, 1/3, 1/3, 0, 0), c(0, 1/3, 1/3, 1/3, 0), c(0, 0, 1/3, 1/3, 1/3), c(0, 0, 0, 1, 0))
k <- 20
corr <- mccor(P, k)
print(corr)

