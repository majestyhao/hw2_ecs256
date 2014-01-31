verifyP0 <- function(n) {
  P <- c(rep(0, n))
  for (i in 1:n) {
    P[i] <- 0.5 * ncol(combn(n, i)) * 0.1^i * (1 - 0.1)^(n - i) + 0.5 * ncol(combn(n, i)) * 0.9^i *(1 - 0.9)^(n - i)
  }
  E <- sum(P * c(1:n))
  cat("EX: ")
  print(E)
}