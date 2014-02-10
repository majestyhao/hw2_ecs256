P <- matrix(c(1/2, 1/2, 0, 1/3, 1/3, 1/3, 0, 1/2, 1/2), nrow = 3, ncol = 3, byrow = TRUE)

findpi1 <- function(p) {
  n <- nrow(p)
  imp <- diag(n) - t(p) # I - P', left side
  rhs <- c(rep(0, n - 1), 1) # form right-hand-side vector
  pivec <- solve(imp, rhs)
  return(pivec)
}

pi1 <- findpi1(P)
pi1