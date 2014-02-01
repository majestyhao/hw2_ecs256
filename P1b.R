P1b <- function(n) {
  X <- c(rep(0, n))
  for (i in 1:n) {
    X[i] <- P1brx()
  }
  cat("EX: ")
  print(mean(X))
  cat("variance: ")
  print(var(X))
}

# 1 trail
P1brx <- function() {
  x <- sample(1:3, 1)
  if (x == 1)
    return(2)
  else {  
    if (x == 2)
      totTime <- 3
    else
      totTime <- 5
      while (TRUE) {
        x <- sample(1:3, 1)
        if (x == 1)
          return (totTime + 2)
        else if (x == 2)
          totTime <- totTime + 3
        else 
          totTime <- totTime + 5        
      }
  }
}