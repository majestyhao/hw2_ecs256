sasim <- function(nreplace, rho, k) {
  replace <- 0 # number of receivers replaced so far
  up <- TRUE # receiver is up
  nbits <- 0 # number of bits received so far
  ncsec0 <- 0 # current number of consecutive 0s
  while (TRUE) {
    bit <- sample(0:1, 1)
    nbits <- nbits + 1
    if (runif(1) < rho) {
      up <- FALSE # receiver is down
      bit <- 0 # receive only '0' when broken
    }
    if (bit == 0) {
      ncsec0 = ncsec0 + 1
      if (ncsec0 == k) {
        replace = replace + 1
        ncsec0 <- 0
        up <- TRUE # replace the receiver
      }
    }
    if (replace == nreplace) break
  }
  return(nbits/nreplace) # calculate miu
}