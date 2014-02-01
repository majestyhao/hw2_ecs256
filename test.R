# simulate the event fliping coin for k times, p is the prob of head
bf<-function(k,p){
  nhead = 0
  bhead=0
  nonbhead=0
  for (i in 1:k){
    flip = sample(0:1,1,prob=c((1-p),p))
    nhead = nhead+flip
    nonbhead = nonbhead+flip
    if (flip == 1){
      bonus = sample(0:1,1,prob=c((1-p),p))
      nhead = nhead+bonus
      bhead = bhead+bonus
    }  
  }
  c(nhead,nonbhead,bhead)
}
#repeat the event for nreps times to calculate corr
bfsim<-function(k,p,nreps){
  nheadvec <- c()
  nonbheadvec <- c()
  for (i in 1:nreps){
    temp <- bf(k,p)
    nheadvec[i] <- temp[1]
    nonbheadvec[i] <- temp[2]
  }
  print(nheadvec)
  # cat("cov:")
  # print(cov(nheadvec,nonbheadvec))
  p <- cov(nheadvec,nonbheadvec)/((var(nheadvec))^(1/2)+(var(nonbheadvec))^(1/2)) 
  p
}
bfsim(10,0.5,10000)
