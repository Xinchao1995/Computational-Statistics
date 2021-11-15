set.seed(135)

M <- 2000
z <- rexp(M,1)
y <- (sqrt(1+2*z)-1)^2

objfun <- function(y){
  return((1+sqrt(y))/2/sqrt(y)*exp(-(y/2+sqrt(y))))
}

hist(y, breaks=30, freq=F, col=rgb(1,0.1,0,0.5),cex.axis=1.5,cex.lab=1.5) # Histogram of the samples
curve(objfun, add = T,lw=5,col=rgb(0, 1, 0)) ##Plotting the density function to verify
#plot(densityblues9

estimated_mean <- sum(log(y))/M
estimated_stdevi <- sqrt(var(log(y)))
leftBound <- estimated_mean - 1.96*estimated_stdevi/sqrt(M)
rightBound <- estimated_mean + 1.96*estimated_stdevi/sqrt(M)