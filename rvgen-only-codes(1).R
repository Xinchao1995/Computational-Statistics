## First example: Logistic random variable generation

set.seed(123)
logistic_rvgen <- function(lamb,n=1){
  # Recall the signature for runif(n,high,low)
  u=runif(n)
  x=-log((1/u)-1)/lamb
  return(x)
}

lamb=0.1
n=10000
x=logistic_rvgen(lamb, n)
hist(x)
lines(density(x))

y=seq(-50,50,0.5)
fy=lamb*exp(-lamb*y)/((1.0+exp(-lamb*y))^2)
hist(x, freq  = F)
lines(y,fy, col = "red")

## Second example - Exponential random variable generation 

n = 1e3
lambda = 2
U = runif(n)
X = - 1/lambda*log(U)
hist(X, breaks = 30, freq=F)

Y=seq(0,50,0.5)
fy=lambda*exp(-lambda*Y)
hist(X, breaks = 30, freq  = F)
lines(Y,fy, col = "red")

## Third example - Box-Mueller transformation 

u1 = runif(n)
u2 = runif(n)
x1 = sqrt(-2*log(u1))*cos(2*pi*u2)
x2 = sqrt(-2*log(u1))*sin(2*pi*u2)
x = c(x1,x2)

hist(x,breaks = 30, freq = F)
curve(dnorm,add=T)

qqnorm(x)
qqline(x)

ks.test(x,pnorm)

## Fourth Example - Uniform Disk

R = runif(1e4,0,1)
theta = 2*pi*runif(1e3,0,1)
X = R*cos(theta);Y = R*sin(theta) # This won't work 
plot(X,Y, pch = 20, col=rgb(1,0,0,0.2)) #Not uniform 

X = sqrt(R)*cos(theta);Y = sqrt(R)*sin(theta)
plot(X,Y,pch = 20, col=rgb(1,0,0,0.2))

## Same problem - different method - accept-reject 

arfunction <- function(n){
  rvx = rep(0,n)
  rvy = rep(0,n)
  a = 0
  for (i in 1:n){
    x = runif(1,min = -1,max = 1)
    y = runif(1,min = -1, max = 1)
    if (x^2 + y^2 <= 1){
      rvx[i] = x
      rvy[i] = y 
      a = a + 1
    }
  }
  cat("Efficiency is ", a/n)  
  plot(rvx,rvy)
  title("Uniform on a disc")
}

n = 5000; arfunction(n)

## Now your turn



