rm(list = ls())
binorm_gibbs<- function(theta_init, y, rho, niter){
  theta = matrix(0,2,niter)
  theta[1,] = theta_init
  for(i in 2:niter){
    theta[1,i] = rnorm(1,y[1]+rho*(theta[2,(i-1)]-y[2]),1-rho^2)
    theta[2,i] = rnorm(1,y[2]+rho*(theta[1,i]-y[1]),1-rho^2)
  }
  return(theta)
}

y = c(-1,1)
theta_init = c(0,0)
niter = 500
rho = 0.8

theta <- binorm_gibbs(theta_init,y,rho,niter)

plot(t(theta),type="l")
plot(t(theta),type="p")

### 

normal_gibbs<- function(y, niter){
  n = length(y)
  Ybar = mean(y)
  Sigma2 = var(y)
  
  mu = rep(0,niter)
  sigmasq = rep(1,niter)
  
  mu[1] = Ybar; sigmasq[1] = Sigma2
  
  for(i in 2:niter){
    tau = rgamma(1,shape=n/2,rate=0.5*sum((y-mu[i-1])^2))
    sigmasq[i] = 1/tau
    mu[i] = rnorm(1,Ybar, sigmasq[i]/n)
    cat(0.5*sum((y-mu[i-1])^2),sigmasq[i]/n,"\n")
  }
  return(list(mu=mu, sigmasq = sigmasq))
}

m = 2
s = 4
y = rnorm(100,m,s)

gibbs.fit <- normal_gibbs(y,niter = 1000)

plot(gibbs.fit$mu,type="l")
plot(gibbs.fit$sigmasq,type="l")
