target <- function(theta,n=20,ybar=0.0675){
  dens <- exp(-n*(theta - ybar)^2/2)*(1/(1+theta^2))
  return(dens)
}

proposal <- function(theta_new, theta_old){
  dens <- dcauchy(theta_new,0,1)
}

accept_prob <- function(theta_new,theta_old){
  alpha_mh <- (target(theta_new)*proposal(theta_old, theta_new))/(target(theta_old)*proposal(theta_new, theta_old))
  alpha_mh <- ifelse(alpha_mh>1,1,alpha_mh)
  return(alpha_mh)
}

MH_sampler <- function(theta_init, niter){
  theta_samples <- rep(0,niter)
  theta_samples[1] <- theta_init 
  for(i in 1:niter){
    theta_old = theta_samples[i]
    theta_new <- rcauchy(1)
    u <- runif(1)
    alpha_mh = accept_prob(theta_new,theta_old)
    if(u <= alpha_mh){
      theta_samples[i+1] = theta_new
    }else{
      theta_samples[i+1] = theta_old
    }
  }
  return(theta_samples) 
}

set.seed(453)
th.samp = MH_sampler(0,1e4)
mean(th.samp[5000:10000])
plot(th.samp,type="l")

setwd("C:/Users/Jyotishka/OneDrive/Documents/Course Notes/stat5443/R codes")
dev.copy2pdf(file="normal-cauchy.pdf")
dev.off()


set.seed(453)
th.samp = MH_sampler(0,1e4)

mean(th.samp[5000:10000])

par(mfrow=c(1,1))
#plot(th.samp,type="l",main="Trace Plot")
plot(acf(th.samp[5000:10000],plot=F), main = "Autocorrelation")
setwd("C:/Users/Jyotishka/OneDrive/Documents/Course Notes/stat5443/R codes")
dev.copy2pdf(file="normal-cauchy-2.pdf")
dev.off()