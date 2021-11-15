## Monte Carlo Integration
## Source for both examples: Jason Goldstick's lecture notes.


## Example 1: Arbitrary Integral
set.seed(1984)
# underlying uniforms
U <- runif(100000, 0, 2*pi)
# calculate h(U)
hU <- sin(U*cos(U))
# monte carlo approximation
2*pi*mean(hU)

# R's numerical integration approximation
h <- function(u) sin(u*cos(u))
integrate(h, 0, 2*pi)$val


## Example 2 : Logistic Regression 

# underlying normal variables
Z = rnorm(1000, mean=1.5, sd=sqrt(1.43))
# the transformed variables
P = 1/(1 + exp(-Z))
# sample mean
p = mean(P)
# sample variance
var_p = (n-1)*var(P)/n
# multiplier
z <- qnorm(1-(.05/2))
c(p - z*sqrt(var_p/1000), p + z*sqrt(var_p/1000))

