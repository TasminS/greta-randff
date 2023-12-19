library(greta)

N = 1000
x = runif(N,0,2)
x = x[order(x)]
y = sin(2*pi*x) + rnorm(N,0,.3)

k=10
mockdata = list(y=y,x=as.matrix(x, nrow = length(x)),n=length(y),k=k,bw=8,omega=rnorm(k))


scale <- sqrt(2./mockdata$n)

bw    <- normal(0, 1, truncation=c(0,Inf))
beta  <- normal(0, 1, dim = k)
beta2 <- normal(0, 1, dim = k)
sigma2<- normal(0, 1, truncation=c(0,Inf))

## construct RFFs
features    <- mockdata$x %*% t(mockdata$omega) * bw
cosfeatures <- cos(features) * scale
sinfeatures <- sin(features) * scale

fhat        <- cosfeatures %*% beta1 + sinfeatures %*% beta2

## likelihood
distribution(mockdata$y)          <- normal(fhat, sigma2)

m <- model(bw, beta1, beta2, sigma2)
# sampling
lmin  <- 15
draws <- greta::mcmc(m,sampler = hmc(Lmin = lmin, Lmax = lmin+5), 
                     n_samples = 250, warmup = 1000, chains = 20,
                     one_by_one = T)

min(coda::effectiveSize(draws))

library(bayesplot)
mcmc_trace(draws, facet_args = list(ncol = 3))