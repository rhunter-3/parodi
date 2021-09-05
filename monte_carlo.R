
#### LIBRARIES
library(MASS)
library(stats4)

#### INPUTS

N.sim = 10000 # number of simulated scenarios
lamda = 3 # poisson rate
logn.mu = 10.5 # mu parameter of lognormal
logn.sigma = 1.3 # sigma parameter of lognormal

#### FUNCTIONS

set.zeroes = function(x){
  multiplier = c(rep(1,x[1]),rep(0,length(x)-1-x[1]))
  x = x[-1]*multiplier
} # function that keeps the first rpois simulated loss amounts for each sim and discards the rest  

#### MONTE CARLO SIMULATION
claim.count = rpois(N.sim,lamda)
max.claim.count = max(claim.count)

indiv.losses = rlnorm(N.sim*max.claim.count, meanlog = logn.mu, sdlog = logn.sigma)
loss.amounts = matrix(indiv.losses,ncol = max.claim.count)
loss.matrix = cbind(claim.count,loss.amounts)
losses =t(apply(loss.matrix,1,set.zeroes))
agg.losses = apply(losses,1,sum)

#### RETENTION ANALYSIS
# Retention structure
EEL = 10000
AAD = 50000
Limit = 1000000
Aggr.Limit = 1000000

# Calculate retained losses
ret.losses = apply(losses,c(1,2),min,EEL)
ret.pa.noaad = apply(ret.losses,1,sum)
ret.pa.aad = apply(as.array(ret.pa.noaad),1,min,AAD) #can use pmin/pmax
aad.impact = ret.pa.noaad - ret.pa.aad

# Calculate losses ceded to first layer
ced.losses = losses - ret.losses
ced.losses = apply(ced.losses,c(1,2),min, Limit)
ced.pa = apply(ced.losses,1,sum)
ced.pa = ced.pa + aad.impact
ced.pa = apply(as.array(ced.pa),1,min,Aggr.Limit)
ced.pa == Aggr.Limit

#### OUTPUTS
# Percentiles
percs = c(0.25,0.5,0.75,0.8,0.85,0.90,0.95,0.98,0.99,0.995,0.999)

# Showing basic stats
Gross = c(quantile(agg.losses,probs = percs),mean = mean(agg.losses),"std dev" = sd(agg.losses))
Retained = c(quantile(ret.pa.aad,probs = percs),mean = mean(ret.pa.aad),"std dev" = sd(ret.pa.aad))
Ceded = c(quantile(ced.pa,probs = percs),mean = mean(ced.pa),"std dev" = sd(ced.pa))

results.table = data.frame(Gross, Retained, Ceded)

Gross = c(quantile(agg.losses,probs = percs), mean(agg.losses), sd(agg.losses))
Retained = c(quantile(ret.pa.aad,probs = percs), mean(ret.pa.aad), sd(ret.pa.aad))
Ceded = c(quantile(ced.pa,probs = percs), mean(ced.pa),sd(ced.pa))

Percentile = c(percs, "Mean", "SD")
results.table = data.frame(Percentile,Gross, Retained, Ceded)

