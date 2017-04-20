# Sara Williams
# 7/20/2015; updated 11/15/2016; 3/1/2017
# Assess changes in visually assigned whale behavior
# Analysis script
################################################################################

#  Load packages
library(rjags)
library(mcmcplots)
library(coda)
#  Load "glm" module for JAGS
load.module("glm")
load.module("dic")

#   MCMC settings
nc <- 3
ni <- 10000
nb <- 500
nt <- 2
na <- 1000

#   Bundle data
jags.dat <- list(eh = eh, ind = ind, occ = occ, n_obs = n_obs, n_ind = n_ind, n_occ = n_occ, 
                         dist_to_ship = dist_to_ship, bear_to_ship = bear_to_ship)
 
#   Inits function
inits <- function(){list(mu = runif(2, 0, 1),
                                      alpha = runif(2, -5, 5),
                                      beta = runif(2, -5, 5))
                                      }

#   Parameters to monitor
params <- c("mu", "alpha", "beta") 

#  Initialize model and go through adaptation 
msm <- jags.model(data = jags.dat,
                                  file = "C:/Users/sara.williams/Documents/GitHub/Whale-Behavior-Analysis/models/msm.txt", 
                                  inits = inits, 
                                  n.chains = nc, 
                                  n.adapt = na)

#  Burnin
update(msm, n.iter = nb)

#  Sample posterior
msm_fit <- coda.samples(msm,
                                            variable.names= params, 
                                            n.iter = ni, 
                                            thin = nt)


mcmcplot(msm_fit)


 save(msm_fit, file = "C:/Users/sara.williams/Documents/GitHub/Whale-Behavior-Analysis/results/msm_fit.RData")
 load("C:/Users/sara.williams/Documents/GitHub/Whale-Behavior-Analysis/results/msm_fit.RData")