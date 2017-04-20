# Sara Williams
# 3/8/2017
# Assess whale_behavior within multistate models
# Parameter plots
#################################################################################

#  Load packages
library(stringr)
library(dplyr)
library(tidyr)

#  Read in data using data_prep script


#  For scaled and centered dist to ship and bearing to ship, and transformed transition probability (not on real probability scale)
#   Mean parameter estimates 
alpha1 <- 0.571 # dist to ship
beta1 <- -0.276 # bear to ship
mu_psi1 <- 0.001 # transition prob from T to S
mu_psi2 <- 0.109 # transition prob from S to T
trans_mu_psi1 <- qlogis(mu_psi1) # untransformed transition prob from T to S
trans_mu_psi2 <- qlogis(mu_psi2) # untransformed transition prob from S to T
#################################################################################

#  Create credible intervals on plots using iterations from MCMC.

#   Load saved MCMC values as R objects for mean_psi1, mean_psi2, alpha1, and beta1
chain_1 <- msm_fit[[1]]
chain_2 <- msm_fit[[2]]
chain_3 <- msm_fit[[3]]
sims <- rbind(chain_1, chain_2, chain_3)
sim_reps_alpha1 <- as.numeric(sims[,1])
sim_reps_beta1 <-as.numeric(sims[,3])
sim_reps_mean_psi1 <- as.numeric(sims[,5])
sim_reps_mean_psi2 <- as.numeric(sims[,6])
#################################################################################

 #   Untransformed mean_phi parameter values for all iterations                    
iter_trans_psi1 <- qlogis(sim_reps_mean_psi1)
iter_trans_psi2<- qlogis(sim_reps_mean_psi2) # untransformed mean psi over all iteration reps
#  Give a name to the object for the iterations of alpha 1 and beta2
iter_alpha1 <- sim_reps_alpha1
iter_beta1 <- sim_reps_beta1

    quantile(sim_reps_alpha1, probs=c(0.05, 0.95))
    quantile(sim_reps_beta1, probs=c(0.05, 0.05))
    quantile(sim_reps_mean_psi1, probs=c(0.05, 0.95))
    quantile(sim_reps_mean_psi2, probs=c(0.05, 0.95))
#################################################################################

#  Create plot
#   Number of pulls out of MCMC iterations.
n <- 5000
keep <- sample(1:length(sim_reps_mean_psi1), n, replace = F)
par(mar=c(5,5,4,2))
#################################################################################

# Distance to ship plot
#  Get a single length for each individual, then calculate survival for mean_phi + 0*toe + beta2*length
whale1 <- as.data.frame(dist_to_ship) %>%
                  arrange(dist_to_ship) %>%
                 mutate(trans_TS = trans_mu_psi1 + alpha1 * dist_to_ship) %>%
                  as.data.frame(.)

plot(dist_to_ship, seq(0,1,length.out=length(dist_to_ship)), 
        type="n", 
        ylim=c(0.00, 0.10),
        xlim=c(-1.018, 4.4),
        xaxt="n",
        xlab = "Distance to ship (m)",
        ylab = "Transition probability", 
        cex.lab=1.5,
        cex.axis=1.5,
        bty="L")
axis(1, at=c(-0.7726323, -0.1278009, 0.5172472, 1.160016, 1.805335, 2.45015, 3.091, 3.7364, 4.375), 
        labels=c("1000", "2000", "3000", "4000", "5000", "6000", "7000", "8000", "9000"), cex.axis = 1.25)

for (j in 1:n){
    iter_state <- iter_trans_psi1[keep[j]] + iter_alpha1[keep[j]] * dist_to_ship #+ iter_beta1[keep[j]] * dist_to_shore
    lines(dist_to_ship, plogis(iter_state), col= grey(.9,.3))
  }

         
#  Add line for length and survival, scaled and centered
lines(whale1$dist_to_ship, plogis(whale1$ trans_TS), col="grey40", lwd = 2)
#################################################################################


#   Bearing to ship plot
whale2 <- as.data.frame(bear_to_ship) %>%
                  arrange(bear_to_ship) %>%
                  mutate(trans_TS = trans_mu_psi1 + beta1 * bear_to_ship) %>%
                  as.data.frame(.)

plot(bear_to_ship, seq(0,1,length.out=length(bear_to_ship)), 
        type="n", 
        ylim=c(0.00, 0.10),
        xlim=c(-1.175, 1.91),
        xaxt="n",
        #main = "",
        xlab = "Absolute relative bearing to ship (deg)",
        ylab = "Transition probability ", 
        cex.lab=1.5,
        cex.axis=1.25,
        bty="L")
axis(1, at=c(-1.019845, -0.432609, 0.154184, 0.7385072, 1.325919, 1.91),
          labels=c("15", "30", "45", "60", "75", "90"), cex.axis = 1.25)

for (j in 1:n){
    iter_state <- iter_trans_psi1[keep[j]] + iter_beta1[keep[j]] * bear_to_ship 
    lines(bear_to_ship, plogis(iter_state), col= grey(.9,.3))
  }
 
  
#  Add line for length and survival, scaled and centered
lines (whale2$bear_to_ship, plogis(whale2$ trans_TS), col="grey40", lwd = 2)
#################################################################################














# lines (lowess(plogis(frog$surv) ~ frog$scaled_len), col="red", lwd = 2)
# seq_len <- seq(min(frog$scaled_len), max(frog$scaled_len), length.out=5000)
# y <- trans + beta2*seq_len
# lines(seq_len, plogis(y), lwd = 1)