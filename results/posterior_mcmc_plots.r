# Sara Williams
# 3/8/2017
# Assess whale_behavior within multistate models
# Generate plots from MCMC posterior distribution object (iterations)
#   and run in JAGS.
################################################################################

#  Load packages
library(ggplot2)
library(plyr)
library(dplyr)
library(packcircles)
library(ggmcmc)
library(rjags)
library(gridExtra)
library(grid)
library(ggthemes)
################################################################################

#  MCMC plots with package 'ggmcmc'
trans1 <- ggs(msm_fit, family = "^alpha")
trans2 <- ggs(msm_fit, family = "^beta")
trans3 <- ggs(msm_fit, family = "^mu")

dens_trans1 <- ggs_density(trans1, greek = TRUE) + 
                         theme_bw() +
                         theme(panel.grid.minor = element_blank(), 
                                     strip.text = element_text(size = rel(1.75)), 
                                     axis.text = element_text(size = rel(1.25)), 
                                     axis.title = element_blank(),
                                     legend.position="none")

dens_trans2 <- ggs_density(trans2, greek = TRUE) + 
                         theme_bw() +
                         theme(panel.grid.minor = element_blank(), 
                                     strip.text = element_text(size = rel(1.75)), 
                                     axis.text = element_text(size = rel(1.25)), 
                                     axis.title = element_blank(),
                                     legend.position="none")

dens_trans3 <- ggs_density(trans3, greek = TRUE) + 
                         theme_bw() +
                         theme(panel.grid.minor = element_blank(), 
                                     strip.text = element_text(size = rel(1.75)), 
                                     axis.text = element_text(size = rel(1.25)), 
                                     axis.title = element_blank(),
                                     legend.position="none")

grid.arrange(dens_trans1, dens_trans2, dens_trans3, ncol=3, nrow=1, 
                      left = textGrob("Density", rot = 90, gp=gpar(fontsize=16)))
