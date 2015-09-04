		# Sara Williams
		# 8/6/2015
		# Assess changes in whale movement mode based on changing distance to ship
		# Based on formulation from Servanty and Gimenez 2010
		# Model script
################################################################################# 
 
 sink("ms_Servanty.txt")
cat("
      model{
 #  Parameters
	   #   phiT: probability of being in transit (initially?) 
	   #   phiS: probability of being stationary (initially?) 
	   #   psiTS: transition probability transit to stationary
	   #   psiST: transition probability stationary to transit
				
				#  Detection probability is also often estimated in MS models.
				#   But, since I have no "not seen" events/observations, 
				#   there is no detection probability associated with my data. 
				#    If there was a detection probability, it may be assigned:
				#   pT: detection prob. of individuals in transit mode
				#   pS: detection prob. of individuals in stationary mode
				
		     
	    #  States
        #   1 = in transit mode
        #   2 = in stationary mode
		
				???????????????????????????????????????????????????????????????????????
				#  Uncertainty in state assignment ??? Create "artificial" uncertainty within data?
				#   Still only 2 states, but 4 possible events:
				#   1 = seen in transit and assigned in transit (correctly)
				#   2 = seen in transit and assigned in stationary (incorrectly)
				#   3 = seen in stationary and assigned in stationary (correctly)
				#   4 = seen in stationary and assigned in transit (incorrectly)
				???????????????????????????????????????????????????????????????????????
				
		#  Define state-transition matrix
		#   An observation matrix is also usually defined in MS models.
		#   But, since I have no "not seen" events/observations, 
		#   I have no need for a observation matrix than is different
		#   than the state matrix. Perhaps this is where I can incorporate
		#   state assignment uncertainty that is estimated in a separate model?
			
			#  STATE Indices
			#   First index = state at time t-1 (departure state)
			#   Second index time (t)
			#   Third index = state at time t (arrival state)
		
				#  Define probabilities of state S(t+1) given S(t)
				#    using the generalized logit link function.
				for(t in 1:(n_occ-1)){			#  Loop over sightings
					ps[1, t,1] <- phi[t,1]]* (1-(psi[t,1]/sum(psi[t,])))		#  Probability of being in state T, then staying in state T
					ps[1, t,2] <- phi[t,1]]* (psi[t,1]/sum(psi[t,]))				#  Probability of being in state T, then transitioning to state S
					ps[2, t,1] <- phi[t,2]]* (psi[t,2]/sum(psi[t,]))				#  Probability of being in state S, then transitioning to state T
					ps[2, t,2] <- phi[t,2]]* (1-(psi[t,2]/sum(psi[t,])))		#  Probability of being in state S, then staying in state S
				}
				
				#  State equation
				for(i in 1:n_ind){  		#  Loop over individuals
					for(t in 2:n_occ[i]){  			#  Loop over sightings
						y[i,t] ~ dcat(ps[y[i,t-1],i,t-1,1:2])
					} 
				} 

		#  Initial states (phi) and transitions (psi)
		for (t in 1:(n_occ-1){
				phi[1] <- beta1[1] + beta2 [1]* dist_to_shore[t]			#  phiT (initially in transit)
				phi[2] <- beta1[2] + beta2[2] * dist_to_shore[t]			#  phiS (intially in stationary)
				
				psi[1] <- alpha1[1] + alpha2[1] * dist_to_ship[t] 		#  psiTS (Transition of transit to stationary);
																														#   so psiTT = 1-psi[1]
				psi[2] <- alpha1[2] + alpha2[2] * dist_to_ship[t]		#  psiST (Transition of stationary to transit);
																														#   so psiSS = 1-psi[2]
			}
		
		#  Priors and constraints
		for(u in 1:2){
			beta1[u] ~ dnorm(0,0.1)		#  Prior for initially being in transit or stationary mode
			beta2[u] ~ dnorm(0,0.1)		#  Prior for effect of covariate on initial state
			alpha1[u] ~ dnorm(0,0.1)		#  Prior for transitions
			alpha2[u] ~ dnorm(0,0.1)		#  Prior for effect of covariate on transitions
		}
					???????????????????????????????????????????????????????????????????????
					#  Coefficients for the baseline category constrained to zero?????
					???????????????????????????????????????????????????????????????????????
      	}  
		
      ",fill=TRUE)
      sink()