		# Sara Williams
		# 7/20/2015
		# Assess whale_behavior before, during and after Closest Point of Approach (CPA)
		# Call JAGS script
####################################################################################################
		#  Load packages
		require(R2jags)
		require(mcmcplots)

		#  Source data prep script
		source(file.path("C:/Users/sara.williams/Documents/GitHub/Whale-Behavior-Analysis/data_prep.R"))
		
		# Bundle data
		data <- list(n_ind=n_ind, 
						n_occ=n_occ, 
						enc=final, 
						state_index=state_index)
						
		inits <- function(){
			list("mu_phi" = runif(1, -1, 1),
				 "mu_p" =   runif(1, -1, 1), 
				 "beta1" =  runif(1, -5, 5),
				 "beta2" =  runif(1, -5, 5),
				 "beta3" =  runif(1, -5, 5),
				 "beta4" =  runif(1, -5, 5),
				 "z" =  z.init(n_ind=n_ind, first_occ=first_occ, n_prim=n_prim)) # Latent (true) state of individual i at time t
		}
		
	
#################################################################################
		#  Call JAGS
		call_jags <- function(mod_name, 
								parallel = F,
								ni = 100,
								nt = 1,
								nb = 10,
								nc = 3,
								debug_mode = T,
								return_fit = T){
				
				{out <- try(jags(data = data, 
								inits = inits, 
								parameters.to.save = parameters,
								model.file = paste("C:/Users/sara.williams/Documents/GitHub/Whale-Behavior-Analysis/models/",
													mod_name,
													".txt", 
													sep=""),
								n.chains = nc, 
								n.thin = nt, 
								n.iter = ni, 
								n.burnin = nb), silent = T)
								
				# Save output of JAGS run
				if(class(out) != "try-error"){
					save(out, file = file.path("C:/Users/sara.williams/Documents/GitHub/Whale-Behavior-Analysis/results/"
												paste(sara.williams, 
														"_", 
														mod_name,
														"_",
														format(as.POSIXlt(Sys.time()), 
																"%d%b%Y_%H%M%S"), 
														".RData", sep = "")))
						}
				}
			
			if(return_fit){
				return(out)
			}else{
				return(NULL)
			}
		}