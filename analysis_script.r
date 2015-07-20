		# Sara Williams
		# 7/20/2015
		# Assess whale_behavior before, during and after Closest Point of Approach (CPA)
		# Analysis script
####################################################################################################
	
		#  Source prep scripts
		source(file.path("C:/Users/sara.williams/Documents/GitHub/Whale-Behavior-Analysis/call_jags.R"))
	
		# Call models
		mod_name <- ms_null
		parameters <- c("")
		fit <- call_jags("",
					ni = 30000,
					nt = 2,
					nb = 15000,
					nc = 3,
					debug_mode = F,
					return_fit = T)		
			fit
			mcmcplot(fit)
		
	