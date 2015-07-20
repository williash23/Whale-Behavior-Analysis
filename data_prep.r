		# Sara Williams
		# 7/16/2015
		# Assess whale_behavior before, during and after Closest Point of Approach (CPA)
		# Data prep script
#################################################################################
			#  Load packages
			require(stats)
			library(plyr)
			library(dplyr)

			#  Load data 
				temp <- read.csv("C:/Users/sara.williams/Documents/GitHub/Whale-Behavior-Analysis/data/Whale_Pts.csv")
				temp2 <- subset(temp, temp$whale_behavior=="BL-Blowing" | temp$whale_behavior=="DF-Dive-fluke-up" | temp$whale_behavior=="DN-Dive-no-fluke" | temp$whale_behavior=="LF-Lunge-feed" | temp$whale_behavior=="RE-Resting" | temp$whale_behavior=="SA-Surface-active")
				temp2$whale_behavior <- droplevels(temp2$whale_behavior)
			#  Prep data
				#  Only use observations where there is more than one observation for comparison of behaviors
				temp2 <- temp2[which(temp2$ObType=="MultiOb"),]
				#  Make a new whale_behavior category that is numeric and attach to dataframe
				#   1: transit type behaviors (blowing/ ive with no fluke, dive with fluke up) and 2: stationary type behaviors (lunge feed, resting, surface active)
				new_beh <- revalue(temp2$whale_behavior, c("BL-Blowing"=1, "DF-Dive-fluke-up"=1, "DN-Dive-no-fluke"=1, "LF-Lunge-feed"=2, "RE-Resting"=2, "SA-Surface-active"=2))
				x <- cbind(temp2,new_beh)
				y <- arrange(x, SwB_Wpt_ID, ObOrder_Time)
				
			#  Function to split data within each group of same whale observations into before, after, and at CPA						
				split.cpa <- function(x){
			
					nms <- c("before", "after", "closest")
			
					x[x > 0] <- 1
					x[x < 0] <- 2
					x[x == 0] <- 3
			
					out <- nms[x]
				out
			}
		
			z <- y %>%
				group_by(SwB_Wpt_ID) %>%
				mutate(pos = ObOrder_CPA[levels(CPA) == "Y"] - ObOrder_CPA,
						   ba = split.cpa(pos))	%>%
				as.data.frame(.)
				
			#  Create data frame for multistate model
				final <- z %>%
								dplyr::select(SwB_Wpt_ID, new_beh, pos, ba, ObOrder_Time, ObOrder_CPA)
			
			
			#  Data needed for multistate model input
			#   Can use data set up from Giminez et al (2012) if data are in wide format matrix.
			#   (an individual on each row and number of encounters is number of columns)
			
			#  Number of total observations (which equals total number of times a state was assessed for an individual)
			n_obs <- nrow(final)
				 
			#  Number of individuals 
			#   From Gimenez....
			# n <- dim(final)[[1]]  # number of rows in data 
			#   My attempt... 
			ind <- as.factor(final$SwB_Wpt_ID)
			ind <- as.numeric(ind)
			n_ind <- length(ind)
				 
			#  Number of capture occasions
			#   From Gimenez....
			# k <- dim(final)[[2]]  # number of columns in data 
			#   My attempt...n_occ is different for each individual whale
			t <- as.factor(final$ObOrder_Time)
			t <- as.numeric(t)
			n_occ <- rep(NA,12)	
						for(i in 1:n_ind){
							n_occ[i] <- max(t[i])
						}	
			
			#  Index for state (total number of times a state was assessed for an individual)
			#   From Gimenez....
			state_index <- matrix(NA,nrow=n,ncol=k)  ## from Gimenez
			#   My attempt...total number of times a state was assessed should be the same as the total number of observations.
			state_index <- seq(1:n_obs)
				
	
			#  Covariate --- Closest Point of Approach (CPA)
			#   Options are: before (1), closest (0), after(-1)
			cpa <- tapply(as.numeric(final$pos), state_index, unique)
				
				 
