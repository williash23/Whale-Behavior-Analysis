# Sara Williams
# 6/4/2015
#  Asses whale_behavior before, during and after Closest Point of Approach (CPA)
################################################################################

#  Load packages
require(stats)
require(plyr)
require(dplyr)

#  Load data
temp <- read.csv("C:/Users/sara.williams/Documents/GitHub/Whale-Behavior-Analysis/data/Whale_Pts.csv")
head(temp)
table(temp$whale_behavior)

temp2 <- subset(temp, temp$whale_behavior=="BL-Blowing" | temp$whale_behavior=="DF-Dive-fluke-up" | temp$whale_behavior=="DN-Dive-no-fluke" | temp$whale_behavior=="LF-Lunge-feed" | temp$whale_behavior=="RE-Resting" | temp$whale_behavior=="SA-Surface-active")
temp2$whale_behavior <- droplevels(temp2$whale_behavior)

#  Only use observations where there is more than one observation for comparison of behaviors
temp2 <- temp2[which(temp2$ObType=="MultiOb"),]

#  Make a new whale_behavior category that is numeric and attach to dataframe
#   1: transit type behaviors (blowing/ ive with no fluke, dive with fluke up) and 2: stationary type behaviors (lunge feed, resting, surface active)
new_beh <- revalue(temp2$whale_behavior, c("BL-Blowing"=1, "DF-Dive-fluke-up"=1, "DN-Dive-no-fluke"=1, "LF-Lunge-feed"=2, "RE-Resting"=2, "SA-Surface-active"=2))
x <- cbind(temp2,new_beh)



#  Function to split data within each group of same whale observations into before, after, and at CPA						
		split.cpa <- function(x){
			
			nms <- c("before", "after", "closest")
			
			x[x > 0] <- 1
			x[x < 0] <- 2
			x[x == 0] <- 3
			
			out <- nms[x]
		out
		}
		
final <- x %>%
	group_by(SwB_Wpt_ID) %>%
	mutate(pos = ObOrder_CPA[levels(CPA) == "Y"] - ObOrder_CPA,
               ba = split.cpa(pos))

#  Get number of times behaviors occurred at each category
table(final$whale_behavior, final$CPA)
table(final$whale_behavior, final$ba)

table(final$new_beh, final$CPA)
table(final$new_beh, final$ba)


###################################################################################################
###################################################################################################			   			   
#  Used to trouble shoot code to create 'final' above --- found one erroneous group where there were 2 CPA's for one whale 			   
final <- x %>%
	group_by(SwB_Wpt_ID) %>%
	summarise(sum(CPA == "Y"))

colnames(final) <- c("id", "NumCPA")
	
t <- final[ which(final$NumCPA >1),] 	
t

Error was relocation: 2010-07-25-K-007 
