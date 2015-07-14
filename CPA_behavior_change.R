###################################################################################################
####### Script to group by same whale ID and to designate each relocation as before, at, or after CPA ##############
#######6.4.15 ######################################################################################

require(stats)
require(plyr)
require(dplyr)

#  Get data all situated
temp <- read.csv("C:/Users/sara.williams/Documents/GitHub/Remotely_Sensed_Data/data/Whale_Pts.csv")
head(temp)
table(temp$Behavior)

temp2 <- subset(temp, temp$Behavior=="BL-Blowing" | temp$Behavior=="DF-Dive-fluke-up" | temp$Behavior=="DN-Dive-no-fluke" | temp$Behavior=="LF-Lunge-feed" | temp$Behavior=="RE-Resting" | temp$Behavior=="SA-Surface-active")
temp2$Behavior <- droplevels(temp2$Behavior)

#  Only use observations where there is more than one observation for comparison of behaviors
temp2 <- temp2[which(temp2$ObType=="MultiOb"),]

#  Make a new behavior category that is numeric and attach
new_beh <- as.numeric(revalue(temp2$Behavior, c("BL-Blowing"=1, "DF-Dive-fluke-up"=2, "DN-Dive-no-fluke"=3, "LF-Lunge-feed"=4, "RE-Resting"=5, "SA-Surface-active"=6)))
table(new_beh)
x <- cbind(temp2,new_beh)
str(x)
head(x)


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
	mutate(pos = ObOrder[levels(CPA) == "Y"] - ObOrder,
               ba = split.cpa(pos))

#  Create smaller data frame with just pertinent information
vars <- c("EvB_Wpt_Id", "SwB_Wpt_ID", "ba", "pos", "new_beh", "Behavior", "Direction", "Orientation", "BB_MM_Dist", "MMDepth_m", "MMDstSh_m", "ObType" ,"CPA")
final <- final[vars]
final <- as.data.frame(final)
head(final)

#  Get number of times behaviors occured at each category
table(final$Behavior, final$CPA)
table(final$Behavior, final$ba)

#  Subset to each category
CPAs <- final[final$CPA=="Y",]
before
after

###################################################################################################			   
#  Used to trouble shoot code to create 'final' above --- found one erroneous group where there were 2 CPA's for one whale 			   
final <- x %>%
	group_by(SwB_Wpt_ID) %>%
	summarise(sum(CPA == "Y"))

colnames(final) <- c("id", "NumCPA")
	
t <- final[ which(final$NumCPA >1),] 	
t

Error was relocation: 2010-07-25-K-007 

#################################################################################################
final2 <- x %>%
	group_by(SwB_Wpt_ID) %>%
	summarise(sum(ObOrder2 > 3))

colnames(final2) <- c("id", "SumObOrder2")
	
locs2 <- final2[ which(final2$SumObOrder2 >0),] 	
locs2

