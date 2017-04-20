# Sara Williams
# 7/16/2015
# Assess whale_behavior within multistate models
# Data prep script
#################################################################################

#  Load packages
require(stats)
library(plyr)
library(dplyr)
library(tidyr)

#  Load data 
tmp <- read.csv("C:/Users/sara.williams/Documents/GitHub/Whale-Detection-Probability-Analysis/data/Whales_0615_general_clean.csv")

#tmp <- read.csv("C:/Users/sara.williams/Documents/GitHub/Whale-Behavior-Analysis/data/Whale_Pts.csv")

#  Prep data
tmp2 <- subset(tmp, tmp$whale_behavior=="BL-Blowing" | tmp$whale_behavior=="DF-Dive-fluke-up" | tmp$whale_behavior=="DN-Dive-no-fluke" | tmp$whale_behavior=="LF-Lunge-feed" | tmp$whale_behavior=="RE-Resting" | tmp$whale_behavior=="SA-Surface-active")
tmp2$whale_behavior <- droplevels(tmp2$whale_behavior)
tmp3 <- arrange(tmp2, same_whale_ID, ob_order_time)
tmp4 <-tmp3[!is.na(tmp3$ship_whale_dist),]

#  Make a new whale_behavior category that is numeric and attach to dataframe
#   1: transit type behaviors (blowing/ ive with no fluke, dive with fluke up) and 2: stationary type behaviors (lunge feed, resting, surface active)
beh_cat <- revalue(tmp4$whale_behavior, c("BL-Blowing"=1, "DF-Dive-fluke-up"=1, "DN-Dive-no-fluke"=1, "LF-Lunge-feed"=2, "RE-Resting"=2, "SA-Surface-active"=2))
tmp5 <- cbind(tmp4, beh_cat)

#  Create data frame for multistate model
tmp6 <- tmp5 %>%
              dplyr::select(same_whale_ID, ob_order_time, beh_cat, ship_whale_dist, 
              ship_whale_bearing, whale_dist_shore_m, count) %>%
              group_by(same_whale_ID) %>%
              dplyr::mutate(occ_num = seq(1:n())) %>%
              dplyr::mutate(max_occ = max(occ_num)) %>%
              filter(max_occ > 1) %>% # use this line if need more than 2 sightings
              ungroup(.) %>%
              as.data.frame(.)
group_number = (function(){i = 0; function() i <<- i+1 })()
dat <- tmp6 %>%
           group_by(same_whale_ID) %>%
           dplyr::mutate(new_id = group_number()) %>%
           dplyr::mutate(next_beh = lead(beh_cat)) %>%
           dplyr::mutate(TT = ifelse(next_beh == 1 & beh_cat == 1, 1, 0)) %>%
           dplyr::mutate(TS = ifelse(next_beh == 1 & beh_cat == 2, 1, 0)) %>%
           dplyr::mutate(SS = ifelse(next_beh == 2 & beh_cat == 2, 1, 0)) %>%
           dplyr::mutate(ST = ifelse(next_beh == 2 & beh_cat == 1, 1, 0)) %>%
           ungroup(.) %>%
           as.data.frame(.)
dat$occ_num <- factor(dat$occ_num)

#  Data needed for multistate model input
#   All data inputs except "eh" are long format, "eh" is wide format
#   Individuals and occasions
ind <- as.numeric(as.factor(dat$new_id))
occ <- as.numeric(dat$occ_num)
n_obs <- nrow(dat)
n_ind <- length(unique(dat$new_id))
n_occ_tmp <- dat%>%
                        group_by(new_id) %>%
                        slice(1) %>%
                        ungroup(.) %>%
                        dplyr::select(max_occ) %>%
                        as.data.frame(.)
n_occ <- as.numeric(n_occ_tmp$max_occ)
#   Encounter history
eh <- dat %>%
          dplyr::select(new_id, beh_cat, occ_num) %>%
          spread(occ_num, beh_cat)
eh <- eh %>%
          dplyr::select(2:13) %>%
          mutate_each(funs(as.numeric(as.character(.))))

#eh <- as.numeric(dat$beh_cat) # long format of eh

#   Covariates
dist_to_ship <- as.numeric(scale(dat$ship_whale_dist))
dist_to_shore <- as.numeric(scale(dat$whale_dist_shore_m)) 
bear_to_ship <- as.numeric(scale(abs(dat$ship_whale_bearing)))









# #  Data for simple logistic regression of stationary whales
# dat_first_row <- dat %>%
                                   # group_by(same_whale_ID) %>%
                                   # slice(1) %>%
                                   # filter(beh_cat == 2) %>%
                                   # as.data.frame(.)
# dat_stay_s <- dat_first_row %>%
                       # filter(next_beh == 2) %>%
                       # mutate(switch_state = "0")
# dat_switch_t <- dat_first_row %>%
                           # filter(next_beh == 1) %>%
                           # mutate(switch_state = "1")
# dat_switch <- bind_rows(dat_stay_s, dat_switch_t)
# dat_switch$switch_state <- as.numeric(dat_switch$switch_state)

# y <- dat_switch$switch_state
# ship_dist <- as.numeric(scale(dat_switch$ship_whale_dist))
# ship_bear <- as.numeric(scale(abs(dat_switch$ship_whale_bearing)))
# ship_cpa <- as.numeric(scale(abs(dat_switch$ob_order_CPA)))
# mod1 <- glm(y ~ ship_dist + ship_bear + ship_cpa, family = binomial)


# # dat_log <- semi_join(dat, dat_log_first_row, by = "same_whale_ID")
# # dat_log_ST <- filter(dat_log, ST == 1)
# # dat_log_SS <- filter(dat_log, SS == 1)














# get.last <- function(x) max(which(x != 0))
# l <- apply(eh_mat, 1, get.last)

# z <- matrix(1, nrow = n_ind, ncol = max(n_occ))
# for(i in 1:n_ind){
        # z[i, (l[i]:12)] <- NA
      # }




# #  The following code is used to make data wide instead of long
# # shore_dist <- dat %>%
                    # # dplyr::select(SwB_Wpt_ID, ship_dist_to_shore_m, occ_num) %>%
                    # # spread(occ_num, ship_dist_to_shore_m) %>%
                    # # dplyr::select(2:13)
# # ship_dist <- dat %>%
                    # # dplyr::select(SwB_Wpt_ID, ship_bb_to_whale_dist, occ_num) %>%
                    # # spread(occ_num, ship_bb_to_whale_dist) %>%
                    # # dplyr::select(2:13)
# # cpa <- dat %>%
           # # dplyr::select(SwB_Wpt_ID, ObOrder_CPA, occ_num) %>%
           # # spread(occ_num, ObOrder_CPA) %>%
           # # dplyr::select(2:13)


# eh_mat[,1] <- as.numeric(eh_mat[,1])
# eh_mat[,2] <- as.numeric(eh_mat[,2])
# eh_mat[,3] <- as.numeric(eh_mat[,3])
# eh_mat[,4] <- as.numeric(eh_mat[,4])
# eh_mat[,5] <- as.numeric(eh_mat[,5])
# eh_mat[,6] <- as.numeric(eh_mat[,6])
# eh_mat[,7] <- as.numeric(eh_mat[,7])
# eh_mat[,8] <- as.numeric(eh_mat[,8])
# eh_mat[,9] <- as.numeric(eh_mat[,9])
# eh_mat[,10] <- as.numeric(eh_mat[,10])
# eh_mat[,11] <- as.numeric(eh_mat[,11])
# eh_mat[,12] <- as.numeric(eh_mat[,12])
# eh_mat[is.na(eh_mat)] <- 0


# #  Function to split data within each group of same whale observations into before, after, and at CPA            
  # split.cpa <- function(x){

    # nms <- c("before", "after", "closest")

    # x[x > 0] <- 1
    # x[x < 0] <- 2
    # x[x == 0] <- 3

    # out <- nms[x]
  # out
# }

# tmp6 <- tmp5 %>%
              # group_by(same_whale_ID) %>%
              # mutate(pos = ob_order_CPA[levels( ob_order_CPA) == "Y"] - ob_order_CPA,
                            # ba = split.cpa(pos))  %>%
              # as.data.frame(.)