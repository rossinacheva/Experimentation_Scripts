#Loading libraries
wants <- c("MatchIt", "dplyr", "ggplot2", "ROCR", "Zelig", "jtools")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
lapply(wants, library, character.only=T)

options(repr.matrix.max.cols=100, repr.matrix.max.rows=100)




#read data

all <- read.csv("/Users/r.nacheva/Downloads/Experimentation_RadiusTest_CO_beforeAfter_before_forMatching_2021_08_04.csv")


head(all)


# Turn back to numeric values
col_factor <- sapply(all, is.factor)
all[col_factor] <- lapply(all[col_factor], as.character)
num_cols <- sapply(all, is.numeric)
all[,num_cols][is.na(all[,num_cols])] <- 0



#narrow down data scope

all_cg = all[all$variant =='Control' ,]


all_tg1 =all[all$variant =='Treatment',]
all_tg = all_tg1[sample(1:nrow(all_tg1), 0.95*nrow(all_tg1)),]
  
  


summary(all_tg)




#relevel

dt_50 = rbind(all_cg,all_tg)
dt_50$variant = as.character(dt_50$variant)
dt_50$variant[dt_50$variant=='Treatment'] <- 1
dt_50$variant[dt_50$variant=='Control'] <- 0
dt_50$variant <- as.factor(dt_50$variant)

##matching balance improvement steps were omitted, and only the final models are listed

#########
#overall#
#########

names(dt_50)

#matching
match_52_4 <- matchit(variant ~ Rides_driver +Supply_hours+Active_hours+active_days_period+driver_earning+
                        pax_can+Rides+Revenue+acc+acc_won+ignored+broadcast+eta+acc_upto_1km      
+acc_upto_2km+acc_upto_3km+acc_above_3km+ignored_upto_1km+ignored_upto_2km  
+ignored_upto_3km+ignored_above_3km+request_upto_1km+request_upto_2km+request_upto_3km  
+request_above_3km+eta_upto_1km
               , method="nearest", data = dt_50, distance = 'cauchit')


summary(match_52_4)
plot(summary(match_52_4, standardize=TRUE), xlim(0,1.6))
plot(match_52_4,  type = "hist", xlim(c(0:1.6)))
dt_overall = match.data(match_52_4)

write.table(dt_overall,"/Users/r.nacheva/Downloads/Matched_CO_Radius.csv", sep=',', dec='.',row.names=F)



quntiles_cinfidence_level = c()
mean_diff = c()
kpi = c()



#estimation acceptance rate
set.seed(000)
z.out <- zelig(acceptance_rate ~ Segment.x + Supply_hours_before +Active_hours_before +Rides_before + Revenue_before +ride_total_distance_before
               + avg_fare_before + acceptance_rate_before + acc_before +req_before
               , data = dt_overall, model='ls', cite=FALSE)

att_all <- ATT(z.out, treatment='Segment.x', treated=1)
att_all <- get_qi(att_all, qi='ATT', xvalue = "TE")
quantile(att_all, c(0.025, 0.975))
mean(att_all)


quntiles_cinfidence_level_a = quantile(att_all, c(0.025, 0.975))
mean_diff_a = mean(att_all)
kpi_a = 'acceptance_rate'

quntiles_cinfidence_level = rbind(quntiles_cinfidence_level,quntiles_cinfidence_level_a)
mean_diff = rbind(mean_diff,mean_diff_a)
kpi = rbind(kpi,kpi_a)



#estimation revenue
set.seed(000)
z.out <- zelig(Revenue ~ Segment.x + Supply_hours_before +Active_hours_before +Rides_before + Revenue_before +ride_total_distance_before
               + avg_fare_before + acceptance_rate_before + acc_before +req_before
               , data = dt_overall, model='ls', cite=FALSE)

att_all <- ATT(z.out, treatment='Segment.x', treated=1)
att_all <- get_qi(att_all, qi='ATT', xvalue = "TE")
quantile(att_all, c(0.025, 0.975))
mean(att_all)

quntiles_cinfidence_level_a = quantile(att_all, c(0.025, 0.975))
mean_diff_a = mean(att_all)
kpi_a = "Revenue"

quntiles_cinfidence_level = rbind(quntiles_cinfidence_level,quntiles_cinfidence_level_a)
mean_diff = rbind(mean_diff,mean_diff_a)
kpi = rbind(kpi,kpi_a)

#estimation Supply HOurs
set.seed(000)
z.out <- zelig(Supply_hours ~ Segment.x + Supply_hours_before +Active_hours_before +Rides_before + Revenue_before +ride_total_distance_before
               + avg_fare_before + acceptance_rate_before + acc_before +req_before 
               , data = dt_overall, model='ls', cite=FALSE)

att_all <- ATT(z.out, treatment='Segment.x', treated=1)
att_all <- get_qi(att_all, qi='ATT', xvalue = "TE")
quantile(att_all, c(0.025, 0.975))

mean(att_all)

quntiles_cinfidence_level_a = quantile(att_all, c(0.025, 0.975))
mean_diff_a = mean(att_all)
kpi_a = "Supply_hours"

quntiles_cinfidence_level = rbind(quntiles_cinfidence_level,quntiles_cinfidence_level_a)
mean_diff = rbind(mean_diff,mean_diff_a)
kpi = rbind(kpi,kpi_a)



#estimation Ignore 
dt_overall$ignRate = dt_overall$ignored / dt_overall$req

set.seed(000)
z.out <- zelig(ignRate  ~ Segment.x + Supply_hours_before +Active_hours_before +Rides_before + Revenue_before +ride_total_distance_before
               + avg_fare_before + acceptance_rate_before + acc_before +req_before
               , data = dt_overall, model='ls', cite=FALSE)

att_all <- ATT(z.out, treatment='Segment.x', treated=1)
att_all <- get_qi(att_all, qi='ATT', xvalue = "TE")
quantile(att_all, c(0.025, 0.975))

mean(att_all)

quntiles_cinfidence_level_a = quantile(att_all, c(0.025, 0.975))
mean_diff_a = mean(att_all)
kpi_a = "IgnoreRate"

quntiles_cinfidence_level = rbind(quntiles_cinfidence_level,quntiles_cinfidence_level_a)
mean_diff = rbind(mean_diff,mean_diff_a)
kpi = rbind(kpi,kpi_a)





output <- data.frame(kpi,mean_diff,quntiles_cinfidence_level)

write.table(output,"/Users/r.nacheva/Downloads/Matching_tre_con_used.csv", sep=',', dec='.',row.names=F)

write.table(all_tg,"/Users/r.nacheva/Downloads/all_tg.csv", sep=',', dec='.',row.names=F)

