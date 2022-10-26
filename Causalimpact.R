#### Install and load needed packages
wants <- c("forecast", "MLmetrics", "dplyr", "astsa", "CausalImpact","zoo",'plyr','dplyr','boot','lubridate','ggplot2')
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
lapply(wants, library, character.only=T)



#### Load data
data=read.csv("/Users/r.nacheva/Downloads/PED_AR_t2_q3_data_extract_1_2022_10_12.csv") # All


summary(data)

head(data)


data$Date <- as.Date(data$hour,format = "%Y-%m-%d") # Change Data format

data = data[order(data$Date),] # Order by date
data$week_num = strftime(data$Date, format = "%V") # Add weekl number
data$week_day1 <- weekdays(as.Date(data$Date)) #Add week day
data$week_day = ifelse (data$week_day1 %in% c('Saturday','Sunday'),'weekend','weekday')


data$hour_r = as.numeric(strftime(data$hour, format = "%H"))

col_factor <- sapply(data, is.factor)
data[col_factor] <- lapply(data[col_factor], as.character)
num_cols <- sapply(data, is.numeric)
data[,num_cols][is.na(data[,num_cols])] <- 0


#### Assign period
data$period = ifelse (data$hour >= '2022-07-21 00:00:00.000','after',
                      ifelse (data$hour >= '2022-07-14 00:00:00.000','week_3',
                              ifelse (data$hour >= '2022-07-07 00:00:00.000','week_2',
                                      ifelse (data$hour >= '2022-06-30 00:00:00.000','week_1',
                                              ifelse (data$hour >= '2022-06-23 00:00:00.000','week_3_before',
                                                      ifelse (data$hour >= '2022-06-16 00:00:00.000','week_2_before',
                                                              ifelse (data$hour >= '2022-06-09 00:00:00.000','week_1_before',     
                                                                      'before'
                                                              )))))))


#### Assign variant
data$variant = 
  ifelse (data$period %in% c( 'week_1','week_1_before') & data$hour_r %in% c(1,5,9,13,17,21), 'increase_14',
          ifelse (data$period %in% c( 'week_1','week_1_before') & data$hour_r %in% c(3,7,11,15,19,23), 'increase_28',
                  ifelse (data$period %in% c( 'week_1','week_1_before')  & data$hour_r %in% c(2,6,10,14,18,22), 'increase_21', 
                          ifelse (data$period %in% c( 'week_1','week_1_before') & data$hour_r %in% c(0,4,8,12,16,20), 'Control', 
                          
                          ifelse (data$period %in% c( 'week_2','week_2_before') & data$hour_r %in% c(3,7,11,15,19,23), 'increase_21',
                                  ifelse (data$period %in% c( 'week_2','week_2_before') & data$hour_r %in% c(2,6,10,14,18,22), 'increase_14',
                                          ifelse (data$period %in% c( 'week_2','week_2_before') & data$hour_r %in% c(1,5,9,13,17,21), 'increase_28', 
                                                  ifelse (data$period %in% c( 'week_2','week_2_before') & data$hour_r %in% c(0,4,8,12,16,20), 'Control',  
                                                  
                                                  ifelse (data$period %in% c( 'week_3','week_3_before') & data$hour_r %in% c(2,6,10,14,18,22), 'increase_28',
                                                          ifelse (data$period %in% c( 'week_3','week_3_before') & data$hour_r %in% c(1,5,9,13,17,21), 'increase_21',
                                                                  ifelse (data$period %in% c( 'week_3','week_3_before') & data$hour_r %in% c(3,7,11,15,19,23), 'increase_14',
                                                                          ifelse (data$period %in% c( 'week_3','week_3_before') & data$hour_r %in% c(0,4,8,12,16,20), 'Control',  
                                                                      'out'    
                                                                   ))))))))))))

data = as.data.frame(data)

col_factor <- sapply(data, is.factor)
data[col_factor] <- lapply(data[col_factor], as.character)
num_cols <- sapply(data, is.numeric)
data[,num_cols][is.na(data[,num_cols])] <- 0

summary(data)
head(data)

data = subset(data, data$variant %in% c('increase_14','increase_21','Control','increase_28'))




######################################################


####### Control hours - pre post analysis 
level_0_segment_name = "g.Control_hours_core"

core = subset(data, data$Clusters %in% c(1,2,4,5))
#core=core[!(is.na(core$zone_cb) | core$zone_cb=="" ),] 
#core = subset(core,!( core$zone_cb %in% c('Totoralillo','Serena Golf','Colina')))


segmentation <-  list(segment1 = c('total')
                        ,segment2 = c('Clusters')
                      #  ,segment3 = c('special')
                      # ,segment4 = c('district')
                      # ,segment4 = c('battleground')
)
p_value = c()
rel_effect = c()
kpi = c()
mean_observed = c()
mean_predicted = c()
segmentationL = c()
correl = c()
level_0_segment = c()
flag_segment = c()

for (level in 1:length(segmentation)){
  
  if (tolower(segmentation[level][[1]][1]) == "total"){
    segment_input <- core
    combinations = 1
  }
  else {
    unique_combinations <- unique(core[,segmentation[level][[1]], drop = FALSE])
    # filter out missing values
    unique_combinations <- as.data.frame(na.omit(unique_combinations))

    combinations = nrow(unique_combinations)
  }
  
  for(i_comb in 1:combinations){
    
    if (tolower(segmentation[level][[1]][1]) == "total"){
      segment_input <- core
    }
    else {
      segment_input <- merge(unique_combinations[i_comb,,drop = FALSE], core)
      
    }
    segmentName = ifelse(tolower(segmentation[level][[1]][1]) == "total", "Total", paste(as.character(as.vector(unique_combinations[i_comb,])), collapse = "_"))
    print(segmentName)
    
    
    #segment = "control_hours"
    
    data_1= subset(segment_input, segment_input$variant %in% c('Control')) 
    data_a <- data_1 %>%
      group_by(Date) %>%
      dplyr::summarise(
        searches = sum(unsess_searches),
        eyeballs_sess= sum(sess_eyeballs),
        eyeballs_unsess= sum(unsess_eyeballs),
        unsess_requests = sum(unsess_requests),
        unsess_pass = sum(unsess_pass),
        sess_requests = sum(sess_requests),
        rides = sum(total_rides),
        revenue = sum(revenue),
        dr_earnings = sum(dr_earnings),
        available_drivers = sum(available_drivers),
        supply_hours = sum(supply_hours),
        unsess_surge_amount_search_sum = sum(unsess_surge_amount_search_sum),
        unsess_basic_amount = sum(unsess_basic_amount),
        active_hours = sum(on_ride))%>% 
      as.data.frame()
    data_a$EtSC_unsess = data_a$searches/ data_a$eyeballs_unsess
    data_a$EtSC_pPass = data_a$searches/ data_a$unsess_pass
    data_a$EtRC_sess = data_a$sess_requests/ data_a$eyeballs_sess
    data_a$EtC_sess = ifelse(data_a$rides >0, data_a$rides/ data_a$eyeballs_sess,0)
    data_a$RpE_sess = ifelse(data_a$rides >0, data_a$revenue/ data_a$eyeballs_sess,0)
    data_a$PuR_sess = ifelse(data_a$sess_requests >0, data_a$rides/ data_a$sess_requests,0)
    
    data_a$Dr_earnings = data_a$dr_earnings/ data_a$available_drivers
    data_a$Utilisation = data_a$active_hours/ data_a$supply_hours
    data_a$DP_level = data_a$unsess_surge_amount_search_sum/data_a$unsess_basic_amount
    kpis_list = c ('EtSC_unsess','EtSC_pPass','EtRC_sess','EtC_sess',
                   'RpE_sess','PuR_sess','Dr_earnings','Utilisation','DP_level')
    

    
    for (i in 1: length(kpis_list)) {
      
      data_all = data_a[c('Date',kpis_list[i])]
      dt_all = read.zoo(data_all, format = " %Y-%m-%d") 
      
      pre.period <- as.Date(c("2022-06-10", "2022-06-30"))
      post.period <- as.Date(c("2022-07-01", "2022-07-21"))
      impact_c <- CausalImpact(dt_all, pre.period, post.period, model.args = list(niter = 5000, nseasons = 7))
      

      
      p_value_a = ifelse (is.null( impact_c$summary[1,15]),0, impact_c$summary[1,15])
      rel_effect_a = ifelse (is.null(impact_c$summary[1,10] ),0,impact_c$summary[1,10])
      kpi_a = kpis_list[i]
      
      mean_observed_a = ifelse (is.null( impact_c$summary[1,1]),0, impact_c$summary[1,1])
      mean_predicted_a = ifelse (is.null( impact_c$summary[1,2]),0, impact_c$summary[1,2])
      
      segmentation_a = segmentName
      flag_segment_a = segmentation[level][[1]][1]
      
      p_value = rbind(p_value,p_value_a)
      rel_effect = rbind(rel_effect,rel_effect_a)
      kpi = rbind(kpi,kpi_a)
      mean_observed = rbind(mean_observed,mean_observed_a)
      mean_predicted = rbind(mean_predicted,mean_predicted_a)
      segmentationL = rbind(segmentationL,segmentation_a)
      level_0_segment = rbind(level_0_segment,level_0_segment_name)
      flag_segment = rbind(flag_segment,flag_segment_a)
      correl = rbind(correl,'none')
    }
  }
}

#output_1_a <- data.frame(level_0_segment,segmentationL,kpi,mean_observed,mean_predicted,rel_effect,p_value,flag_segment)
output_1 <- data.frame(level_0_segment,segmentationL,kpi,mean_observed,mean_predicted,rel_effect,p_value,flag_segment)


######################################################
#Treatment calculation based on Control hours performance pre and post experiment


level_0_segment_name = "c.21%_increase"

core = subset(data, data$id_city  == 1 & data$Clusters %in% c(1,2,4,5))
#core=core[!(is.na(core$zone_cb) | core$zone_cb=="" ),] 
#core = subset(core,!( core$zone_cb %in% c('Totoralillo','Serena Golf','Colina')))
#core = subset(core,!( core$zone_new %in% c('airport')))

segmentation <-  list(segment1 = c('total')
                        ,segment2 = c('Clusters')
                      # ,segment4 = c('district')
                      # ,segment4 = c('battleground')
)
p_value = c()
rel_effect = c()
kpi = c()
mean_observed = c()
mean_predicted = c()
segmentationL = c()
correl = c()
level_0_segment = c()
flag_segment = c()

for (level in 1:length(segmentation)){
  
  if (tolower(segmentation[level][[1]][1]) == "total"){
    segment_input <- core
    combinations = 1
  }
  else {
    unique_combinations <- unique(core[,segmentation[level][[1]], drop = FALSE])
    # filter out missing values
    unique_combinations <- na.omit(unique_combinations)
    combinations = nrow(unique_combinations)
  }
  
  for(i_comb in 1:combinations){
    
    if (tolower(segmentation[level][[1]][1]) == "total"){
      segment_input <- core
    }
    else {
      segment_input <- merge(unique_combinations[i_comb,,drop = FALSE], core)
      
    }
    segmentName = ifelse(tolower(segmentation[level][[1]][1]) == "total", "Total", paste(as.character(as.vector(unique_combinations[i_comb,])), collapse = "_"))
    print(segmentName)
    
    
    #segment = "control_hours"
    
    data_1= subset(segment_input, segment_input$variant %in% c('Control') )# control hours
    data_2 = subset(segment_input, segment_input$variant == 'increase_21')
    
    data_a <- data_1 %>%
      group_by(Date) %>%
      dplyr::summarise(
        searches = sum(unsess_searches),
        eyeballs_sess= sum(sess_eyeballs),
        eyeballs_unsess= sum(unsess_eyeballs),
        unsess_requests = sum(unsess_requests),
        unsess_pass = sum(unsess_pass),
        sess_requests = sum(sess_requests),
        rides = sum(total_rides),
        revenue = sum(revenue),
        dr_earnings = sum(dr_earnings),
        available_drivers = sum(available_drivers),
        supply_hours = sum(supply_hours),
        unsess_surge_amount_search_sum = sum(unsess_surge_amount_search_sum),
        unsess_basic_amount = sum(unsess_basic_amount),
        active_hours = sum(on_ride))%>% 
      as.data.frame()
    data_a$EtSC_unsess = data_a$searches/ data_a$eyeballs_unsess
    data_a$EtSC_pPass = data_a$searches/ data_a$unsess_pass
    data_a$EtRC_sess = data_a$sess_requests/ data_a$eyeballs_sess
    data_a$EtC_sess = ifelse(data_a$rides >0, data_a$rides/ data_a$eyeballs_sess,0)
    data_a$RpE_sess = ifelse(data_a$rides >0, data_a$revenue/ data_a$eyeballs_sess,0)
    data_a$PuR_sess = ifelse(data_a$sess_requests >0, data_a$rides/ data_a$sess_requests,0)
    data_a$Dr_earnings = data_a$dr_earnings/ data_a$available_drivers
    data_a$Utilisation = data_a$active_hours/ data_a$supply_hours
    data_a$DP_level = data_a$unsess_surge_amount_search_sum/data_a$unsess_basic_amount
    
    
    
    data_b <- data_2 %>%
      group_by(Date) %>%
      dplyr::summarise(
        searches = sum(unsess_searches),
        eyeballs_sess= sum(sess_eyeballs),
        eyeballs_unsess= sum(unsess_eyeballs),
        unsess_requests = sum(unsess_requests),
        unsess_pass = sum(unsess_pass),
        sess_requests = sum(sess_requests),
        rides = sum(total_rides),
        revenue = sum(revenue),
        dr_earnings = sum(dr_earnings),
        available_drivers = sum(available_drivers),
        supply_hours = sum(supply_hours),
        unsess_surge_amount_search_sum = sum(unsess_surge_amount_search_sum),
        unsess_basic_amount = sum(unsess_basic_amount),
        active_hours = sum(on_ride))%>% 
      as.data.frame()
    data_b$EtSC_unsess = data_b$searches/ data_b$eyeballs_unsess
    data_b$EtSC_pPass = data_b$searches/ data_b$unsess_pass
    data_b$EtRC_sess = data_b$sess_requests/ data_b$eyeballs_sess
    data_b$EtC_sess = ifelse(data_b$rides >0, data_b$rides/ data_b$eyeballs_sess,0)
    data_b$RpE_sess = ifelse(data_b$rides >0, data_b$revenue/ data_b$eyeballs_sess,0)
    data_b$PuR_sess = ifelse(data_b$sess_requests >0, data_b$rides/ data_b$sess_requests,0)
    data_b$Dr_earnings = data_b$dr_earnings/ data_b$available_drivers
    data_b$Utilisation = data_b$active_hours/ data_b$supply_hours
    data_b$DP_level = data_b$unsess_surge_amount_search_sum/data_b$unsess_basic_amount
    
    
    
    #summary(data_a)
    
    for (i in 1: length(kpis_list)) {
      
      data_all = data_a[c('Date',kpis_list[i])]
      dt_all = read.zoo(data_all, format = " %Y-%m-%d") 
      
      data_all_2 = data_b[c('Date',kpis_list[i])]
      dt_all_2 = read.zoo(data_all_2, format = " %Y-%m-%d") 
      
      pre.period <- as.Date(c("2022-06-10", "2022-06-30"))
      post.period <- as.Date(c("2022-07-01", "2022-07-21"))
      
      
      impact_c <- CausalImpact(cbind(dt_all_2,dt_all), pre.period, post.period, model.args = list(niter = 5000, nseasons = 7))
      #plot(impact_c)
      #summary(impact_c)
      
      p_value_a = ifelse (is.null( impact_c$summary[1,15]),0, impact_c$summary[1,15])
      rel_effect_a = ifelse (is.null(impact_c$summary[1,10] ),0,impact_c$summary[1,10])
      kpi_a = kpis_list[i]
      
      mean_observed_a = ifelse (is.null( impact_c$summary[1,1]),0, impact_c$summary[1,1])
      mean_predicted_a = ifelse (is.null( impact_c$summary[1,2]),0, impact_c$summary[1,2])
      
      segmentation_a = segmentName
      flag_segment_a = segmentation[level][[1]][1]
      
      p_value = rbind(p_value,p_value_a)
      rel_effect = rbind(rel_effect,rel_effect_a)
      kpi = rbind(kpi,kpi_a)
      mean_observed = rbind(mean_observed,mean_observed_a)
      mean_predicted = rbind(mean_predicted,mean_predicted_a)
      segmentationL = rbind(segmentationL,segmentation_a)
      level_0_segment = rbind(level_0_segment,level_0_segment_name)
      flag_segment = rbind(flag_segment,flag_segment_a)
      correl = rbind(correl,'none')
    }
  }
}

output_2 <- data.frame(level_0_segment,segmentationL,kpi,mean_observed,mean_predicted,rel_effect,p_value,flag_segment)


##################################
#Collect all outputs into 1 file and then save it as a scv

final_output = rbind(output_1, output_2)
write.table(final_output,"/Users/r.nacheva/Downloads/PED_AR_test_2_totals_clusters.csv", sep=',', dec='.',row.names=F)







