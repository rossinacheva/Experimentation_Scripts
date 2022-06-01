
#### Install and load needed packages
wants <- c('plyr','dplyr','boot','lubridate','ggplot2','forecast',
           'MLmetrics','astsa','CausalImpact','zoo')
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
lapply(wants, library, character.only=T)


# Load Data 
data_1=read.csv("/Users/r.nacheva/Downloads/PriceChange_May_2022_totals_2022_06_01.csv")# All
data_2=read.csv("/Users/r.nacheva/Downloads/PriceChange_May_2022_zones_2022_06_01.csv")# All
data_3=read.csv("/Users/r.nacheva/Downloads/PriceChange_May_2022_service_2022_06_01.csv")# All

data = rbind(data_1,data_2,data_3)



data = subset(data, (data$service_name %in% c('Beat','XL','Envío','Luxi','ALl')))
data = subset(data, data$zone %in% c('All','Playas del Sur','Costa Verde','Lima Este','Callao','Lima Sur','Lima Norte','Lima Centro','Lima Moderna'
))

unique(data$service_name)

data$Date <- as.Date(data$day,format = "%Y-%m-%d") # Change Data format

data = data[order(data$Date),] # Order by date
data$week_num = strftime(data$Date, format = "%V") # Add weekl number
data$week_day <- weekdays(as.Date(data$Date)) #Add week day
data$week_day = ifelse (data$week_day %in% c('Saturday','Sunday'),'weekend','weekday')

data = as.data.frame(data)

#data$time_day = ifelse(data$hour %in% c(12,13,14,15,16,17,18,19,20,21,22,23,0) & data$week_day == 'weekday', 'weekday_12-00___10%_increase'
 #                      , ifelse(data$hour %in% c(0,1,2,3) & data$week_day == 'weekend', 'weekend_00-03___15%_increase'
  #                     , ifelse(!(data$hour %in% c(0,1,2,3,4,5,6)) & data$week_day == 'weekend', 'weekend_07-00___15%_increase'
   #                     ,'no_price_change')))


col_factor <- sapply(data, is.factor)
data[col_factor] <- lapply(data[col_factor], as.character)
num_cols <- sapply(data, is.numeric)
data[,num_cols][is.na(data[,num_cols])] <- 0

summary(data)
head(data)




#### Forecasting Performance
level_0_segment_name = "forecasting_total_levels"

core = data

segmentation <-  list(segment1 = c('zone','service_name')
                    #  ,segment4 = c('time_day')
                    #  ,segment5 = c('time_day','service_name')
                    #  ,segment6 = c('time_day','zone')
                    #  ,segment7 = c('time_day','service_name','zone')
                      
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
    
    
    data_tre = segment_input
    
  
    
    
    
    data_b <- data_tre %>%
      group_by(Date) %>%
      dplyr::summarise(sess_eye = sum(sess_eye),
                       sess_requests= sum(sess_requests),
                       rides = sum(rides),
                       revenue = sum(revenue),
                       dr_earnings = sum(dr_earnings),
                       ETA = sum(ETA),
                       available_drivers = sum(available_drivers),
                       active_drivers = sum(active_drivers),
                       Active_hours = sum(Active_hours),
                       Supply_hours = sum(Supply_hours),
                       pass_cancellations = sum(pass_cancellations),
                       drv_cancellations = sum(drv_cancellations),
                       broadcasts_total = sum(broadcasts_total),
                       acc = sum(acc))%>% 
      as.data.frame()
    
    data_b$EtRC = data_b$sess_requests/ data_b$sess_eye
    data_b$EtC = data_b$rides/ data_b$sess_eye
    data_b$PR = data_b$rides/ data_b$sess_requests
    data_b$Rev_pEye   = data_b$revenue    / data_b$sess_eye
    data_b$Rev_pRide   = data_b$revenue    / data_b$rides
    data_b$Driver_earnings   = data_b$dr_earnings    / data_b$available_drivers
    data_b$Rides_pDr   = data_b$rides    / data_b$available_drivers
    data_b$avg_Eta   = data_b$ETA    / data_b$rides
    data_b$Activation_rate   = data_b$active_drivers    / data_b$available_drivers
    data_b$Supply_pDriver   = data_b$Supply_hours    / data_b$available_drivers
    data_b$Utilisation   = data_b$Active_hours    / data_b$Supply_hours
    data_b$Pass_CancR   = data_b$pass_cancellations    / (data_b$pass_cancellations + data_b$rides + data_b$drv_cancellations)
    data_b$Broadcasts_pDriver   = data_b$broadcasts_total    / data_b$available_drivers
    data_b$AccR   = data_b$acc    / data_b$broadcasts_total
    
    kpis_list = c ('EtRC','EtC','PR','Rev_pEye','Rev_pRide','Rides_pDr','Driver_earnings','avg_Eta','Activation_rate','Supply_pDriver',
                   'Utilisation','Pass_CancR','Broadcasts_pDriver','AccR')
    
    #summary(data_a)
    
    for (i in 1: length(kpis_list)) {
      

      data_all2 = data_b[c('Date',kpis_list[i])]
      

      dt_all2 = read.zoo(data_all2, format = " %Y-%m-%d") 
      
      pre.period <- as.Date(c("2022-04-02", "2022-05-21"))
      post.period <- as.Date(c("2022-05-22", "2022-05-30"))
      
  #    impact_c <- CausalImpact(cbind(dt_all2,dt_all), pre.period, post.period, model.args = list(niter = 5000, nseasons = 7))
   
      impact_c <- CausalImpact(dt_all2, pre.period, post.period, model.args = list(niter = 5000, nseasons = 7))
      
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

output_3 <- data.frame(level_0_segment,segmentationL,kpi,mean_observed,mean_predicted,rel_effect,p_value,flag_segment)







#### Forecasting Performance - weekdays (5 units of seasonality)
level_0_segment_name = "forecasting_weekdays"

core = subset(data, data$week_day == 'weekday')

segmentation <-  list(segment1 = c('zone','service_name')
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
    
    
    data_tre = segment_input
    
    
    
    
    
    data_b <- data_tre %>%
      group_by(Date) %>%
      dplyr::summarise(sess_eye = sum(sess_eye),
                       sess_requests= sum(sess_requests),
                       rides = sum(rides),
                       revenue = sum(revenue),
                       dr_earnings = sum(dr_earnings),
                       ETA = sum(ETA),
                       available_drivers = sum(available_drivers),
                       active_drivers = sum(active_drivers),
                       Active_hours = sum(Active_hours),
                       Supply_hours = sum(Supply_hours),
                       pass_cancellations = sum(pass_cancellations),
                       drv_cancellations = sum(drv_cancellations),
                       broadcasts_total = sum(broadcasts_total),
                       acc = sum(acc))%>% 
      as.data.frame()
    
    data_b$EtRC = data_b$sess_requests/ data_b$sess_eye
    data_b$EtC = data_b$rides/ data_b$sess_eye
    data_b$PR = data_b$rides/ data_b$sess_requests
    data_b$Rev_pEye   = data_b$revenue    / data_b$sess_eye
    data_b$Rev_pRide   = data_b$revenue    / data_b$rides
    data_b$Driver_earnings   = data_b$dr_earnings    / data_b$available_drivers
    data_b$Rides_pDr   = data_b$rides    / data_b$available_drivers
    data_b$avg_Eta   = data_b$ETA    / data_b$rides
    data_b$Activation_rate   = data_b$active_drivers    / data_b$available_drivers
    data_b$Supply_pDriver   = data_b$Supply_hours    / data_b$available_drivers
    data_b$Utilisation   = data_b$Active_hours    / data_b$Supply_hours
    data_b$Pass_CancR   = data_b$pass_cancellations    / (data_b$pass_cancellations + data_b$rides + data_b$drv_cancellations)
    data_b$Broadcasts_pDriver   = data_b$broadcasts_total    / data_b$available_drivers
    data_b$AccR   = data_b$acc    / data_b$broadcasts_total
    
    kpis_list = c ('EtRC','EtC','PR','Rev_pEye','Rev_pRide','Rides_pDr','Driver_earnings','avg_Eta','Activation_rate','Supply_pDriver',
                   'Utilisation','Pass_CancR','Broadcasts_pDriver','AccR')
    
    #summary(data_a)
    
    for (i in 1: length(kpis_list)) {
      
      
      data_all2 = data_b[c('Date',kpis_list[i])]
      
      
      dt_all2 = read.zoo(data_all2, format = " %Y-%m-%d") 
      
      pre.period <- as.Date(c("2022-04-02", "2022-05-21"))
      post.period <- as.Date(c("2022-05-22", "2022-05-30"))
      
      #    impact_c <- CausalImpact(cbind(dt_all2,dt_all), pre.period, post.period, model.args = list(niter = 5000, nseasons = 7))
      
      impact_c <- CausalImpact(dt_all2, pre.period, post.period, model.args = list(niter = 5000, nseasons = 5))
      
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








#### Forecasting Performance - weekdays (5 units of seasonality)
level_0_segment_name = "forecasting_weekend"

core = subset(data, data$week_day == 'weekend')

segmentation <-  list(segment1 = c('zone','service_name')
  
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
    
    
    data_tre = segment_input
    
    
    
    
    
    data_b <- data_tre %>%
      group_by(Date) %>%
      dplyr::summarise(sess_eye = sum(sess_eye),
                       sess_requests= sum(sess_requests),
                       rides = sum(rides),
                       revenue = sum(revenue),
                       dr_earnings = sum(dr_earnings),
                       ETA = sum(ETA),
                       available_drivers = sum(available_drivers),
                       active_drivers = sum(active_drivers),
                       Active_hours = sum(Active_hours),
                       Supply_hours = sum(Supply_hours),
                       pass_cancellations = sum(pass_cancellations),
                       drv_cancellations = sum(drv_cancellations),
                       broadcasts_total = sum(broadcasts_total),
                       acc = sum(acc))%>% 
      as.data.frame()
    
    data_b$EtRC = data_b$sess_requests/ data_b$sess_eye
    data_b$EtC = data_b$rides/ data_b$sess_eye
    data_b$PR = data_b$rides/ data_b$sess_requests
    data_b$Rev_pEye   = data_b$revenue    / data_b$sess_eye
    data_b$Rev_pRide   = data_b$revenue    / data_b$rides
    data_b$Driver_earnings   = data_b$dr_earnings    / data_b$available_drivers
    data_b$Rides_pDr   = data_b$rides    / data_b$available_drivers
    data_b$avg_Eta   = data_b$ETA    / data_b$rides
    data_b$Activation_rate   = data_b$active_drivers    / data_b$available_drivers
    data_b$Supply_pDriver   = data_b$Supply_hours    / data_b$available_drivers
    data_b$Utilisation   = data_b$Active_hours    / data_b$Supply_hours
    data_b$Pass_CancR   = data_b$pass_cancellations    / (data_b$pass_cancellations + data_b$rides + data_b$drv_cancellations)
    data_b$Broadcasts_pDriver   = data_b$broadcasts_total    / data_b$available_drivers
    data_b$AccR   = data_b$acc    / data_b$broadcasts_total
    
    
    col_factor <- sapply(data_b, is.factor)
    data_b[col_factor] <- lapply(data_b[col_factor], as.character)
    num_cols <- sapply(data_b, is.numeric)
    data_b[,num_cols][is.na(data_b[,num_cols])] <- 0
    
    
    
    kpis_list = c ('EtRC','EtC','PR','Rev_pEye','Rev_pRide','Rides_pDr','Driver_earnings','avg_Eta','Activation_rate','Supply_pDriver',
                   'Utilisation','Pass_CancR','Broadcasts_pDriver','AccR')
    
    #summary(data_a)
    
    for (i in 1: length(kpis_list)) {
      
      
      data_all2 = data_b[c('Date',kpis_list[i])]
      
      
      dt_all2 = read.zoo(data_all2, format = " %Y-%m-%d") 
      
      pre.period <- as.Date(c("2022-04-02", "2022-05-21"))
      post.period <- as.Date(c("2022-05-22", "2022-05-30"))
      
      #    impact_c <- CausalImpact(cbind(dt_all2,dt_all), pre.period, post.period, model.args = list(niter = 5000, nseasons = 7))
      
      impact_c <- CausalImpact(dt_all2, pre.period, post.period, model.args = list(niter = 5000, nseasons = 2))
      
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

output_1 <- data.frame(level_0_segment,segmentationL,kpi,mean_observed,mean_predicted,rel_effect,p_value,flag_segment)








output = rbind(output_3,output_2,output_1)


#write.table(output,"/Users/r.nacheva/Downloads/Moto_forecast.csv", sep=',', dec='.',row.names=F)

