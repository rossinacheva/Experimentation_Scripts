
library(forecast)
library(MLmetrics)
library(dplyr)
library(astsa)
library(CausalImpact)
library(zoo)
lapply(list('plyr','dplyr','boot','lubridate','ggplot2'), library, character.only = TRUE)


# Load Data 
data=read.csv("/Users/r.nacheva/Downloads/PED_WE_sessionised_2022_06_02.csv") # All



data$Date <- as.Date(data$day,format = "%Y-%m-%d") # Change Data format

data = data[order(data$Date),] # Order by date
data$week_num = strftime(data$Date, format = "%V") # Add weekl number
data$week_day <- weekdays(as.Date(data$Date)) #Add week day
data$week_day = ifelse (data$week_day %in% c('Saturday','Sunday'),'weekend','weekday')

data = as.data.frame(data)

col_factor <- sapply(data, is.factor)
data[col_factor] <- lapply(data[col_factor], as.character)
num_cols <- sapply(data, is.numeric)
data[,num_cols][is.na(data[,num_cols])] <- 0

summary(data)
head(data)

data$variant = ifelse(data$hour %in% c(0,3,6,9,12,15,18,21) , 'control'
                       ,ifelse(data$hour %in% c(2,5,8,11,14,17,20,23) , 'increase_15%'
                               ,ifelse(data$hour %in% c(1,4,7,10,13,16,19,22) , 'increase_25%' ,0)))
                                




#### Control hours
level_0_segment_name = "Control_hours"

core = subset(data, data$week_day == 'weekend')
segmentation <-  list(segment1 = c('total')
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
    
    data_1= subset(segment_input, segment_input$hour %in% c(3,6,9,12,15,18,21,0) )# control hours
    #data_a = subset(core_a, core_a$hour %in% c(2,5,8,11,14,17,20,23) )# ckeck for treatment
    #data_a = subset(core_a, core_a$hour %in% c(1,4,7,10,13,16,19,22) )# ckeck for treatment
    
    data_b <- data_1 %>%
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
                       acc = sum(acc),
                       dp_eyeballs = sum(dp_eyeballs),
                       surge = sum(surge),
                       basic_amount_surge = sum(basic_amount_surge)
                       
                       )%>% 
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
    data_b$Rides_pActiveDriver = data_b$rides    / data_b$active_drivers
    data_b$Share_DP_eye = data_b$dp_eyeballs/data_b$sess_eye
    data_b$avg_DP = data_b$surge/data_b$basic_amount_surge
    
    col_factor <- sapply(data_b, is.factor)
    data_b[col_factor] <- lapply(data_b[col_factor], as.character)
    num_cols <- sapply(data_b, is.numeric)
    data_b[,num_cols][is.na(data_b[,num_cols])] <- 0
    
    
    kpis_list = c ('EtRC','EtC','PR','Rev_pEye','Rev_pRide','Rides_pDr','Driver_earnings','avg_Eta','Activation_rate','Supply_pDriver',
                   'Utilisation','Pass_CancR','Broadcasts_pDriver','AccR','available_drivers','active_drivers','Rides_pActiveDriver'
                   ,'Share_DP_eye','avg_DP')    
    
    
    #summary(data_a)
    
    for (i in 1: length(kpis_list)) {
      
      data_all = data_b[c('Date',kpis_list[i])]
      
      dt_all = read.zoo(data_all, format = " %Y-%m-%d") 
      
      pre.period <- as.Date(c("2022-04-01", "2022-05-20"))
      post.period <- as.Date(c("2022-05-21", "2022-05-29"))
      
      
      impact_c <- CausalImpact(dt_all, pre.period, post.period, model.args = list(niter = 5000, nseasons = 2))
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


#### Treatment Zones
level_0_segment_name = "increase_15%"

core = subset(data, data$week_day == 'weekend')
segmentation <-  list(segment1 = c('total')
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
    
    data_1= subset(segment_input, segment_input$hour %in% c(3,6,9,12,15,18,21,0) )# control hours
    
    data_tre = subset(segment_input, segment_input$hour %in% c(2,5,8,11,14,17,20,23)) 
                   

    
    
    data_a <- data_1 %>%
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
                       acc = sum(acc),
    dp_eyeballs = sum(dp_eyeballs),
    surge = sum(surge),
    basic_amount_surge = sum(basic_amount_surge)
    
    )%>% 
  
      as.data.frame()
    
    data_a$EtRC = data_a$sess_requests/ data_a$sess_eye
    data_a$EtC = data_a$rides/ data_a$sess_eye
    data_a$PR = data_a$rides/ data_a$sess_requests
    data_a$Rev_pEye   = data_a$revenue    / data_a$sess_eye
    data_a$Rev_pRide   = data_a$revenue    / data_a$rides
    data_a$Driver_earnings   = data_a$dr_earnings    / data_a$available_drivers
    data_a$Rides_pDr   = data_a$rides    / data_a$available_drivers
    data_a$avg_Eta   = data_a$ETA    / data_a$rides
    data_a$Activation_rate   = data_a$active_drivers    / data_a$available_drivers
    data_a$Supply_pDriver   = data_a$Supply_hours    / data_a$available_drivers
    data_a$Utilisation   = data_a$Active_hours    / data_a$Supply_hours
    data_a$Pass_CancR   = data_a$pass_cancellations    / (data_a$pass_cancellations + data_a$rides + data_a$drv_cancellations)
    data_a$Broadcasts_pDriver   = data_a$broadcasts_total    / data_a$available_drivers
    data_a$AccR   = data_a$acc    / data_a$broadcasts_total
    data_a$Rides_pActiveDriver = data_a$rides    / data_a$active_drivers
    data_a$Share_DP_eye = data_a$dp_eyeballs/data_a$sess_eye
    data_a$avg_DP = data_a$surge/data_a$basic_amount_surge
    
    col_factor <- sapply(data_a, is.factor)
    data_a[col_factor] <- lapply(data_a[col_factor], as.character)
    num_cols <- sapply(data_a, is.numeric)
    data_a[,num_cols][is.na(data_a[,num_cols])] <- 0
    
    
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
                       acc = sum(acc),
    dp_eyeballs = sum(dp_eyeballs),
    surge = sum(surge),
    basic_amount_surge = sum(basic_amount_surge)
    
    )%>% 
  
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
    data_b$Rides_pActiveDriver = data_b$rides    / data_b$active_drivers
    data_b$Share_DP_eye = data_b$dp_eyeballs/data_b$sess_eye
    data_b$avg_DP = data_b$surge/data_b$basic_amount_surge
    
    col_factor <- sapply(data_b, is.factor)
    data_b[col_factor] <- lapply(data_b[col_factor], as.character)
    num_cols <- sapply(data_b, is.numeric)
    data_b[,num_cols][is.na(data_b[,num_cols])] <- 0
    
    kpis_list = c ('EtRC','EtC','PR','Rev_pEye','Rev_pRide','Rides_pDr','Driver_earnings','avg_Eta','Activation_rate','Supply_pDriver',
                   'Utilisation','Pass_CancR','Broadcasts_pDriver','AccR','available_drivers','active_drivers','Rides_pActiveDriver'
                   ,'Share_DP_eye','avg_DP')
    
    for (i in 1: length(kpis_list)) {
      
      data_all = data_a[c('Date',kpis_list[i])]
      data_all2 = data_b[c('Date',kpis_list[i])]
      
      dt_all = read.zoo(data_all, format = " %Y-%m-%d") 
      dt_all2 = read.zoo(data_all2, format = " %Y-%m-%d") 
      
      pre.period <- as.Date(c("2022-04-01", "2022-05-20"))
      post.period <- as.Date(c("2022-05-21", "2022-05-29"))
      
      
      impact_c <- CausalImpact(cbind(dt_all2,dt_all), pre.period, post.period, model.args = list(niter = 5000, nseasons = 2))
   #   plot(impact_c)
  #    summary(impact_c)
      
      p_value_a = ifelse (is.null( impact_c$summary[1,15]),0, impact_c$summary[1,15])
      rel_effect_a = ifelse (is.null(impact_c$summary[1,10] ),0,impact_c$summary[1,10])
      kpi_a = kpis_list[i]
      
      mean_observed_a = ifelse (is.null( impact_c$summary[1,1]),0, impact_c$summary[1,1])
      mean_predicted_a = ifelse (is.null( impact_c$summary[1,2]),0, impact_c$summary[1,2])
      segmentation_a = segmentName
      level_0_segment_a = level_0_segment_name
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






#### Treatment Zones 25% increase
level_0_segment_name = "Increase_25%"

core = subset(data, data$week_day == 'weekend')
segmentation <-  list(segment1 = c('total')
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
    
    data_1= subset(segment_input, segment_input$hour %in% c(3,6,9,12,15,18,21,0) )# control hours
    
    data_tre = subset(segment_input, segment_input$hour %in% c(1,4,7,10,13,16,19,22)) 
   


      data_a <- data_1 %>%
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
                       acc = sum(acc),
      dp_eyeballs = sum(dp_eyeballs),
      surge = sum(surge),
      basic_amount_surge = sum(basic_amount_surge)
      
      )%>% 
  
      as.data.frame()
    
    data_a$EtRC = data_a$sess_requests/ data_a$sess_eye
    data_a$EtC = data_a$rides/ data_a$sess_eye
    data_a$PR = data_a$rides/ data_a$sess_requests
    data_a$Rev_pEye   = data_a$revenue    / data_a$sess_eye
    data_a$Rev_pRide   = data_a$revenue    / data_a$rides
    data_a$Driver_earnings   = data_a$dr_earnings    / data_a$available_drivers
    data_a$Rides_pDr   = data_a$rides    / data_a$available_drivers
    data_a$avg_Eta   = data_a$ETA    / data_a$rides
    data_a$Activation_rate   = data_a$active_drivers    / data_a$available_drivers
    data_a$Supply_pDriver   = data_a$Supply_hours    / data_a$available_drivers
    data_a$Utilisation   = data_a$Active_hours    / data_a$Supply_hours
    data_a$Pass_CancR   = data_a$pass_cancellations    / (data_a$pass_cancellations + data_a$rides + data_a$drv_cancellations)
    data_a$Broadcasts_pDriver   = data_a$broadcasts_total    / data_a$available_drivers
    data_a$AccR   = data_a$acc    / data_a$broadcasts_total
    data_a$Rides_pActiveDriver = data_a$rides    / data_a$active_drivers
    data_a$Share_DP_eye = data_a$dp_eyeballs/data_a$sess_eye
    data_a$avg_DP = data_a$surge/data_a$basic_amount_surge
    
    col_factor <- sapply(data_a, is.factor)
    data_a[col_factor] <- lapply(data_a[col_factor], as.character)
    num_cols <- sapply(data_a, is.numeric)
    data_a[,num_cols][is.na(data_a[,num_cols])] <- 0
    
    
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
                       acc = sum(acc),
    dp_eyeballs = sum(dp_eyeballs),
    surge = sum(surge),
    basic_amount_surge = sum(basic_amount_surge)
    
    )%>% 
  
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
    data_b$Rides_pActiveDriver = data_b$rides    / data_b$active_drivers
    data_b$Share_DP_eye = data_b$dp_eyeballs/data_b$sess_eye
    data_b$avg_DP = data_b$surge/data_b$basic_amount_surge
    
    col_factor <- sapply(data_b, is.factor)
    data_b[col_factor] <- lapply(data_b[col_factor], as.character)
    num_cols <- sapply(data_b, is.numeric)
    data_b[,num_cols][is.na(data_b[,num_cols])] <- 0
    
    
    kpis_list = c ('EtRC','EtC','PR','Rev_pEye','Rev_pRide','Rides_pDr','Driver_earnings','avg_Eta','Activation_rate','Supply_pDriver',
                   'Utilisation','Pass_CancR','Broadcasts_pDriver','AccR','available_drivers','active_drivers','Rides_pActiveDriver'
                   ,'Share_DP_eye','avg_DP')
    
    summary(data_a)
    
    for (i in 1: length(kpis_list)) {
      
      data_all = data_a[c('Date',kpis_list[i])]
      data_all2 = data_b[c('Date',kpis_list[i])]
      
      dt_all = read.zoo(data_all, format = " %Y-%m-%d") 
      dt_all2 = read.zoo(data_all2, format = " %Y-%m-%d") 
      
      pre.period <- as.Date(c("2022-04-01", "2022-05-20"))
      post.period <- as.Date(c("2022-05-21", "2022-05-29"))
      
      
      impact_c <- CausalImpact(cbind(dt_all2,dt_all), pre.period, post.period, model.args = list(niter = 5000, nseasons = 2))
      #plot(impact_c)
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


output = rbind(output_1,output_2,output_3)
output$market = 'AR'
write.table(output,"/Users/r.nacheva/Downloads/PED_AR_weekend_elasticities_v4.csv", sep=',', dec='.',row.names=F)



########################## END of Analysis


