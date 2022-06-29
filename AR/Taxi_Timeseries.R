
library(forecast)
library(MLmetrics)
library(dplyr)
library(astsa)
library(CausalImpact)
library(zoo)
lapply(list('plyr','dplyr','boot','lubridate','ggplot2'), library, character.only = TRUE)



# Load Data 
data_a=read.csv("/Users/r.nacheva/Downloads/Taxi_results_2022_06_29 (3).csv") # All
data_b=read.csv("/Users/r.nacheva/Downloads/Taxi_results_2022_06_29 (2).csv") #No Taxi
data_b = subset(data_b, data_b$service_name == 'other')


data= rbind(data_a,data_b)

head(data)


data$Date <- as.Date(data$day,format = "%Y-%m-%d") # Change Data format

data = data[order(data$Date),] # Order by date


col_factor <- sapply(data, is.factor)
data[col_factor] <- lapply(data[col_factor], as.character)
num_cols <- sapply(data, is.numeric)
data[,num_cols][is.na(data[,num_cols])] <- 0





summary(data)
head(data)

####### Plot data on Rides and check to outliers or strange behaviour
#plot(data$eyeballs, col="black", xlab="Day", ylab="Eyeballs", main="Overview Rides", type='l')




#### Treatment Zones
level_0_segment_name = "Service_separately"


core = subset(data,data$service_name == 'All')
segmentation <-  list(segment1 = c('flag_taxi')
                      # ,segment2 = c('flag_taxi','service_name')
                      #   ,segment3 = c('zone')
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
    
    data_1= segment_input
    
    data_a <- data_1 %>%
      group_by(Date) %>%
      dplyr::summarise(
        rides = sum(rides),
        available_drivers = sum(available_drivers),
        revenue = sum(revenue),
        Supply_hours = sum(Supply_hours),
        Active_hours = sum(Active_hours)

      )%>% 
  as.data.frame()
    
    data_a$rides_pDr = data_a$rides/ data_a$available_drivers
    data_a$revenue_pDr = data_a$revenue/ data_a$available_drivers
    data_a$supply_pDr = data_a$Supply_hours/ data_a$available_drivers
    data_a$utilisation = data_a$Active_hours/ data_a$Supply_hours
    
    
    kpis_list = c ('rides_pDr','revenue_pDr','supply_pDr','utilisation')
    
    
    
    #summary(data_a)
    
    for (i in 1: length(kpis_list)) {
      
      data_all = data_a[c('Date',kpis_list[i])]
      dt_all = read.zoo(data_all, format = " %Y-%m-%d") 
      
      pre.period <- as.Date(c("2022-05-06", "2022-06-06"))
      post.period <- as.Date(c("2022-06-07", "2022-06-28"))
      
      impact_c <- CausalImpact(dt_all, pre.period, post.period, model.args = list(niter = 5000, nseasons = 7))
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

#output_1_a <- data.frame(level_0_segment,segmentationL,kpi,mean_observed,mean_predicted,rel_effect,p_value,flag_segment)
output_1 <- data.frame(level_0_segment,segmentationL,kpi,mean_observed,mean_predicted,rel_effect,p_value,flag_segment)





#### Treatment Zones
level_0_segment_name = "Corrected_onNonTaxi_drivers"


core = subset(data,data$service_name == 'All')

segmentation <-  list(segment1 = c('total')
                      # ,segment2 = c('service_name')
                      #   ,segment3 = c('zone')
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
    
    data_1= subset(segment_input, segment_input$flag_taxi %in% c('not_taxi') )# control hours
    data_2 = subset(segment_input, segment_input$flag_taxi == 'taxi')
    
   
    
    data_a <- data_1 %>%
      group_by(Date) %>%
      dplyr::summarise(
        rides = sum(rides),
        available_drivers = sum(available_drivers),
        revenue = sum(revenue),
        Supply_hours = sum(Supply_hours),
        Active_hours = sum(Active_hours)
        
      )%>% 
      as.data.frame()
      
        data_a$rides_pDr = data_a$rides/ data_a$available_drivers
        data_a$revenue_pDr = data_a$revenue/ data_a$available_drivers
        data_a$supply_pDr = data_a$Supply_hours/ data_a$available_drivers
        data_a$utilisation = data_a$Active_hours/ data_a$Supply_hours
        
   
    
    
        data_b <- data_2 %>%
          group_by(Date) %>%
          dplyr::summarise(
            rides = sum(rides),
            available_drivers = sum(available_drivers),
            revenue = sum(revenue),
            Supply_hours = sum(Supply_hours),
            Active_hours = sum(Active_hours)
            
          )%>% 
          as.data.frame()
          
            data_b$rides_pDr = data_b$rides/ data_b$available_drivers
            data_b$revenue_pDr = data_b$revenue/ data_b$available_drivers
            data_b$supply_pDr = data_b$Supply_hours/ data_b$available_drivers
            data_b$utilisation = data_b$Active_hours/ data_b$Supply_hours
            
    
            kpis_list = c ('rides_pDr','revenue_pDr','supply_pDr','utilisation')
    
    #summary(data_a)
    
    for (i in 1: length(kpis_list)) {
      
      data_all = data_a[c('Date',kpis_list[i])]
      dt_all = read.zoo(data_all, format = " %Y-%m-%d") 
      
      data_all_2 = data_b[c('Date',kpis_list[i])]
      dt_all_2 = read.zoo(data_all_2, format = " %Y-%m-%d") 
      
      pre.period <- as.Date(c("2022-05-06", "2022-06-06"))
      post.period <- as.Date(c("2022-06-07", "2022-06-28"))
      
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

#output_1_a <- data.frame(level_0_segment,segmentationL,kpi,mean_observed,mean_predicted,rel_effect,p_value,flag_segment)
output_2 <- data.frame(level_0_segment,segmentationL,kpi,mean_observed,mean_predicted,rel_effect,p_value,flag_segment)







#### Treatment Zones
level_0_segment_name = "Service_separately_Beat_only"

core= subset(data, data$service_name == 'other')

segmentation <-  list(segment1 = c('flag_taxi')
                     # ,segment2 = c('flag_taxi','service_name')
                      #   ,segment3 = c('zone')
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
    
    data_1= segment_input
    
    data_a <- data_1 %>%
      group_by(Date) %>%
      dplyr::summarise(
        rides = sum(rides),
        available_drivers = sum(available_drivers),
        revenue = sum(revenue),
        Supply_hours = sum(Supply_hours),
        Active_hours = sum(Active_hours)
        
      )%>% 
      as.data.frame()
    
    data_a$rides_pDr = data_a$rides/ data_a$available_drivers
    data_a$revenue_pDr = data_a$revenue/ data_a$available_drivers
    data_a$supply_pDr = data_a$Supply_hours/ data_a$available_drivers
    data_a$utilisation = data_a$Active_hours/ data_a$Supply_hours
    
    
    kpis_list = c ('rides_pDr','revenue_pDr','supply_pDr','utilisation')
    
    
    
    #summary(data_a)
    
    for (i in 1: length(kpis_list)) {
      
      data_all = data_a[c('Date',kpis_list[i])]
      dt_all = read.zoo(data_all, format = " %Y-%m-%d") 
      
      pre.period <- as.Date(c("2022-05-06", "2022-06-06"))
      post.period <- as.Date(c("2022-06-07", "2022-06-28"))
      
      impact_c <- CausalImpact(dt_all, pre.period, post.period, model.args = list(niter = 5000, nseasons = 7))
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

#output_1_a <- data.frame(level_0_segment,segmentationL,kpi,mean_observed,mean_predicted,rel_effect,p_value,flag_segment)
output_3 <- data.frame(level_0_segment,segmentationL,kpi,mean_observed,mean_predicted,rel_effect,p_value,flag_segment)





#### Treatment Zones
level_0_segment_name = "Corrected_onNonTaxi_drivers_BeatOnly"

core= subset(data, data$service_name == 'other')

segmentation <-  list(segment1 = c('total')
                      #,segment2 = c('service_name')
                      #   ,segment3 = c('zone')
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
    
    data_1= subset(segment_input, segment_input$flag_taxi %in% c('not_taxi') )# control hours
    data_2 = subset(segment_input, segment_input$flag_taxi == 'taxi')
    
    
    
    data_a <- data_1 %>%
      group_by(Date) %>%
      dplyr::summarise(
        rides = sum(rides),
        available_drivers = sum(available_drivers),
        revenue = sum(revenue),
        Supply_hours = sum(Supply_hours),
        Active_hours = sum(Active_hours)
        
      )%>% 
      as.data.frame()
    
    data_a$rides_pDr = data_a$rides/ data_a$available_drivers
    data_a$revenue_pDr = data_a$revenue/ data_a$available_drivers
    data_a$supply_pDr = data_a$Supply_hours/ data_a$available_drivers
    data_a$utilisation = data_a$Active_hours/ data_a$Supply_hours
    
    
    
    
    data_b <- data_2 %>%
      group_by(Date) %>%
      dplyr::summarise(
        rides = sum(rides),
        available_drivers = sum(available_drivers),
        revenue = sum(revenue),
        Supply_hours = sum(Supply_hours),
        Active_hours = sum(Active_hours)
        
      )%>% 
      as.data.frame()
    
    data_b$rides_pDr = data_b$rides/ data_b$available_drivers
    data_b$revenue_pDr = data_b$revenue/ data_b$available_drivers
    data_b$supply_pDr = data_b$Supply_hours/ data_b$available_drivers
    data_b$utilisation = data_b$Active_hours/ data_b$Supply_hours
    
    
    kpis_list = c ('rides_pDr','revenue_pDr','supply_pDr','utilisation')
    
    #summary(data_a)
    
    for (i in 1: length(kpis_list)) {
      
      data_all = data_a[c('Date',kpis_list[i])]
      dt_all = read.zoo(data_all, format = " %Y-%m-%d") 
      
      data_all_2 = data_b[c('Date',kpis_list[i])]
      dt_all_2 = read.zoo(data_all_2, format = " %Y-%m-%d") 
      
      pre.period <- as.Date(c("2022-05-06", "2022-06-06"))
      post.period <- as.Date(c("2022-06-07", "2022-06-28"))
      
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

#output_1_a <- data.frame(level_0_segment,segmentationL,kpi,mean_observed,mean_predicted,rel_effect,p_value,flag_segment)
output_4 <- data.frame(level_0_segment,segmentationL,kpi,mean_observed,mean_predicted,rel_effect,p_value,flag_segment)





output = rbind(output_1,output_2,output_3,output_4)


#write.table(output,"/Users/r.nacheva/Downloads/PED_CL_Timeseries_220322.csv", sep=',', dec='.',row.names=F)
write.table(output,"/Users/r.nacheva/Downloads/Taxi_TimeSeries.csv", sep=',', dec='.',row.names=F)
