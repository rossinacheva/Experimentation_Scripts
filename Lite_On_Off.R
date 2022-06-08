

#install.packages("devtools")
#devtools::install_github("frankportman/bayesAB", build_vignettes = TRUE)

#install.packages("googlesheets4")
#library("googlesheets4")



#library(bayesAB)
lapply(list('plyr','dplyr','boot','lubridate'), library, character.only = TRUE)


starttime = Sys.time()

##########################################
### Parameters
##########################################


data_a=read.csv("/Users/r.nacheva/Downloads/GO_weekends_2022_06_08.csv") # All
data_b=read.csv("/Users/r.nacheva/Downloads/GO_weekends_2022_06_08 (1).csv") # All
data_c=read.csv("/Users/r.nacheva/Downloads/GO_weekends_2022_06_08 (3).csv") # All
data_d=read.csv("/Users/r.nacheva/Downloads/GO_weekends_2022_06_08 (2).csv") # All


data = rbind(data_a,data_b,data_c,data_d)
d = subset(data, data$zone == 'Lima Moderna' & data$service_name == 'Lite')
e= subset(data, !(data$service_name %in% c('GO','Lite')) &
                 data$zone %in% c('Lima Centro','Lima Norte','Lima Este','Lima Moderna','Callao','Lima Sur', 'Lima Moderna','All'))

data = rbind(d,e)
data = subset(data,!( data$zone %in% c('Callao') & data$service_name %in% c('Luxi')))

data=data[!(is.na(data$zone) | data$zone=="" ),] 
data=data[!(is.na(data$service_name) | data$service_name=="" ),] 

data$day <- as.Date(data$day,format = "%Y-%m-%d") 
data$week_num = strftime(data$day, format = "%V") # Add weekl number


data$test_version = ifelse(data$hour %in% c('0','1','2','6','7','8','12','13','14','18','19','20') 
                           &  data$day %in% c(as.Date('2022-05-28'),as.Date('2022-05-29')), 'Off'
                           ,ifelse(data$hour %in% c('0','1','2','6','7','8','12','13','14','18','19','20') 
                                   &    data$day %in% c(as.Date('2022-06-04'),as.Date('2022-06-05')), 'On'
                                   ,ifelse(!(data$hour %in% c('0','1','2','6','7','8','12','13','14','18','19','20'))  
                                           &      data$day %in% c(as.Date('2022-05-28'),as.Date('2022-05-29')), 'On'
                                           ,ifelse(!(data$hour %in% c('0','1','2','6','7','8','12','13','14','18','19','20')) 
                                                   &     data$day %in% c(as.Date('2022-06-04'),as.Date('2022-06-05')), 'Off'
                                                   
                                                   ,""))))
input = subset(data, data$test_version %in% c('On','Off'))


mm = subset (input, input$service_name == 'Lite')


summary(input)
names(input)
head(input)

# Define segmentation : COUNTRY, SEASON, Total
segmentation <-  list(segment1 = c('zone','service_name')
                      ,segment2 = c('zone','service_name','week_num')
                      #    = c('service_name')
                      #  ,segment3 = c('total')
)


# Define your entity
entity <- 'hour'




kpis <-c('EtRC','EtC','Rev_pE','PickupRate','Utilisation','Drivers_earnings',
         'Broadcast_pDr','Avail_2_active','AccR','Rides_pDr','Supply_pDr','sess_eye','available_drivers'
        
)


# Define Output file name
#outputname <- 'Balancing.csv'


# Turn back to numeric values
col_factor <- sapply(input, is.factor)
input[col_factor] <- lapply(input[col_factor], as.character)
num_cols <- sapply(input, is.numeric)
input[,num_cols][is.na(input[,num_cols])] <- 0


input[,col_factor][is.na(input[,col_factor])] <- 0

size_treatment = c()
size_control = c()
SegmentName = c()
KPI = c()
mean_treatment = c()
mean_control = c()
rel_effect = c()
confidence_interval = c()
pValue = c()
Significant = c()
Baysian_probability = c()

n = 1000
alpha = 0.05


# loop through segmentation levels
for (level in 1:length(segmentation)){
  
  if (tolower(segmentation[level][[1]][1]) == "total"){
    segment_input <- input
    combinations = 1
  }
  else {
    unique_combinations <- unique(input[,segmentation[level][[1]], drop = FALSE])
    # filter out missing values
    unique_combinations <- na.omit(unique_combinations)
    combinations = nrow(unique_combinations)
  }
  
  for(i_comb in 1:combinations){
    
    if (tolower(segmentation[level][[1]][1]) == "total"){
      segment_input <- input
    }
    else {
      segment_input <- merge(unique_combinations[i_comb,,drop = FALSE], input)
      
    }
    segmentName = ifelse(tolower(segmentation[level][[1]][1]) == "total", "Total", paste(as.character(as.vector(unique_combinations[i_comb,])), collapse = "_"))
    print(segmentName)
    
    
    
    #Draw random sample from population for Treatment
    #     Treatment <- subset (segment_input, segment_input$exp_group_name== 'test_with_comms')
    Treatment <- subset (segment_input, segment_input$test_version== 'Off')
    #delete randomly drawn data from population
    #segment_input_1 <- segment_input[!(segment_input$ID %in% Treatment$ID),]
    #Draw random sample from population for Control
    Control <- subset (segment_input, segment_input$test_version == 'On')
    # Summarize results on entity level & add assigment and segment to data sets      
    Treatment<-summarise_all(group_by_(Treatment[c(entity,names(Treatment)[sapply(Treatment, is.numeric)])],.dots = entity),funs(sum))
    Control<-summarise_all(group_by_(Control[c(entity,names(Control)[sapply(Control, is.numeric)])],.dots = entity), funs(sum))
    Treatment$Segment<- 'Treatment'
    Control$Segment<-'Control'
    Control$SegmentName <- segmentName
    Treatment$SegmentName <- segmentName
    # Update matrix to check the while condition      
    
    # calculate for minimum mean difference and Wilcox test   
    for (i in 1: length(kpis)) {
      

      sess_eye_tre = sum(Treatment$sess_eye)
      sess_eye_con = sum(Control$sess_eye)
      requests_tre = sum(Treatment$sess_requests)
      requests_con = sum(Control$sess_requests)
      
      supply_tre = sum(Treatment$Supply_hours)
      supply_con = sum(Control$Supply_hours)
      av_drivers_tre = sum(Treatment$available_drivers)
      av_drivers_con = sum(Control$available_drivers)
      broadcasts_tre = sum(Treatment$broadcasts_total)
      broadcasts_con = sum(Control$broadcasts_total)
 
     Treatment$EtRC = (Treatment$sess_requests/sess_eye_tre)*nrow(Treatment)
      Control$EtRC = (Control$sess_requests/sess_eye_con)*nrow(Control)
      Treatment$Rev_pE = (Treatment$revenue/sess_eye_tre)*nrow(Treatment)
      Control$Rev_pE = (Control$revenue/sess_eye_con)*nrow(Control)
      Treatment$PickupRate = (Treatment$rides/requests_tre)*nrow(Treatment)
      Control$PickupRate = (Control$rides/requests_con)*nrow(Control)
      Treatment$EtC = (Treatment$rides/sess_eye_tre)*nrow(Treatment)
      Control$EtC = (Control$rides/sess_eye_con)*nrow(Control)
      Treatment$Utilisation = (Treatment$Active_hours/supply_tre)*nrow(Treatment)
      Control$Utilisation = (Control$Active_hours/supply_con)*nrow(Control)
      Treatment$Drivers_earnings = (Treatment$dr_earnings/av_drivers_tre)*nrow(Treatment)
      Control$Drivers_earnings = (Control$dr_earnings/av_drivers_con)*nrow(Control)
      Treatment$Broadcast_pDr = (Treatment$broadcasts_total/av_drivers_tre)*nrow(Treatment)
      Control$Broadcast_pDr = (Control$broadcasts_total/av_drivers_con)*nrow(Control)
      Treatment$Avail_2_active = (Treatment$active_drivers/av_drivers_tre)*nrow(Treatment)
      Control$Avail_2_active = (Control$active_drivers/av_drivers_con)*nrow(Control)
          Treatment$AccR = (Treatment$acc/broadcasts_tre)*nrow(Treatment)
          Control$AccR = (Control$acc/broadcasts_con)*nrow(Control)
      
 
      
      Treatment$Rides_pDr = (Treatment$rides/av_drivers_tre)*nrow(Treatment)
      Control$Rides_pDr = (Control$rides/av_drivers_con)*nrow(Control)
      Treatment$Supply_pDr = (Treatment$Supply_hours/av_drivers_tre)*nrow(Treatment)
      Control$Supply_pDr = (Control$Supply_hours/av_drivers_con)*nrow(Control)
      
    
      
      ##### Add bootstrao if not normal distribution ############
      
      test <- t.test(Treatment[[kpis[i]]], Control[[kpis[i]]],
                     var.equal = FALSE, alternative = "two.sided"
                                         
      )
      
      size_treatment_a = nrow(Treatment)
      size_control_a = nrow(Control)
      SegmentName_a = segmentName
      KPI_a = kpis[i]
      mean_treatment_a = mean(Treatment[[kpis[i]]])
      mean_control_a = mean(Control[[kpis[i]]])
      rel_effect_a = (mean(Treatment[[kpis[i]]]) - mean(Control[[kpis[i]]]))/mean(Control[[kpis[i]]])
      confidence_interval_a = paste ('(',round(test$conf.int[1],3),' ; ',round(test$conf.int[2],3),')',sep ='')
      pValue_a = round(test$p.value,3)
      Significant_a = ifelse(test$conf.int[1]*test$conf.int[2]>0, 'YES', 'NO')
      
      #Baysian_probability_a = summary(AB1)$probability$Probability 
      
      
      size_treatment = rbind(size_treatment,size_treatment_a)
      size_control = rbind(size_control,size_control_a)
      KPI = rbind(KPI,KPI_a)
      SegmentName = rbind(SegmentName,SegmentName_a)
      mean_treatment = rbind(mean_treatment,mean_treatment_a)
      mean_control = rbind(mean_control,mean_control_a)
      rel_effect = rbind(rel_effect,rel_effect_a)
      confidence_interval = rbind(confidence_interval,confidence_interval_a)
      pValue = rbind(pValue,pValue_a)
      Significant = rbind(Significant,Significant_a)
      #Baysian_probability = rbind(Baysian_probability,Baysian_probability_a)
    }
    
  }
}

output <- data.frame(SegmentName,KPI,size_treatment, size_control,mean_treatment,mean_control,rel_effect,confidence_interval,pValue,Significant) #,Baysian_probability)


endtime = Sys.time() - starttime
endtime

write.table(output,"/Users/r.nacheva/Downloads/Test_Results_Lite_Removal_WE_PE_Moderna_DirectCom.csv", sep=',', dec='.',row.names=F)




########

level_0_segment_name = "Moderna_to_other"


data$Date <- as.Date(data$day,format = "%Y-%m-%d") 

data_1= subset(data, data$zone != 'Lima Moderna' & data$zone != 'All' & data$service_name == 'ALl')# control group

core= subset(data, data$zone == 'Lima Moderna' &  data$test_version != 'On' & data$service_name == 'ALl') # tre group

segmentation <-  list(segment1 = c('total')
                      # ,segment2 = c('zone')
                      
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
                       acc = sum(acc))%>% 
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
      
      data_all = data_a[c('Date',kpis_list[i])]
      data_all2 = data_b[c('Date',kpis_list[i])]
      
      dt_all = read.zoo(data_all, format = " %Y-%m-%d") 
      dt_all2 = read.zoo(data_all2, format = " %Y-%m-%d") 
      
      pre.period <- as.Date(c("2022-04-01", "2022-05-27"))
      post.period <- as.Date(c("2022-05-28", "2022-06-06"))
      
      impact_c <- CausalImpact(cbind(dt_all2,dt_all), pre.period, post.period, model.args = list(niter = 5000, nseasons = 2))
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

#output_3_a <- data.frame(level_0_segment,segmentationL,kpi,mean_observed,mean_predicted,rel_effect,p_value,flag_segment)

output_a <- data.frame(level_0_segment,segmentationL,kpi,mean_observed,mean_predicted,rel_effect,p_value,flag_segment)


write.table(output_a,"/Users/r.nacheva/Downloads/TimeSeries_Correction_NonLimaModerna.csv", sep=',', dec='.',row.names=F)

