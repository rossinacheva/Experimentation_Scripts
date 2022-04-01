

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




input_1 <- read.csv("/Users/r.nacheva/Downloads/DaIly_Hourly_view_2022_04_01.csv")
input_2 <- read.csv("/Users/r.nacheva/Downloads/2121129_2022_04_01.csv")

input_3 <- read.csv("/Users/r.nacheva/Downloads/2121130_2022_04_01.csv")
input_4 <- read.csv("/Users/r.nacheva/Downloads/2121131_2022_04_01.csv")


data <-rbind(input_1,input_2,input_3,input_4)
data=data[!(is.na(data$zone) | data$zone=="" ),] 
data=data[!(is.na(data$service_name) | data$service_name=="" ),] 
data= subset(data, !(data$service_name == 'GO') & toupper(data$zone )%in% c('TOTAL','LIMA MODERNA','LIMA CENTRO','LIMA SUR','LIMA SUR'
                                                                          ,'LIMA ESTE','LIMA NORTE','CALLAO'
))
data$day <- as.Date(data$day,format = "%Y-%m-%d") 

data$test_version = ifelse(data$hour %in% c('0','1','2','6','7','8','12','13','14','18','19','20') 
                           &  data$day %in% c(as.Date('2022-03-19'),as.Date('2022-03-20')), 'On'
                           ,ifelse(data$hour %in% c('0','1','2','6','7','8','12','13','14','18','19','20') 
                                     &    data$day %in% c(as.Date('2022-03-26'),as.Date('2022-03-27')), 'Off'
                                   ,ifelse(!(data$hour %in% c('0','1','2','6','7','8','12','13','14','18','19','20'))  
                                             &      data$day %in% c(as.Date('2022-03-19'),as.Date('2022-03-20')), 'Off'
                                           ,ifelse(!(data$hour %in% c('0','1','2','6','7','8','12','13','14','18','19','20')) 
                                                     &     data$day %in% c(as.Date('2022-03-26'),as.Date('2022-03-27')), 'On'
                                                   
                                                   ,""))))
input = data


summary(input)
names(input)
head(input)

# Define segmentation : COUNTRY, SEASON, Total
segmentation <-  list(segment1 = c('zone','service_name')
                      #segmentation <-  list(segment1 = c('COUNTRY','SEASON')
                      #   ,segment2 = c('service_name')
                      #  ,segment3 = c('total')
)


# Define your entity
entity <- 'hour'




kpis <-c('EtRC','EtC','Rev_pE','PickupRate','Utilisation','Drivers_earnings',
         'Broadcast_pDr','Avail_2_active','AccR','Rides_pDr','Supply_pDr'
        
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
                     
                     
                     #test <- t.test(subset(Treatment[[kpis[i]]], Treatment[[kpis[i]]] >0) , subset(Control[[kpis[i]]], Control[[kpis[i]]] >0) ,
                     #              var.equal = FALSE, alternative = "two.sided"                
                     
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

write.table(output,"/Users/r.nacheva/Downloads/Test_Results_Lite_Removal_WE_PE_220401_v4.csv", sep=',', dec='.',row.names=F)







