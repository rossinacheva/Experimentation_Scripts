

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


input <- read.csv("/Users/r.nacheva/Downloads/Experimentation_Radius_Nov_Check_2021_11_16.csv")


summary(input)

names(input)

head(input)

# Define segmentation : COUNTRY, SEASON, Total
segmentation <-  list(segment1 = c('total')
                      ,segment2 = c('starting_segment')
                      #  ,segment3 = c('total')
)


# Define your entity
entity <- 'id_driver'



# Main KPIs

kpis <- c('Supply_hours','Active_hours','utilisation','active_days_period','avg_fare',
          'driver_earning','pax_cancelation','Rides','Revenue','ride_total_distance','acc_rate','ign_rate',
          'broadcast','Broadcasts_pSupplyH','eta','acc_rate_3km','acc_rate_2km','share_acc_3km','share_acc_2km',
          'ign_rate_3km','ign_rate_2km','share_ign_3km','share_ign_2km','share_broadcast_3km','share_broadcast_2km'
)

# Define Output file name
outputname <- '/Users/r.nacheva/Downloads/Test_Results_PE_Nov_211116_v2.csv'


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
    Treatment <- subset (segment_input, segment_input$variant == 'Treatment')
    #delete randomly drawn data from population
    #segment_input_1 <- segment_input[!(segment_input$ID %in% Treatment$ID),]
    #Draw random sample from population for Control
    Control <- subset (segment_input, segment_input$variant == 'Control')
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
      
      
      broadcasts_tre = sum(Treatment$broadcast)
      broadcasts_con = sum(Control$broadcast)
      request_upto_3km_tre = sum(Treatment$request_upto_3km)
      request_upto_3km_con = sum(Control$request_upto_3km)
      request_upto_2km_tre = sum(Treatment$request_upto_2km)
      request_upto_2km_con = sum(Control$request_upto_2km)
      acc_tre = sum(Treatment$acc)
      acc_con = sum(Control$acc)
      ignored_tre = sum(Treatment$ignored)
      ignored_con = sum(Control$ignored)
      Supply_hours_tre = sum(Treatment$Supply_hours)
      Supply_hours_con = sum(Control$Supply_hours)
      pax_cancelation_tre = sum(Treatment$Rides)+sum(Treatment$pax_can)
      pax_cancelation_con = sum(Control$Rides)+sum(Control$pax_can)
      
      Treatment$acc_rate = (Treatment$acc/broadcasts_tre)*nrow(Treatment)
      Control$acc_rate = (Control$acc/broadcasts_con)*nrow(Control)
      Treatment$ign_rate = (Treatment$ignored/broadcasts_tre)*nrow(Treatment)
      Control$ign_rate = (Control$ignored/broadcasts_con)*nrow(Control)     
      Treatment$acc_rate_3km = (Treatment$acc_upto_3km/request_upto_3km_tre)*nrow(Treatment)
      Control$acc_rate_3km = (Control$acc_upto_3km/request_upto_3km_con)*nrow(Control) 
      Treatment$acc_rate_2km = (Treatment$acc_upto_2km/request_upto_2km_tre)*nrow(Treatment)
      Control$acc_rate_2km = (Control$acc_upto_2km/request_upto_2km_con)*nrow(Control) 
      Treatment$share_acc_3km = (Treatment$acc_upto_3km/acc_tre)*nrow(Treatment)
      Control$share_acc_3km = (Control$acc_upto_3km/acc_con)*nrow(Control) 
      Treatment$share_acc_2km = (Treatment$acc_upto_2km/acc_tre)*nrow(Treatment)
      Control$share_acc_2km = (Control$acc_upto_2km/acc_con)*nrow(Control) 
      Treatment$ign_rate_3km = (Treatment$ignored_upto_3km/request_upto_3km_tre)*nrow(Treatment)
      Control$ign_rate_3km = (Control$ignored_upto_3km/request_upto_3km_con)*nrow(Control) 
      Treatment$ign_rate_2km = (Treatment$ignored_upto_2km/request_upto_2km_tre)*nrow(Treatment)
      Control$ign_rate_2km = (Control$ignored_upto_2km/request_upto_2km_con)*nrow(Control) 
      
      Treatment$share_ign_3km = (Treatment$ignored_upto_3km/ignored_tre)*nrow(Treatment)
      Control$share_ign_3km = (Control$ignored_upto_3km/ignored_con)*nrow(Control) 
      Treatment$share_ign_2km = (Treatment$ignored_upto_2km/ignored_tre)*nrow(Treatment)
      Control$share_ign_2km = (Control$ignored_upto_2km/ignored_con)*nrow(Control) 
      
      Treatment$share_broadcast_3km = (Treatment$request_upto_3km/broadcasts_tre)*nrow(Treatment)
      Control$share_broadcast_3km = (Control$request_upto_3km/broadcasts_con)*nrow(Control)
      Treatment$share_broadcast_2km = (Treatment$request_upto_2km/broadcasts_tre)*nrow(Treatment)
      Control$share_broadcast_2km = (Control$request_upto_2km/broadcasts_con)*nrow(Control)
      Treatment$utilisation = (Treatment$Active_hours/Supply_hours_tre)*nrow(Treatment)
      Control$utilisation = (Control$Active_hours/Supply_hours_con)*nrow(Control)
      Treatment$pax_cancelation = (Treatment$pax_can/pax_cancelation_tre)*nrow(Treatment)
      Control$pax_cancelation = (Control$pax_can/pax_cancelation_con)*nrow(Control)
      
      Treatment$Broadcasts_pSupplyH = (Treatment$broadcast/Supply_hours_tre)*nrow(Treatment)
      Control$Broadcasts_pSupplyH = (Control$broadcast/Supply_hours_con)*nrow(Control)
      
      
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

write.table(output,outputname, sep=',', dec='.',row.names=F)







