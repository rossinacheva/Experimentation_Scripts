#### Install and load needed packages
wants <- c("plyr", "dplyr", "boot", "lubridate", "samplesize","pwr")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
lapply(wants, library, character.only=T)


##########################################
### Parameters
##########################################



input_b = read.csv("/Users/r.nacheva/Downloads/Lite_On_Off_Experiment_-_KPI_Tracking_RN_2022_10_27.csv") # service level
input_a = read.csv("/Users/r.nacheva/Downloads/Lite_On_Off_Experiment_-_KPI_Tracking_RN_totals_2022_10_27.csv") # totals

input = rbind(input_a,input_b)


input$hour_r = as.numeric(strftime(input$hour, format = "%H"))

input = subset(input, !(input$hour_r %in% c(8,9,10,11))) #exclude hours where lite is on for both treatment and control
input = subset(input, input$hour >= '2022-10-24 12:00:00.000' & input$hour < '2022-11-07 12:00:00.000' ) # exclude pre and post test data


summary(input)
names(input)
head(input)


# Define segmentation : type total fro no segmentation (total level)
segmentation <-  list(segment1 = c('id_product')
                      ,segment2 = c('zone','id_product')
                      
                      
)


# Define your entity
entity <- 'hour'

# Create addittional columns (only sum or multiply)


# Create ratio KPIs: 

kpi_name = c( 'EtRC','EtC','Rev_pEye','PuR','Rev_pRide','AvgFare_ride','Dr_earnings','Utilisation','AccR','Broadcasts_pDriver')  # same name as inserted in the balancing
kpi_nominator = c('sess_requests','total_rides','Revenue','total_rides','Revenue','total_fare_ride','dr_earnings','On_Ride','acc','broadcasts') # same as in data set
kpi_denominator = c('sess_eyeballs','sess_eyeballs','sess_eyeballs','sess_requests','total_rides','total_rides','available_drivers','Supply_Hours','broadcasts','available_drivers') # same as in data set

#### Check your KPIS
kpi = cbind (kpi_name,kpi_nominator,kpi_denominator)
kpi = as.data.frame(kpi)
kpi
######


# KPI list for the analysis
kpis <-c('EtRC','EtC','Rev_pEye','PuR','Rev_pRide','AvgFare_ride','Dr_earnings','Utilisation','AccR','Broadcasts_pDriver','sess_eyeballs'
)

# Define column name where you stored the assigment (treatment or control)
assignment = 'variant'

# Define the name of your Treatment and Control group
Treatment_name = 'Treatment' #  'tired_from8'
#Treatment_name = 'tired_from8' #  'tired_from8'
Control_name = 'Control'

# Set up you confidence interval (95% as a standard recommendation)
conf_int = 0.95

# Define Output file name
outputname <- '/Users/r.nacheva/Downloads/Lite_on_off_CO.csv'

########## End manual Input


starttime = Sys.time()

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
    Treatment <- subset (segment_input, segment_input[[assignment]]== Treatment_name)
    #delete randomly drawn data from population
    #segment_input_1 <- segment_input[!(segment_input$ID %in% Treatment$ID),]
    #Draw random sample from population for Control
    Control <- subset (segment_input, segment_input[assignment] == Control_name)
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
      
      for (kp in 1:nrow(kpi)) {
        denom_tre = sum(Treatment[[kpi_denominator[kp]]])
        denom_con = sum(Control[[kpi_denominator[kp]]])
        Treatment[[kpi_name[kp]]] = (Treatment[[kpi_nominator[kp]]]/denom_tre)*nrow(Treatment)
        Control[[kpi_name[kp]]] = (Control[[kpi_nominator[kp]]]/denom_con)*nrow(Control)
      }
      
      
      ##### t.test ############
      
      test <- t.test(Treatment[[kpis[i]]], Control[[kpis[i]]],
                     var.equal = FALSE, alternative = "two.sided", conf.int = conf_int
                     
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


