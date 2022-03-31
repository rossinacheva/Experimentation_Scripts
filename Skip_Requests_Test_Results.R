

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




input_1 <- read.csv("/Users/r.nacheva/Downloads/Skip_Request_2022_03_31.csv")
input_2 <- read.csv("/Users/r.nacheva/Downloads/2756841_2022_03_31.csv")
input_3 <- read.csv("/Users/r.nacheva/Downloads/1222428_2022_03_31.csv")
input_4 <- read.csv("/Users/r.nacheva/Downloads/2117355_2022_03_31.csv")
input_5 <- read.csv("/Users/r.nacheva/Downloads/4418778_2022_03_31.csv")


input <-rbind(input_1,input_2,input_3,input_4,input_5)

#input= subset(input_3, input_3$acc > 0)



summary(input)

names(input)

head(input)

# Define segmentation : COUNTRY, SEASON, Total
segmentation <-  list(segment1 = c('market')
                      #segmentation <-  list(segment1 = c('COUNTRY','SEASON')
                  #    ,segment2 = c('period','starting_segment')
                      #  ,segment3 = c('total')
)


# Define your entity
entity <- 'id_driver'




kpis <-c('RejRate','AccR','IgnR','rejected','accepted','ignored','total_broadcasts','Time_to_acc','Time_to_rej','rides','revenue',
            'Pass_CancR','Dr_CancR','availability'
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
  Treatment <- subset (segment_input, segment_input$exp_group_name== 'test_no_comms')
    #delete randomly drawn data from population
    #segment_input_1 <- segment_input[!(segment_input$ID %in% Treatment$ID),]
    #Draw random sample from population for Control
    Control <- subset (segment_input, segment_input$exp_group_name == 'control')
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
      
      
    
      broadcasts_tre = sum(Treatment$total_broadcasts)
      broadcasts_con = sum(Control$total_broadcasts)
      acc_tre = sum(Treatment$accepted)
      acc_con = sum(Control$accepted)
      rej_tre = sum(Treatment$rejected)
      rej_con = sum(Control$rejected)
      request_tre = sum(Treatment$rides) + sum(Treatment$pass_cancelled_ride) + sum(Treatment$dr_cancelled_ride)
      request_con = sum(Control$rides) + sum(Control$pass_cancelled_ride) + sum(Control$dr_cancelled_ride)
      
      
      Treatment$AccR = (Treatment$accepted/broadcasts_tre)*nrow(Treatment)
      Control$AccR = (Control$accepted/broadcasts_con)*nrow(Control)
      Treatment$RejRate = (Treatment$rejected/broadcasts_tre)*nrow(Treatment)
      Control$RejRate = (Control$rejected/broadcasts_con)*nrow(Control)
      Treatment$IgnR = (Treatment$ignored/broadcasts_tre)*nrow(Treatment)
      Control$IgnR = (Control$ignored/broadcasts_con)*nrow(Control)     
      Treatment$Time_to_acc = (Treatment$sum_time_to_accept/acc_tre)*nrow(Treatment)
      Control$Time_to_acc = (Control$sum_time_to_accept/acc_con)*nrow(Control)     
      Treatment$Time_to_rej = (Treatment$sum_time_to_reject/rej_tre)*nrow(Treatment)
      Control$Time_to_rej = (Control$sum_time_to_reject/rej_con)*nrow(Control)     
      Treatment$Pass_CancR = (Treatment$pass_cancelled_ride/request_tre)*nrow(Treatment)
      Control$Pass_CancR = (Control$pass_cancelled_ride/request_con)*nrow(Control)

      Treatment$Dr_CancR = (Treatment$dr_cancelled_ride/request_tre)*nrow(Treatment)
      Control$Dr_CancR = (Control$dr_cancelled_ride/request_con)*nrow(Control)
      
      
      
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

write.table(output,"/Users/r.nacheva/Downloads/Test_Results_SkipRequests_NoComms_223031.csv", sep=',', dec='.',row.names=F)







