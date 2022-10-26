
lapply(list('plyr','dplyr','boot','lubridate','samplesize'), library, character.only = TRUE)



##########################################
### Parameters
##########################################

#setwd('C:/Users/rnacheva/Desktop/ASF_ABTest/Transformers_May_2019/Preliminary')

#sku<-(read.csv2('Initial_Stock_Correct.csv', sep=",", na.strings = "-",dec='.'))

#sku_eligible<-(read.csv2('Clean_Ready_forRebalancing_New.csv', sep=",", na.strings = "-",dec='.'))
#sku_eligible<-subset(sku_eligible, sku_eligible$Final_Stock_ok ==1)

#df_sales_data<-(read.csv2('ExaSol_Balancing_190510.csv', sep=",", na.strings = "-",dec='.'))

#input<-(read.csv2('ASF_Stock_before.csv', sep=",", na.strings = "-",dec='.'))

input<- read.csv("/Users/r.nacheva/Downloads/Lite ABC Commission Test - Target- 4WE Performance.csv")

summary(input)



# Define segmentation : COUNTRY, SEASON, Total
segmentation <-  list(segment1 = c('total')
                      #segmentation <-  list(segment1 = c('COUNTRY','SEASON')
                      #   ,segment2 = c('COUNTRY','SEASON')
                      #  ,segment3 = c('total')
)


# Define your entity
entity <- 'id_driver'

input$bro = input$Broadcast-input$pending

# Main KPIs you want to balance on and take into concideration for outlier detection
#balancing <- c('Supply_hours', 'Active_hours','available_drivers','Rides','Revenue','acceptance_rate','avg_fare')
balancing <- c('Rides','HoursAvailable','Earnings','Acc','bro')

# Define number of buckets (only 2 for now)
buckets = 2

# Parameter for Wilcox test & outlier detection
mean_difference = 0.2
outlier_threshold = 0.999

# Define Output file name
outputname <- 'Balancing.csv'


############## End manual input

#df_sales_data$match <-paste(df_sales_data$CSKU , df_sales_data$COUNTRY)
#sku$match <-paste(sku$csku , sku$country)

# merge all data sets
#input<- merge (sku,df_sales_data,  by = c("CSKU"), all.x = TRUE)
#input$DR<-input$GROSS_DISC/input$GROSS_SALES_BEF_DISC

# Generate random numbers as preparation for balancing
input$ID <- seq.int(nrow(input))



# Create empty datasets
All <-c()
Outliers_Total <-c()

# Turn back to numeric values
col_factor <- sapply(input, is.factor)
input[col_factor] <- lapply(input[col_factor], as.character)
num_cols <- sapply(input, is.numeric)
input[,num_cols][is.na(input[,num_cols])] <- 0


input[,col_factor][is.na(input[,col_factor])] <- 0






# Sample size calculation --> not included for now 
mean_sample <- mean(input[[balancing[1]]])
st_dev_sample <- sd(input[[balancing[1]]])

sample_size_needed <- n.ttest(power = 0.8, alpha = 0.05, mean.diff = mean_sample, sd1 = st_dev_sample, sd2 = st_dev_sample,
                              k = 1, design = "unpaired", fraction = "balanced", variance = "equal")
sample_size_needed

sample_size_needed_1<-(((1.96+0.842)^2*st_dev_sample^2*(1+2))/(mean_sample^2))/(0.2^2)

sample_size_needed_1

n = 1000
alpha = 0.05

mean_sample
st_dev_sample 

# Remove outliers from total data set 
for (i in 1: length(balancing)) {
  Outlier_1KPI <-subset(input, input[[balancing[i]]] > quantile(input[[balancing[i]]], outlier_threshold, na.rm = TRUE))
  
  Outliers_Total <-rbind(Outliers_Total,Outlier_1KPI)
}

input<-anti_join(input, Outliers_Total, by = entity)  

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
    
    
    
    # Prepare for balancing loop    
    score_mean <- c(1) 
    a_score_mean <- c(1)
    b_score_mean <- c(1)
    c_score_mean <- c(1)
    score_pValue <- c(1) 
    a_score_pValue <- c(1) 
    b_score_pValue <- c(1) 
    c_score_pValue <- c(1) 
    score<-rbind(score_mean,score_pValue)
    
    while (sum(score > mean_difference) > 0)
    {  
      
      
      #Draw random sample from population for Treatment
      Treatment_1 <- segment_input[sample(1:nrow(segment_input), 0.33*nrow(segment_input)),]
      #delete randomly drawn data from population
      segment_input_1 <- segment_input[!(segment_input$ID %in% Treatment_1$ID),]
      
      Treatment_2 <- segment_input_1[sample(1:nrow(segment_input_1), 0.5*nrow(segment_input_1)),]
      #Draw random sample from population for Control
      Control <- segment_input_1[!(segment_input_1$ID %in% Treatment_2$ID),]
      # Summarize results on entity level & add assigment and segment to data sets      
      Treatment_1<-summarise_all(group_by_(Treatment_1[c(entity,names(Treatment_1)[sapply(Treatment_1, is.numeric)])],.dots = entity),funs(sum))
      Treatment_2<-summarise_all(group_by_(Treatment_2[c(entity,names(Treatment_2)[sapply(Treatment_2, is.numeric)])],.dots = entity),funs(sum))
      
      Control<-summarise_all(group_by_(Control[c(entity,names(Control)[sapply(Control, is.numeric)])],.dots = entity), funs(sum))
      
      
      Treatment_1$Segment<- 'Treatment_1'
      
      Treatment_2$Segment<- 'Treatment_2'
      Control$Segment<-'Control'
      Control$SegmentName <- segmentName
      Treatment_1$SegmentName <- segmentName
      Treatment_2$SegmentName <- segmentName
      # Update matrix to check the while condition      
      score_mean <- c() 
      score_pValue <- c() 
      # calculate for minimum mean difference and Wilcox test   
      for (i in 1: length(balancing)) {
        
        #  score_pValue[[balancing[i]]]<-wilcox.test(Treatment[[balancing[i]]],Control[[balancing[i]]])$p.value         
        a_score_mean[[balancing[i]]]<-abs(mean(Treatment_1[[balancing[i]]]) - mean(Control[[balancing[i]]]))/mean(Control[[balancing[i]]])
        b_score_mean[[balancing[i]]]<-abs(mean(Treatment_2[[balancing[i]]]) - mean(Control[[balancing[i]]]))/mean(Control[[balancing[i]]])
        c_score_mean[[balancing[i]]]<-abs(mean(Treatment_1[[balancing[i]]]) - mean(Treatment_2[[balancing[i]]]))/mean(Treatment_1[[balancing[i]]])
        
        score_mean[[balancing[i]]] = max(a_score_mean[[balancing[i]]],b_score_mean[[balancing[i]]],c_score_mean[[balancing[i]]])
        
        
        A_Treatment = NULL
        for (j in 1 : n) {
          Treatment_1.bootstrap.A<-Treatment_1[sample(nrow(Treatment_1), nrow(Treatment_1), replace=TRUE), ]
          A_Treatment[j] = mean(Treatment_1.bootstrap.A[[balancing[i]]])
        }
        # Bootstrap distribution of CoGs_mean in Control Group
        A_Control = NULL
        for (j in 1 : n) {
          Control.bootstrap.A<-Control[sample(nrow(Control), nrow(Control), replace=TRUE), ]
          A_Control[j] = mean(Control.bootstrap.A[[balancing[i]]])
        }  
        B_Treatment = NULL
        for (j in 1 : n) {
          Treatment_2.bootstrap.A<-Treatment_2[sample(nrow(Treatment_2), nrow(Treatment_2), replace=TRUE), ]
          B_Treatment[j] = mean(Treatment_2.bootstrap.A[[balancing[i]]])
        }       
        
        
        
        Effect_A<-A_Treatment-A_Control
        Effect_B<-B_Treatment-A_Control
        Effect_C<-A_Treatment-B_Treatment
        
        quantile(Effect_A, c(alpha/2, 1 - alpha/2))
        
        a_score_pValue[[balancing[i]]]<- ifelse(quantile(Effect_A, c(alpha/2))*quantile(Effect_A, c(1 - alpha/2)) >0, 1, 0)
        b_score_pValue[[balancing[i]]]<- ifelse(quantile(Effect_B, c(alpha/2))*quantile(Effect_B, c(1 - alpha/2)) >0, 1, 0)
        c_score_pValue[[balancing[i]]]<- ifelse(quantile(Effect_C, c(alpha/2))*quantile(Effect_C, c(1 - alpha/2)) >0, 1, 0)
        score_pValue[[balancing[i]]] =a_score_pValue[[balancing[i]]]+b_score_pValue[[balancing[i]]]+c_score_pValue[[balancing[i]]]  
        
      }
      score<-rbind(score_mean,score_pValue) 
      if (sum(score > mean_difference) == 0) break
    }
    
    
    # Write all segments in one data set 
    All <-rbind(All,Treatment_1,Treatment_2,Control)
    
    
  }
}

endtime = Sys.time() - starttime
endtime


#write.table(All,outputname, sep=',', dec='.',row.names=F)


write.table(All,"/Users/r.nacheva/Downloads/Balanced_forJonna.csv", sep=',', dec='.',row.names=F)


#write.table(df_sales_data, 'Exasol.csv', sep=',', dec='.',row.names=F)





