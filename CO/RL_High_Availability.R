
#### Install and load needed packages
wants <- c("plyr", "dplyr", "boot", "lubridate", "samplesize","pwr")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
lapply(wants, library, character.only=T)

##### Insert your data for balancing

input<- read.csv("/Users/r.nacheva/Downloads/[RL]_High_Availability_-_Low_Rides_2022_07_27.csv")


summary(input)

names(input)

# Define segmentation : LIke country, driver type - most granular combination level, leave 'total' for no segmentation 
segmentation <-  list(segment1 = c('total')
)


# Define your entity - the final output will assign every entity to a test variant
entity <- 'id_driver'

# Create ratio KPIs: 

kpi_name = c('1')  # same name as inserted in the balancing, leave 1 for no ratio KPIs
kpi_nominator = c('rides') # same as in data set, put random fine column name fro no ratio KPIs
kpi_denominator = c('rides') # same as in data set, put random fine column name fro no ratio KPIs

# Create data set on new kpis - no user input needed
kpi = cbind (kpi_name,kpi_nominator,kpi_denominator)
kpi = as.data.frame(kpi)
kpi

# Main KPIs you want to balance on and take into consideration for outlier detection
# Start with your main KPI
balancing <- c('supply_hours','rides','broadcasts_per_supply_hour')

#### Sample size check - please insert expected impact (relative) on your main KPI (first one in the balancing)
expected_impact = 0.07

# Define share for your treatment group (currently script devides into 2 groups only)
buckets = 0.7

# Parameter for min allowed difference and  outlier detection
mean_difference = 0.05 # max allowed difference between groups (even when insignificant results)
outlier_threshold = 0.99 # 1 means no outliere detection - all 1 (100%) of the data is used

# Define Output file name
outputname <- '/Users/r.nacheva/Downloads/Balancing_[RL]_High_Availability_-_Low_Rides_2022_07_27.csv'

################################################
############## End manual input

starttime = Sys.time()


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
    p=0
    sample_size = c()
    
    n = 1000
    alpha = 0.05
    
    while (sum(score > mean_difference) > 0)
    {  
      
      #Draw random sample from population for Treatment
      Treatment <- segment_input[sample(1:nrow(segment_input), buckets*nrow(segment_input)),]
      #delete randomly drawn data from population
      #segment_input_1 <- segment_input[!(segment_input$ID %in% Treatment$ID),]
      #Draw random sample from population for Control
      Control <- segment_input[!(segment_input$ID %in% Treatment$ID),]
      # Summarize results on entity level & add assigment and segment to data sets      
      Treatment<-summarise_all(group_by_(Treatment[c(entity,names(Treatment)[sapply(Treatment, is.numeric)])],.dots = entity),funs(sum))
      Control<-summarise_all(group_by_(Control[c(entity,names(Control)[sapply(Control, is.numeric)])],.dots = entity), funs(sum))
      Treatment$Segment<- 'Treatment'
      Control$Segment<-'Control'
      Control$SegmentName <- segmentName
      Treatment$SegmentName <- segmentName
      
      
      # Update matrix to check the while condition 
      
      score_mean <- c() 
      score_pValue <- c() 
      # calculate for minimum mean difference and Wilcox test   
      for (i in 1: length(balancing)) {
        
        for (kp in 1:nrow(kpi)) {
          denom_tre = sum(Treatment[[kpi_denominator[kp]]])
          denom_con = sum(Control[[kpi_denominator[kp]]])
          Treatment[[kpi_name[kp]]] = (Treatment[[kpi_nominator[kp]]]/denom_tre)*nrow(Treatment)
          Control[[kpi_name[kp]]] = (Control[[kpi_nominator[kp]]]/denom_con)*nrow(Control)
        }
        
        # Sample size calculation 
        both = rbind(Treatment,Control)
        delta <- (1+expected_impact)*mean(both[[balancing[1]]]) - mean(both[[balancing[1]]])
        sigma <- sd(both[[balancing[1]]])
        d <- delta/sigma
        a = pwr.t.test(d=d, sig.level=.05, power = .80, type = 'two.sample')
        sample_size_needed <- a[1]
        
        reverse = pwr.t.test(n=nrow(segment_input), sig.level=.05, power = .80, type = 'two.sample')
        mde = reverse$d 
        
        hk = (reverse$d *sigma)/mean(both[[balancing[1]]])
        
        sample_size_needed <- a[1]
        
        check_sample = ifelse (sample_size_needed <=  buckets*nrow(segment_input),paste('ok',as.numeric(sample_size_needed)/buckets, sep = "_")
                               ,paste('not_enough_sample',sample_size_needed, sep = "_"))
        
        #  score_pValue[[balancing[i]]]<-wilcox.test(Treatment[[balancing[i]]],Control[[balancing[i]]])$p.value    
        score_mean[[balancing[i]]]<-abs(mean(Treatment[[balancing[i]]]) - mean(Control[[balancing[i]]]))/mean(Control[[balancing[i]]])
        
        
        test <- t.test(Treatment[[balancing[i]]], Control[[balancing[i]]],
                       var.equal = FALSE, alternative = "two.sided"
                       #test <- t.test(subset(Treatment[[kpis[i]]], Treatment[[kpis[i]]] >0) , subset(Control[[kpis[i]]], Control[[kpis[i]]] >0) ,
                       #              var.equal = FALSE, alternative = "two.sided"                
                       
        )
        score_pValue[[balancing[i]]]<- ifelse(test$conf.int[1]*test$conf.int[2] >0, 1, 0)
        
      }
      score<-rbind(score_mean,score_pValue) 
      p = p+1
      if (sum(score > mean_difference) == 0| p ==1000| is.na(sum(score > mean_difference)) == TRUE) break
    }
    Treatment$sample_check = check_sample
    Control$sample_check = check_sample
    Treatment$mde = mde
    Control$mde = mde
    
    # Write all segments in one data set 
    All <-rbind(All,Treatment,Control)
    
  }
}

endtime = Sys.time() - starttime
endtime

write.table(All,outputname, sep=',', dec='.',row.names=F)
