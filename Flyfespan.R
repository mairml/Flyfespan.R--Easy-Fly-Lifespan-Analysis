##############Run this BLOCK first##################
#load dependencies 
library(survival)
library(survminer)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)


#Function to calculate log rank tests 
log_rank_run<- function(contro,pcutoff){
  
  #Identify unique genotypes in file
  genotypes <- unique(flyfespan$Genotype) #or ?.. or Genotype
  genotypes <- genotypes[genotypes != contro]  # Exclude genotype 2691f from the list
  #Initialize list to store results
  log_rank_results <- list()
  
  #Loop through each genotype's data and compare with control
  for (genotype in genotypes) {
    #Subset the data for the current genotype and control
    subset_data <- flyfespan[flyfespan$Genotype %in% c(contro, genotype), ]
    
    #Perform the log-rank test
    log_rank_test <- survdiff(Surv(Day, Escape) ~ Genotype, data = subset_data)
    p_value<-log_rank_test$pvalue
    
    if (p_value < pcutoff) {
      #Store the result with the genotype name
      log_rank_results[[genotype]] <- log_rank_test
    }
  }
  
  #Print the results
  for (genotype in names(log_rank_results)) {
    cat("Log-rank test result comparing control to", genotype, ":\n")
    print(log_rank_results[[genotype]])
    cat("\n")
  }
}

##############END OF BLOCK###################

###Your custom data goes here

#Fly lifespan file:
flyfespan<-read.csv("path/to/file.csv")

#Declare your control Genotype for the experiment:
exp_ctl="v60000"

#Declare your p-value cutoff.
exp_p=0.01

#####Run the survival analysis 
model_fit <- survfit(Surv(Day, Escape) ~  Genotype, data=flyfespan) 
autoplot(model_fit)
model_fit #gives you median lifespan -- save this data to a file


#####Run log rank test
log_rank_run(exp_ctl,exp_p) #finds the significant median differences -- save this data to a file

####Plot data of interest:
#Declare what genotypes you want to plot:
genotypes_of_interest<-c("Control","Favorite_Gene","Condition")

#Subset the data and plot:
subset_df <- flyfespan[flyfespan$Genotype %in% genotypes_of_interest, ]
model_refit <- survfit(Surv(Day, Escape) ~ Genotype, data = subset_df)
custom_colors <- c("#B3B3B3", "#FF6F61","#AFCBFF")
ggsurvplot(model_refit,xlab="Age (Days)",palette = custom_colors,surv.median.line="h")

