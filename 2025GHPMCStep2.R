###Combinging all complete .csv's into one document with treatment columns

####################Start Here#####################



Run1=read.csv("DR PMC 2025 rd1 calcs full.csv")
Run2=read.csv("DR PMC 2025 rd2 calcs full.csv")
Run3=read.csv("DR PMC 2025 rd3 calcs full.csv")
Run4=read.csv("DR PMC 2025 rd4 calcs full.csv")


names(Run1)
names(Run2)
names(Run3)
names(Run4)

# Rename std.dev variations to Std.Dev
#names(Run2)[names(Run2) == "std.dev"] <- "Std.Dev"
#names(Run3)[names(Run3) == "Std.dev"] <- "Std.Dev"

mergeAll=rbind(Run1,Run2,Run3,Run4)
head(mergeAll)

mergeClean <- select(mergeAll, 
                     UID,            
                     Type_of_Sample,  
                     ugC.g.day)

write.csv(mergeClean, "PMC_DR2025_all_calcs.csv",
          row.names = FALSE)


#### Adapting Hava's PMC variablity, and coefficient of variation (CV)

#read in cleaned data from main directory 
all.data<-read.csv("PMC_DR2025_all_calcs.csv", stringsAsFactors = FALSE) 

num <- all.data$number 

#Calculate mean ugC.g.day per sample 
#Calculates mean of 3 technical replicates, which now share the same sample.id for easy grouping. 
full.means <- all.data %>% 
  group_by(UID) %>%
  summarize(mean.resp = mean(ugC.g.day, na.rm = T))

no.NA = all.data[!is.na(all.data$ugC.g.day),]


#Calculate coefficient of variation (CV) 
#CV is standard deviation divided by mean, expressesed as a percent. 
add.cv <- no.NA %>% 
  group_by(UID) %>% 
  summarize(sample.cv = sd(ugC.g.day, na.rm = TRUE)/mean(ugC.g.day, na.rm = TRUE)*100)

#merge sample mean and cv data 
mean.cv <- merge(full.means, add.cv, by = "UID")
#if issues occur, this line is why, make dummy file----
#bring in full sample names from the name master. 
#sample.id <- read.csv("List of samples CCNRD.csv", 
#                      stringsAsFactors = FALSE)

# 1. Assign the aggregated data (mean and CV) to the final variable
pmc.var <- mean.cv

# 2. Keep only samples that have CV > 0.
pmc.var <- subset(pmc.var, sample.cv > 0) 

# 3. Export the final aggregated, filtered dataset
write.csv(pmc.var, "PMC_DR2025_all_calcs_CV.csv", row.names = FALSE)

# 4. Identify high-variability samples for further investigation (CV > 20)
missing <- subset(pmc.var, sample.cv > 20)

# 5. Export the list of high-CV samples
write.csv(missing, "DR2025_missing.csv", row.names = FALSE)

