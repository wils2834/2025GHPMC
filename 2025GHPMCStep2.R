dplyr::select(Date_factor, Mean = response) %>%
left_join(
cld_time_0_15 %>% as.data.frame() %>% dplyr::select(Date_factor, CLD = .group),
by = "Date_factor"
)
print(time_0_15_summary)
# ===== 15-30 cm Depth =====
anova(model_time_15_30)
shapiro.test(residuals(model_time_15_30))
qqnorm(residuals(model_time_15_30)); qqline(residuals(model_time_15_30))
emm_time_15_30 <- emmeans(model_time_15_30, ~ Date_factor)
pairs(emm_time_15_30, adjust = "bonferroni")
emm_time_15_30_back <- emmeans(model_time_15_30, ~ Date_factor, type = "response")
cld_time_15_30 <- suppressWarnings(cld(emm_time_15_30, Letters = letters, adjust = "bonferroni"))
time_15_30_summary <- as.data.frame(emm_time_15_30_back) %>%
dplyr::select(Date_factor, Mean = response) %>%
left_join(
cld_time_15_30 %>% as.data.frame() %>% dplyr::select(Date_factor, CLD = .group),
by = "Date_factor"
)
print(time_15_30_summary)
# ===== 30-60 cm Depth (refit first) =====
model_time_30_60 <- lmer(log(Avg_runs) ~ Date_factor + (1|UID),
data = filter(poxc_data, Depth == "30-60"))
anova(model_time_30_60)
shapiro.test(residuals(model_time_30_60))
qqnorm(residuals(model_time_30_60)); qqline(residuals(model_time_30_60))
# With only 3 time points, post-hoc tests are less useful
# But you can still look at the estimates
emm_time_30_60 <- emmeans(model_time_30_60, ~ Date_factor)
pairs(emm_time_30_60, adjust = "bonferroni")
emm_time_30_60_back <- emmeans(model_time_30_60, ~ Date_factor, type = "response")
print(emm_time_30_60_back)
# PLOT 1: POXC by Depth (boxplot with CLD letters)
labels_depth <- poxc_data %>%
group_by(Depth) %>%
summarise(max_y = max(Avg_runs, na.rm = TRUE), .groups = "drop") %>%
left_join(
cld_depth %>% as.data.frame() %>% dplyr::select(Depth, CLD = .group),
by = "Depth"
) %>%
mutate(y_position = max_y * 1.05)
ggplot(poxc_data, aes(x = Depth, y = Avg_runs, fill = Depth)) +
geom_boxplot(alpha = 0.7) +
geom_text(
data = labels_depth,
aes(x = Depth, y = y_position, label = CLD),
inherit.aes = FALSE,
size = 5,
fontface = "bold",
color = "black"
) +
scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
theme_bw(base_size = 14) +
theme(
axis.text.x = element_text(size = 12, face = "bold", color = "black"),
axis.text.y = element_text(size = 12, face = "bold", color = "black"),
axis.title = element_text(size = 14, face = "bold"),
legend.position = "none",
plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
) +
labs(
title = "Permanganate Oxidizable Carbon by Soil Depth",
x = "Depth (cm)",
y = "POXC (moles permanganate reduced per kg soil)"
)
# PLOT 2: POXC over Time (line plot with error bars)
time_plot_data <- poxc_data %>%
group_by(Date_Collected) %>%
summarise(
Mean = mean(Avg_runs, na.rm = TRUE),
SE = sd(Avg_runs, na.rm = TRUE) / sqrt(n()),
.groups = "drop"
)
ggplot(time_plot_data, aes(x = Date_Collected, y = Mean)) +
geom_line(size = 1.2, color = "#0072B2") +
geom_point(size = 3, color = "#0072B2") +
geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE),
width = 2, color = "#0072B2") +
theme_bw(base_size = 14) +
theme(
axis.text = element_text(size = 12, face = "bold", color = "black"),
axis.title = element_text(size = 14, face = "bold"),
plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
) +
labs(
title = "POXC Over Time",
x = "Date Collected",
y = "Mean POXC (moles permanganate reduced per kg soil)"
)
# PLOT 3: POXC by Depth over Time (interaction plot)
depth_time_plot_data <- poxc_data %>%
group_by(Date_Collected, Depth) %>%
summarise(
Mean = mean(Avg_runs, na.rm = TRUE),
SE = sd(Avg_runs, na.rm = TRUE) / sqrt(n()),
.groups = "drop"
)
ggplot(depth_time_plot_data, aes(x = Date_Collected, y = Mean,
color = Depth, group = Depth)) +
geom_line(size = 1.2) +
geom_point(size = 3) +
geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 2) +
scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
theme_bw(base_size = 14) +
theme(
axis.text = element_text(size = 12, face = "bold", color = "black"),
axis.title = element_text(size = 14, face = "bold"),
legend.title = element_text(size = 14, face = "bold"),
legend.text = element_text(size = 12, face = "bold"),
plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
) +
labs(
title = "POXC Over Time by Soil Depth",
x = "Date Collected",
y = "moles permanganate reduced per kg soil",
color = "Depth (cm)"
)
$ git status -s
# Basic git status
system("git status")
# Capture output as a character vector
status_output <- system("git status", intern = TRUE)
print(status_output)
# Clone the repo (run this once)
system("git clone https://github.com/wils2834/2025GHPMC.git")
# Navigate into the cloned directory
setwd("2025GHPMC")
# Now you can check status
system("git status")
library(dplyr)
library(ggplot2)
#Run1
getwd("git clone https://github.com/wils2834/2025GHPMC.git")
#Run1
getwd()
dir() #what files are in my wd?
dir() #what files are in my wd?
meta = read.csv("2025_GH_PMC_SW - Run 1B 1_15_2026.csv", header=TRUE)
str(meta)
licor = read.table("20260115GHPMCadjusted.txt", header = TRUE,
sep="\t", #tab-delim
skip = 1, #skip the first two lines
col.names = c("date", "time", "ppm", "temp","press",
"abs", "voltage", "flow", "junk"))
str(licor)#what kind of data? structure
## get date
licor$date=as.Date(licor$date, "%Y-%m-%d")
licor$time = as.POSIXct(licor$time, format = "%H:%M:%S")
smple_cnt = 1
for (i in 1:nrow(licor)){
print(paste("Working on",i)) #print the name of the row
if (i == nrow(licor)){ #on the last row, do something different
print("  Last one...")
print(paste("pause #", smple_cnt))
ppm_avg = mean(licor$ppm[(i-5):i])
#ppm_avg is the major output of this code
#it tells you, what did  the licor find the average ppm right before the pause
meta[smple_cnt, 'time'] = substr(as.character(licor$time[i]), 12, 19)
meta[smple_cnt, 'ppm'] = ppm_avg
next
}
no_pause = as.numeric(difftime(licor$time[i+1], licor$time[i], units="secs"))
#line above , "no_pause", calculates the difference between each line
if (no_pause<=4) { #if () is trure, then the action {} happens
print(paste("  No pause yet..."))
next #go to next line
}
print("Found a pause!")
if (i<5){
print("But its less than five")
next #go to next line
}
print(paste("Pause #", smple_cnt))
ppm_avg = mean(licor$ppm[(i-5):i]) # calculates average of previous 5 lines
meta[smple_cnt, 'time'] = substr(as.character(licor$time[i]), 12, 19)
meta[smple_cnt, 'ppm'] = ppm_avg
meta[smple_cnt, 'line'] = i
smple_cnt = smple_cnt + 1
}
meta$ppm.corr = meta$ppm-mean(meta$ppm[which(meta$Type_of_Sample
=="blank")])
# mean(meta$ppm[which(meta$Weight.10g==0)])#
#correct for blank
#replace "plot' with the column id name that you use if necessary
str(meta)
#calculate actual time elapsed
meta$start = as.POSIXct(meta$start, format = "%H:%M")
meta$end = as.POSIXct(meta$end, format = "%H:%M")
meta$days = (meta$end-meta$start)/(24*60*60)+1
meta$ugC.L <- NA
meta$ugC.L =as.numeric(meta$ugC.L)
meta$days = as.numeric(meta$days)
meta$ugC.L = (meta$ppm.corr*12*1)/(.082058*298.15)
meta$ugC.g.day = ((meta$ugC.L*.932))/as.numeric(meta$Weight.10g)
#calculate in ug CO2-C per g soil,
#which = mg C/kg soil
#.932 = L in jar
(meta$ugC.L*.932)/meta$Weight.10g/meta$days
# Convert Weight.10g to numeric
meta$Weight.10g <- as.numeric(meta$Weight.10g)
#calculate in ug CO2-C per L using PV=nRT
meta$ugC.g.day = ((meta$ugC.L*.932))/as.numeric(meta$Weight.10g)
#calculate in ug CO2-C per g soil,
#which = mg C/kg soil
#.932 = L in jar
(meta$ugC.L*.932)/meta$Weight.10g/meta$days
(meta$ugC.L*.932)/meta$Weight.10g
((meta$ugC.L*.932))/ meta$Weight.10g
meta$ugC.g.day = ((meta$ugC.L*.932))/ meta$Weight.10g
head(meta)
write.csv(meta, "DR PMC 2025 rd1B calcs full.csv",
row.names = FALSE)
# Look at the original data before conversion
meta_original <- read.csv("2025_GH_PMC_SW - Run 1B 1_15_2026.csv", header=TRUE)
# See which rows have problems
meta_original$Weight.10g[is.na(meta$Weight.10g)]
# Or see all rows with NA in ugC.g.day
meta[is.na(meta$ugC.g.day), ]
# Check the row numbers
which(is.na(meta$Weight.10g))
#Run2----
getwd()
dir() #what files are in my wd?
meta = read.csv("2025_GH_PMC_SW - Run 2B 1_22_2026.csv", header=TRUE)
str(meta)
licor = read.table("20260122GHPMCadjusted.txt", header = TRUE,
sep="\t", #tab-delim
skip = 1, #skip the first two lines
col.names = c("date", "time", "ppm", "temp","press",
"abs", "voltage", "flow", "junk"))
str(licor)#what kind of data? structure
## get date
licor$date=as.Date(licor$date, "%Y-%m-%d")
licor$time = as.POSIXct(licor$time, format = "%H:%M:%S")
smple_cnt = 1
for (i in 1:nrow(licor)){
print(paste("Working on",i)) #print the name of the row
if (i == nrow(licor)){ #on the last row, do something different
print("  Last one...")
print(paste("pause #", smple_cnt))
ppm_avg = mean(licor$ppm[(i-5):i])
#ppm_avg is the major output of this code
#it tells you, what did  the licor find the average ppm right beforethe pause
meta[smple_cnt, 'time'] = substr(as.character(licor$time[i]), 12, 19)
meta[smple_cnt, 'ppm'] = ppm_avg
next
}
no_pause = as.numeric(difftime(licor$time[i+1], licor$time[i], units="secs"))
#line above , "no_pause", calculates the difference between each line
if (no_pause<=4) { #if () is trure, then the action {} happens
print(paste("  No pause yet..."))
next #go to next line
}
print("Found a pause!")
if (i<5){
print("But its less than five")
next #go to next line
}
print(paste("Pause #", smple_cnt))
ppm_avg = mean(licor$ppm[(i-5):i]) # calculates average of previous 5 lines
meta[smple_cnt, 'time'] = substr(as.character(licor$time[i]), 12, 19)
meta[smple_cnt, 'ppm'] = ppm_avg
meta[smple_cnt, 'line'] = i
smple_cnt = smple_cnt + 1
}
meta$ppm.corr = meta$ppm - mean(meta$ppm[which(meta$Type_of_Sample == "blank")], na.rm = TRUE)
# mean(meta$ppm[which(meta$Weight.10g==0)])#
#correct for blank
#replace "plot' with the column id name that you use if necessary
str(meta)
#calculate actual time elapsed
# this didn't work meta$readdate = as.Date(meta$readdate, format = "%Y%m%d")
meta$start = as.POSIXct(meta$start, format = "%H:%M")
meta$end = as.POSIXct(meta$end, format = "%H:%M")
meta$days = (meta$end-meta$start)/(24*60*60)+1
meta$ugC.L <- NA
meta$ugC.L =as.numeric(meta$ugC.L)
meta$Weight.10g = as.numeric(meta$Weight.10g)
meta$days = as.numeric(meta$days)
meta$ugC.L = (meta$ppm.corr*12*1)/(.082058*298.15)
meta$ugC.g.day = (meta$ugC.L*.932)/meta$Weight.10g/meta$days
(meta$ugC.L*.932)/meta$Weight.10g/meta$days
(meta$ugC.L*.932)/meta$Weight.10g
((meta$ugC.L*.932))/ meta$Weight.10g
meta$ugC.g.day = ((meta$ugC.L*.932))/ meta$Weight.10g
#calculate in ug CO2-C per g soil,
#which = mg C/kg soil
#.932 = L in jar
head(meta)
write.csv(meta, "DR PMC 2025 rd2B calcs full.csv",
row.names = FALSE)
# Find this specific sample
problem_sample <- meta[meta$UID == "GH25051" & meta$Rep_PMC == "3", ]
print(problem_sample)
# Check the blank mean used for correction
mean_blank <- mean(meta$ppm[meta$Type_of_Sample == "blank"], na.rm = TRUE)
print(paste("Mean blank ppm:", mean_blank))
# Check all blanks
blanks <- meta[meta$Type_of_Sample == "blank", ]
print(blanks[, c("UID", "ppm", "time", "readdate")])
# Look at nearby samples
meta[meta$Cup_Label %in% c("077A", "077B", "077C"),
c("UID", "Cup_Label", "ppm", "ppm.corr", "ugC.g.day")]
getwd()
dir() #what files are in my wd?
meta = read.csv("2025_GH_PMC_SW - Run 3B 1_24_2026.csv", header=TRUE)
str(meta)
licor = read.table("20260124GHPMCadjusted.txt", header = TRUE,
sep="\t", #tab-delim
skip = 1, #skip the first two lines
col.names = c("date", "time", "ppm", "temp","press",
"abs", "voltage", "flow", "junk"))
str(licor)#what kind of data? structure
## get date
licor$date=as.Date(licor$date, "%Y-%m-%d")
licor$time = as.POSIXct(licor$time, format = "%H:%M:%S")
smple_cnt = 1
for (i in 1:nrow(licor)){
print(paste("Working on",i)) #print the name of the row
if (i == nrow(licor)){ #on the last row, do something different
print("  Last one...")
print(paste("pause #", smple_cnt))
ppm_avg = mean(licor$ppm[(i-5):i])
#ppm_avg is the major output of this code
#it tells you, what did  the licor find the average ppm right beforethe pause
meta[smple_cnt, 'time'] = substr(as.character(licor$time[i]), 12, 19)
meta[smple_cnt, 'ppm'] = ppm_avg
next
}
no_pause = as.numeric(difftime(licor$time[i+1], licor$time[i], units="secs"))
#line above , "no_pause", calculates the difference between each line
if (no_pause<=4) { #if () is trure, then the action {} happens
print(paste("  No pause yet..."))
next #go to next line
}
print("Found a pause!")
if (i<5){
print("But its less than five")
next #go to next line
}
print(paste("Pause #", smple_cnt))
ppm_avg = mean(licor$ppm[(i-5):i]) # calculates average of previous 5 lines
meta[smple_cnt, 'time'] = substr(as.character(licor$time[i]), 12, 19)
meta[smple_cnt, 'ppm'] = ppm_avg
meta[smple_cnt, 'line'] = i
smple_cnt = smple_cnt + 1
}
meta$ppm.corr = meta$ppm - mean(meta$ppm[which(meta$Type_of_Sample == "blank")], na.rm = TRUE)
# mean(meta$ppm[which(meta$Weight.10g==0)])#
#correct for blank
#replace "plot' with the column id name that you use if necessary
str(meta)
#calculate actual time elapsed
meta$start = as.POSIXct(meta$start, format = "%H:%M")
meta$end = as.POSIXct(meta$end, format = "%H:%M")
meta$days = (meta$end-meta$start)/(24*60*60)+1
meta$ugC.L <- NA
meta$ugC.L =as.numeric(meta$ugC.L)
meta$Weight.10g = as.numeric(meta$Weight.10g)
meta$ugC.L = (meta$ppm.corr*12*1)/(.082058*298.15)
meta$ugC.g.day = (meta$ugC.L*.932)/meta$Weight.10g/as.numeric(meta$days)
meta$ugC.g.day = ((meta$ugC.L*.932))/ meta$Weight.10g
#calculate in ug CO2-C per g soil,
#which = mg C/kg soil
#.932 = L in jar
head(meta)
write.csv(meta, "DR PMC 2025 rd3B calcs full.csv",
row.names = FALSE)
getwd()
dir() #what files are in my wd?
meta = read.csv("2025_GH_PMC_SW - Run 4B 1_30_2026.csv", header=TRUE)
str(meta)
licor = read.table("20260129GHPMCadjusted.txt", header = TRUE,
sep="\t", #tab-delim
skip = 1, #skip the first two lines
col.names = c("date", "time", "ppm", "temp","press",
"abs", "voltage", "flow", "junk"))
str(licor)#what kind of data? structure
## get date
licor$date=as.Date(licor$date, "%Y-%m-%d")
licor$time = as.POSIXct(licor$time, format = "%H:%M:%S")
smple_cnt = 1
for (i in 1:nrow(licor)){
print(paste("Working on",i)) #print the name of the row
if (i == nrow(licor)){ #on the last row, do something different
print("  Last one...")
print(paste("pause #", smple_cnt))
ppm_avg = mean(licor$ppm[(i-5):i])
#ppm_avg is the major output of this code
#it tells you, what did  the licor find the average ppm right beforethe pause
meta[smple_cnt, 'time'] = substr(as.character(licor$time[i]), 12, 19)
meta[smple_cnt, 'ppm'] = ppm_avg
next
}
no_pause = as.numeric(difftime(licor$time[i+1], licor$time[i], units="secs"))
#line above , "no_pause", calculates the difference between each line
if (no_pause<=4) { #if () is trure, then the action {} happens
print(paste("  No pause yet..."))
next #go to next line
}
print("Found a pause!")
if (i<5){
print("But its less than five")
next #go to next line
}
print(paste("Pause #", smple_cnt))
ppm_avg = mean(licor$ppm[(i-5):i]) # calculates average of previous 5 lines
meta[smple_cnt, 'time'] = substr(as.character(licor$time[i]), 12, 19)
meta[smple_cnt, 'ppm'] = ppm_avg
meta[smple_cnt, 'line'] = i
smple_cnt = smple_cnt + 1
}
meta$ppm.corr = meta$ppm - mean(meta$ppm[which(meta$Type_of_Sample == "blank")], na.rm = TRUE)
# mean(meta$ppm[which(meta$Weight.10g==0)])#
#correct for blank
#replace "plot' with the column id name that you use if necessary
str(meta)
#calculate actual time elapsed
meta$start = as.POSIXct(meta$start, format = "%H:%M")
meta$end = as.POSIXct(meta$end, format = "%H:%M")
meta$days = (meta$end-meta$start)/(24*60*60)+1
meta$ugC.L <- NA
meta$ugC.L =as.numeric(meta$ugC.L)
meta$Weight.10g = as.numeric(meta$Weight.10g)
meta$ugC.L = (meta$ppm.corr*12*1)/(.082058*298.15)
meta$ugC.g.day = (meta$ugC.L*.932)/meta$Weight.10g/as.numeric(meta$days)
meta$ugC.g.day = ((meta$ugC.L*.932))/ meta$Weight.10g
#calculate in ug CO2-C per g soil,
#which = mg C/kg soil
#.932 = L in jar
head(meta)
write.csv(meta, "DR PMC 2025 rd4B calcs full.csv",
row.names = FALSE)
Run1=read.csv("DR PMC 2025 rd1B calcs full.csv")
Run2=read.csv("DR PMC 2025 rd2B calcs full.csv")
Run3=read.csv("DR PMC 2025 rd3B calcs full.csv")
Run4=read.csv("DR PMC 2025 rd4B calcs full.csv")
names(Run1)
names(Run2)
names(Run3)
names(Run4)
mergeAll=rbind(Run1,Run2,Run3,Run4)
head(mergeAll)
mergeClean <- select(mergeAll,
UID,
Type_of_Sample,
ugC.g.day)
library(dplyr)
mergeClean <- select(mergeAll,
UID,
Type_of_Sample,
ugC.g.day)
head(mergeAll)
mergeClean <- dplyr::select(mergeAll,
UID,
Type_of_Sample,
ugC.g.day)
write.csv(mergeClean, "PMC_DR2025_all_calcsB.csv",
row.names = FALSE)
#read in cleaned data from main directory
all.data<-read.csv("PMC_DR2025_all_calcsB.csv", stringsAsFactors = FALSE)
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
# 1. Assign the aggregated data (mean and CV) to the final variable
pmc.var <- mean.cv
# 2. Keep only samples that have CV > 0.
pmc.var <- subset(pmc.var, sample.cv > 0)
# 3. Export the final aggregated, filtered dataset
write.csv(pmc.var, "PMC_DR2025_all_calcsB_CV.csv", row.names = FALSE)
# 4. Identify high-variability samples for further investigation (CV > 20)
missing <- subset(pmc.var, sample.cv > 20)
# 5. Export the list of high-CV samples
write.csv(missing, "DR2025_missingB.csv", row.names = FALSE)
# Add all changes
system("git add .")
# Commit with a descriptive message
system('git commit -m "Added PMC calculations and merged data from runs 1-4"')
# Push to GitHub
system("git push")
# Check what R files exist
list.files(pattern = "\\.R$")
# If you have an R script (e.g., "PMC_analysis.R"), add it:
system("git add your_script_name.R")
system('git commit -m "Added R analysis script"')
system("git push")
# Add both R scripts
system("git add 2025GHPMCStep1.R")
system("git add 2025GHPMCStep2.R")
# Commit
system('git commit -m "Added R analysis scripts for PMC data processing"')
# Push
system("git push")
# See detailed status
system("git status")
# Check if the files are actually different
system("git diff 2025GHPMCStep1.R")
system("git diff 2025GHPMCStep2.R")
# This saves all commands you've typed
savehistory("2025GHPMCStep2.R")
