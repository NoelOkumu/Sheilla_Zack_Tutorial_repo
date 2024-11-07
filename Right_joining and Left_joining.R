##This is a script for a practice exercise sent by Sheilla Mwangi on Right joining and Left joining

#Set working directory
Joining<- "/home/noel/R/Noel_Barbraham_Ggplot2/ggplot_data_files"
setwd(Joining)

#Load necessary libraries
library("tidyverse")

############################################## QUESTION ONE #######################################################
#Data<-DownloadFestival.csv
DownloadFestival_data<- read.csv("DownloadFestival.csv")
dim(DownloadFestival_data)
head(DownloadFestival_data)

#Group and count appearance of each person
Group_count <- DownloadFestival_data %>%
  group_by(person) %>%
  count() %>%
  rename(Attendance= n)
head (Group_count)

#Use right_join() to merge this attendance count
Attendance_festival_data <- DownloadFestival_data %>%
  right_join(Group_count)
head(Attendance_festival_data)

############################################# QUESTION TWO #################################################################
#Data<- cancer_stats.csv
Cancer_data<- read.csv("cancer_stats.csv")
dim(Cancer_data)
head(Cancer_data)

#creating a new data set containing only female cancer death statistics
Female_deaths <- Cancer_data %>%
  select (Class, Site, Female.Deaths)
head (Female_deaths)

#Use left_join() to merge this subset with the original data
Cancer_Female_left_joined <- Cancer_data %>%
  left_join(Female_deaths,by = c("Class", "Site"))
head(Cancer_Female_left_joined)

############################################## QUESTION 3 ###################################################################
#Data<-brain_bodyweight.tsv
Brain_bodyweight <- read_delim ("brain_bodyweight.txt")
head(Brain_bodyweight)
dim(Brain_bodyweight)

#Calculate Log10 transformations for both body weight and brain
Log10_Bodyweight <-log10(Brain_bodyweight$body) 
head(Log10_Bodyweight)

Log10_Brainweight<- log10(Brain_bodyweight$brain)
head(Log10_Brainweight)

#Create a data frame for the log transformations
Log10_Weights<- data.frame(Log10_Bodyweight,Log10_Brainweight)
head(Log10_Weights)

#Create a species habitat dataframe
Species_habitat<- data.frame(Brain_bodyweight[1:2])
head(Species_habitat)

#Use left_join() to ensure all species from brain_body weight.tsv are included, even if their habitats are not available in species_habitats.
Combined_Data<- left_join(Brain_bodyweight,Species_habitat,by = "Species","Category")
head(Combined_Data)

#Check if category.y and Category.x ae similar
identical(Combined_Data$Category.x, Combined_Data$Category.y) #TRUE

#Delete one of the two categories
Combined_Data<- Combined_Data %>%
  select(-Category.y) %>%
  rename(Category = Category.x)

dim(Combined_Data)
head(Combined_Data)

############################################################# QUESTION FOUR ################################################################################
#Data<-treatments.csv
treatment_data<- read.csv("treatments.csv")
dim(treatment_data)
head(treatment_data)

#Calculating the mean and standard of each treatment condition using group_by()
Summary_stats_treatment<- treatment_data %>%
  group_by(Sample) %>%
    summarise(
    mean_measure = mean(Measure,na.rm = TRUE),
    sd_measure = sd(Measure, na.rm = TRUE)
  )
head(Summary_stats_treatment)
dim(Summary_stats_treatment)

#using right_join() to merge these calculated summaries back with treatments.csv
Combined_summary_treament<- right_join(Summary_stats_treatment, treatment_data, by = "Sample")
dim(Combined_summary_treament)
head(Combined_summary_treament)


