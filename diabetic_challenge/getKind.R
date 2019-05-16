#Author: Emmanuelle Coutu-Nadeau
#Date created: 05/09/2019

#Uniqueness Graph

#Libraries
library(ggplot2)
library(ggpubr)
library(gridExtra)

#Import data set (csv)
data <- read.csv("diabetic_data.csv")
total_nrow <- nrow(data)
print(nrow)
source("Functions.R")

#set up factors (aka all that are categories-- not numerical, continuous data)
race <- as.factor(data$race)
gender <- as.factor(data$gender)
age <- as.factor(data$age)
weight <- as.factor(data$weight)
admission_type_id <- as.factor(data$admission_type_id)
discharge_disposition_id <- as.factor(data$discharge_disposition_id)
inPatientsFact <- as.factor(data$number_inpatient)
outPatientsFact <- as.factor(data$number_outpatient)
emergencyPatientsFact <- as.factor(data$number_emergency)


#set up numerical data columns
encounter_id <- data$encounter_id
patient_nb <- data$patient_nbr
admission_type_id_cont <- data$admission_type_id
inPatients <- data$number_inpatient
outPatients <- data$number_outpatient
emergencyPatients <- data$number_emergency
timeInHospital <- data$time_in_hospital
numProcedures <- data$num_procedures

inPatients_plot_cont <- ggplot(data = data, aes(inPatients))
inPatients_plot_cont + geom_freqpoly() + ggtitle("Distribution of in-patients visits")
outPatients_plot_cont <- ggplot(data = data, aes(outPatients))
outPatients_plot_cont + geom_freqpoly() + ggtitle("Distribution of out-patients visits")
emergencyPatients_plot_cont <- ggplot(data = data, aes(emergencyPatients))
emergencyPatients_plot_cont + geom_freqpoly() + ggtitle("Distribution of emergency-patients visits")


inPatients_plot <- ggplot(data, aes(x = inPatientsFact, fill = gender)) + 
  geom_bar(stat = "count", size = 1) +
  ggtitle("Frequency of number of visits in the preceding year for in-patients") + ylim(0,100000) + scale_x_discrete(limits = c("0", "1", "2", "3"))
outPatients_plot <- ggplot(data, aes(x = outPatientsFact, fill = gender)) + 
  geom_bar(stat = "count", size = 1) +
  ggtitle("Frequency of number of visits in the preceding year for out-patients") + ylim(0,100000) + scale_x_discrete(limits = c("0", "1", "2", "3"))
erPatients_plot <- ggplot(data, aes(x = emergencyPatientsFact, fill = gender)) + 
  geom_bar(stat = "count", size = 1) +
  ggtitle("Frequency of number of visits in the preceding year for ER-patients") + ylim(0,100000) + scale_x_discrete(limits = c("0", "1", "2", "3"))

grid.arrange(inPatients_plot, outPatients_plot, erPatients_plot)
