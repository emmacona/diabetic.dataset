#Author: Emmanuelle Coutu-Nadeau
#Date created: 05/09/2019

#Uniqueness Graph

#Libraries
library(ggplot2)
library(ggpubr)

#Import data set (csv)
data <- read.csv("diabetic_data.csv")
total_nrow <- nrow(data)
print(nrow)

timeInHospital <- data$time_in_hospital


#Getting info about data set
#ummary of data
summary(data)

#Time in hospital
time_plot <- ggplot(data = data, aes(timeInHospital))
time_plot +
  xlab("Number of days between admission and discharge") +
  ylab("Frequency") +
  ggtitle("Distribution of time in hospital")  +
  geom_histogram(breaks=seq(0, 10, by=1),
                 col = "grey",
                 aes(fill=..count..)) +
  scale_fill_gradient("Counts", low="blue", high="green") +
  geom_vline(xintercept=mean(timeInHospital), color="red", lwd = 2) +
  geom_vline(xintercept=median(timeInHospital), color="orange", lwd = 2)
