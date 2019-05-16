#Author: Emmanuelle Coutu-Nadeau
#Date created: 05/09/2019

#Uniqueness Graph

source("Functions.R")

timeInHospital <- data$time_in_hospital

#Time in hospital
time_plot <- ggplot(data = data, aes(timeInHospital))
time_plot +
  xlab("Number of days between admission and discharge") +
  ylab("Frequency") +
  ggtitle("Distribution of time in hospital")  +
  scale_x_continuous(breaks=seq(0, 10, by=1))+
  geom_histogram(breaks=seq(0, 10, by=1),
                 col = "grey",
                 aes(fill=..count..)) +
  geom_vline(xintercept=mean(timeInHospital), color="red", lwd = 1) +
  geom_vline(xintercept=median(timeInHospital), color="orange", lwd = 1) + ylim(0, 18000) 
  legend("topright", legend = c("Mean", "Median"), col = c("red", "orange"),  lwd=1)
