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

#set up factors (aka all that are categories-- not numerical, continuous data)
admission_type_id <- as.factor(data$admission_type_id)

#set up numerical data columns
admission_type_id_cont <- data$admission_type_id


#Getting info about data set
#ummary of data
summary(data)

#Admissions
admission_plot_continuous <- ggplot(data = data, aes(admission_type_id_cont))
admission_plot_continuous + geom_freqpoly() # we observe that this data is not

hist(admission_type_id_cont)

admission_plot <- ggplot(data = data, aes(admission_type_id))
admission_plot +
  xlab("Admission Type ID") +
  ylab("Counts") +
  ggtitle("Distribution of admission types")  +
  geom_histogram(breaks=seq(0, 8, by=1),
                 col = "grey",
                 aes(fill=..count..)) +
  scale_fill_gradient("Counts", low="blue", high="green")
