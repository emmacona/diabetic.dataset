#Author: Emmanuelle Coutu-Nadeau
#Date created: 05/09/2019

#Uniqueness Graph

#Libraries
library(ggplot2)
library(ggpubr)
library(gridExtra)

data.type <- sapply(data, typeof)

#Import data set (csv)
data <- read.csv("diabetic_data.csv")
data.category <- c(names(data))
data.type <- sapply(data, typeof)
total_nrow <- nrow(data)
reversed_data <- rev(data)


#Patients
unique_patients <- subset(data, !(duplicated(data$patient_nbr)))
print(unique_patients)
print(nrow(unique_patients))
repeated_patients <- subset(data, duplicated(data$patient_nbr)|duplicated(reversed_data))
print(nrow(repeated_patients))
unique_id <- unique(data$patient_nbr)


uniqueness_plot <- ggplot(unique_patients, aes(x = gender, fill = race)) + 
  geom_bar(stat = "count", size = 1) + ylim(0, 40000) +
  ggtitle("Proportion of male/female of unique patients")

repeated_plot <- ggplot(repeated_patients, aes(x = gender, fill = race)) + 
  geom_bar(stat = "count", size = 1) + ylim(0, 40000) +
  ggtitle("Proportion of male/female of repeated patients")

grid.arrange(uniqueness_plot, repeated_plot)


# #creating data set
# df <- data.frame(
#   group = c("Unique patients", "Repeated Patients"),
#   value = c(nrow(unique_patients), nrow(repeated_patients)),
#   gender = c(unique_patients$gender, repeated_patients$gender)
# )
# head(df)
# 
# uniqueness_plot <- ggplot(df, aes(x = group, y = value, fill = gender)) +
#   geom_bar(stat="identity")
# uniqueness_plot 
# 
# ggplot(data = data, aes(x= patient_nb, y = encounter_id)) + geom_point()
# hist(unique_patients$patient_nbr)
# hist(repeated_patients$patient_nbr)
# 
# patient_nb_plot <- ggplot(df, aes(x = df$group, y = df$value))
# patient_nb_plot +
#   xlab("Patient Number") +
#   ylab("Counts") +
#   ggtitle("Number of times patient ID is repeated in the dataset (subset)")  +
#   geom_bar(stat="identity") +
#   facet_wrap(~gender, ncol = 2)

# +
#   geom_histogram(breaks=seq(135, 5000, by=50),
#                  col = "grey")
  
  # scale_fill_gradient("Counts", low="blue", high="green")
