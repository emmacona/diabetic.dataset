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
gender.vector <- getSubsetName(data$gender)
gender.value <- getSubsetValue(data, data$gender, gender.vector)
gender.pct <- getPercentage(gender.value)

df_gender <- data.frame(
  group = gender.vector,
  value = gender.value,
  pct = gender.pct
)
head(df_gender)
gender.x <- df_gender$value
gender.labels <- paste(df_gender$pct, "%")
gender_chart <- pie(gender.x, gender.labels, main = "Gender pie chart", col = rainbow(length(gender.x)))
legend("right", gender_chart, gender.vector, fill = rainbow(length(gender.x)))
