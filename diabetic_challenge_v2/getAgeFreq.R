#Author: Emmanuelle Coutu-Nadeau
#Date created: 05/09/2019


#Libraries
library(ggplot2)
library(ggpubr)
library(dplyr)

#Import data set (csv)
data <- read.csv("diabetic_data.csv")
names(data)
total_nrow <- nrow(data)
print(nrow)

#helper functions
#Function to get categories(names) in a vector
# print(as.vector(levels(data$age)))
getSubsetName <- function(col){
  return(as.vector(levels(col)))
}

#Function to get value(number) of each range
# print(nrow(select(filter(data, age == "[0-10)"))))

getSubsetValue <- function(ds, col, v){
  c <- vector(mode = "numeric", length = length(v))
  for (i in 1:length(v)) {
    c[i] <- nrow(select(filter(ds, col == v[i])))
  } 
  return(c)
}

# age <- as.factor(data$age)

#Function to get subset values for age
levels.age <- getSubsetName(data$age)
print(levels.age)
value <- getSubsetValue(data, data$age, levels.age)

#create dataframe
df_age <- data.frame(
  group = levels.age,
  value = value,
  pct = getPercentage(value)
)

#pie chart of distribution of age groups in data set of ALL PATIENTS
x_age <- df_age$value
age_labels <- paste(df_age$pct, "%")
head(df_age)
age_chart <- pie(x_age, label = age_labels, main = "Pie chart of the age of all patients", col = rainbow(length(x_age)))
legend("topright", age_chart,levels.age, fill = rainbow(length(x_age)))
