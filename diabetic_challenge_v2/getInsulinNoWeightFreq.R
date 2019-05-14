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

#Helper functions from getAgeFreq.R
getPercentage <- function(val){
  x <- round(100* (val / sum(val)), 1)
  return(x)
}

#set up factors (aka all that are categories-- not numerical, continuous data)
weight <- getSubsetName(data$weight)
insulin.category <- getSubsetName(data$insulin)
insulin.value <- getSubsetValue(data, data$insulin, insulin.category)

# #Looking at patients with missing weight data
# missing_weight <- subset(data, data$weight == "?")
# insulin_down <- subset(missing_weight, missing_weight$insulin == "Down")
# down_nb <- nrow(insulin_down)
# insulin_no <- subset(missing_weight, missing_weight$insulin == "No")
# no_nb <- nrow(insulin_no)
# insulin_steady <- subset(missing_weight, missing_weight$insulin == "Steady")
# steady_nb <- nrow(insulin_steady)
# insulin_up <- subset(missing_weight, missing_weight$insulin == "Up")
# up_nb <- nrow(insulin_up)

df_insulin <- data.frame(
  group = insulin.category,
  value = insulin.value,
  pct = getPercentage(insulin.value)
)

head(df_insulin)

x_insulin<- df_insulin$value
labels_insulin <- paste(df_insulin$pct, "%")
head(df_insulin)
insulin_chart <- pie(x_insulin, labels_insulin, main = "Insulin for missing weight patients pie chart", col = rainbow(length(x_insulin)))
legend("right", insulin_chart,insulin.category, fill = rainbow(length(x_insulin)))


