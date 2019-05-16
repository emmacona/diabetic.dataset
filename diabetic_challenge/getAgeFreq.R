#Author: Emmanuelle Coutu-Nadeau
#Date created: 05/09/2019

source("Functions.R")

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

df_age$pct[df_age$pct < 1] = " "

x_age <- df_age$value
age_labels <- paste(df_age$pct, "%")
age_labels[age_labels == "  %"] = " "
head(df_age)
age_chart <- pie(x_age, label = age_labels, main = "Pie chart of the age of all patients", col = rainbow(length(x_age)))
legend("topright", age_chart,levels.age, fill = rainbow(length(x_age)))

#pie chart of age for unique patients
unique_group <- getSubsetName(unique_patients$age)
unique_value <- getSubsetValue(unique_patients, unique_patients$age, unique_group)
unique_pct <- getPercentage(unique_value)

df_age_unique <- data.frame(
  group = unique_group,
  value = unique_value,
  pct = unique_pct
)

df_age_unique$pct[df_age_unique$pct < 1] = " "
x_unique_age <- df_age_unique$value
unique_age_labels <- paste(df_age_unique$pct, "%")
unique_age_labels[unique_age_labels == "  %"] = " "

#pie chart of age for repeated patients
repeated_group <- getSubsetName(all_repeated_patients$age)
repeated_value <- getSubsetValue(all_repeated_patients, all_repeated_patients$age, repeated_group)
repeated_pct <- getPercentage(repeated_value)

df_age_repeated <- data.frame(
  group = repeated_group,
  value = repeated_value,
  pct = repeated_pct
)

df_age_repeated$pct[df_age_repeated$pct < 1] = " "
x_repeated_age <- df_age_repeated$value
repeated_age_labels <- paste(df_age_repeated$pct, "%")
repeated_age_labels[repeated_age_labels == "  %"] = " "


par(mfrow=c(1,2) )
age_chart_unique <- pie(x_unique_age, label = unique_age_labels, main = "Pie chart of the age of unique patients", col = rainbow(length(x_unique_age)))
age_chart_repeated <- pie(x_repeated_age, label = repeated_age_labels, main = "Pie chart of the age of repeated patients", col = rainbow(length(x_repeated_age)))
legend("bottomleft", age_chart,levels.age, fill = rainbow(length(x_repeated_age)), cex=0.5)


