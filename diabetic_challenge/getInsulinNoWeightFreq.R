#Author: Emmanuelle Coutu-Nadeau
#Date created: 05/09/2019

#Uniqueness Graph
source("Functions.R")

#set up factors (aka all that are categories-- not numerical, continuous data)
weight <- getSubsetName(data$weight)
insulin.category <- getSubsetName(data$insulin)
insulin.value <- getSubsetValue(data, data$insulin, insulin.category)

# #Looking at patients with missing weight data
missing_weight <- subset(data, data$weight == "?")
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
insulin_chart <- pie(x_insulin, labels_insulin, main = "Insulin for all patients pie chart", col = rainbow(length(x_insulin)))
legend("bottom", insulin_chart,insulin.category, fill = rainbow(length(x_insulin)), horiz = TRUE)


#missing weight
insulin.category.noweight <- getSubsetName(missing_weight$insulin)
insulin.value.noweight <- getSubsetValue(missing_weight, missing_weight$insulin, insulin.category.noweight)

df_insulin_no_weight <- data.frame(
  group = insulin.category.noweight,
  value = insulin.value.noweight,
  pct = getPercentage(insulin.value.noweight)
)

x_insulin_no_weight<- df_insulin_no_weight$value
labels_insulin_no_weight<- paste(df_insulin_no_weight$pct, "%")
insulin_chart_no_weight <- pie(x_insulin_no_weight, labels_insulin_no_weight, main = "Insulin for patients without weight info pie chart", col = rainbow(length(x_insulin)))
legend("bottom", insulin_chart_no_weight,insulin.category.noweight, fill = rainbow(length(x_insulin)), horiz = TRUE)

par(mfrow=c(1,2))
insulin_chart <- pie(x_insulin, labels_insulin, main = "Insulin for all patients pie chart", col = rainbow(length(x_insulin)))
insulin_chart_no_weight <- pie(x_insulin_no_weight, labels_insulin_no_weight, main = "Insulin for patients without weight info pie chart", col = rainbow(length(x_insulin)))
legend("bottom", insulin_chart_no_weight,insulin.category.noweight, fill = rainbow(length(x_insulin)))

