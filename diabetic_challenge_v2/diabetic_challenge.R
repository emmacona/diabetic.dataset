#Author: Emmanuelle Coutu-Nadeau
#Date created: 05/07/2019

#Import data set (csv)
data <- read.csv("diabetic_data.csv")
library("foreach")

#Global variables
weight <- data$weight
medical_specialty <- data$medical_specialty
payer_code <- data$payer_code
examinde <- data$examide
citoglipton <- data$citoglipton

count_weight <- 0
count_specialty <- 0
count_payer <- 0
proportion <- 0
count_examinde <- 0

#1 - If over 40% of weight, payer_code, and medical_specialty is missing == don't consider data
#Check for missing values == ?
print((length(weight)))

foreach(i=1:length(weight)) %do%
  if(weight[i] == "?"){
    count_weight = count_weight +1
  }
print(count_weight)

foreach(i=1:length(medical_specialty)) %do%
  if(medical_specialty[i] == "?"){
    count_specialty = count_specialty +1
  }
print(count_specialty)

foreach(i=1:length(payer_code)) %do%
  if(payer_code[i] == "?"){
    count_payer = count_payer +1
  }
print(count_payer)


proportion = (count_weight + count_payer + count_specialty)/(length(weight) + length(payer_code) + length(medical_specialty))*100

if(proportion > 40){
  print("This data set is missing over 40% of weight, medical specialty, and payer code information.")
}

#2 check if more than 1 value in examinde and citoglipton
foreach(i=1:length(examinde)) %do%
  if(identical(examinde[i], "No")|identical(examinde[i], "Yes")){
    print(examinde[i]+" has an invalid examinde value.")
    count_examinde = count_examinde +1
  }

print("The database has "+ count_examinde + " incorrect examinde entrie(s).")
