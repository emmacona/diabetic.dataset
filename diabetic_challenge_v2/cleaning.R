#Author: Emmanuelle Coutu-Nadeau

#Libraries
library(ggplot2)
library(ggpubr)
library(dplyr)
library(reshape)
library(tidyr)
library(naniar)

#Import data set (csv)
data <- read.csv("diabetic_data.csv")
names(data)
total_nrow <- nrow(data)

#Replace all missing values with NA
data[data =='?'] = NA
data[data =='Unknown/Invalid'] = NA
View(data)

#Add column with NA count
data$na_count <- apply(data, 1, function(x) sum(is.na(x)))

#Find under 30% missing
all_under_30pct <- function(ds){
  pct_in_absolute = 30*(ncol(ds)-1)/100
  ss = subset(ds, ds$na_count < pct_in_absolute)
  return(ss)
}

clean.data <- all_under_30pct(data)

if(nrow(clean.data) == nrow(data)){
  print("There are no entries with over 30% of missing information.")
}


#Convert rows into columns
#melt may not be the best option
melt.data <- melt(data, id = "patient_nbr")
View(melt.data)

#Trying gather function
#not so good either -- melt is better
gather.data <- gather(data, key = "encounter_id", convert = FALSE)
View(gather.data)

#Trying slice function
slice.data <- slice(data, 1:25)
View(slice.data)

#Idea: to get patient info
#1 find nb of encounter ids per patient --> n
#2 slice n for perticular patient
#3 functions performed on individual patient's subset

#1
numberOfEncounters <- function(ds, patient.ID){
  ss <- subset(ds, patient_nb == patient.ID)
  return(nrow(ss))
}
#2
slice.patient <- function(ds, patient.ID){
  L = ds$patient_nbr == patient.ID
  return(ds[L,])
}


#Remove if over 30% missing info
perc.missing <- function(ds, row){
  row.val <- as.vector(ds[row,])
  count = 0
  for(i in 1:length(column.names)){
    if(row.val[i] == "?" | row.val[i] == "NA" | row.val[i] ==  "Unknown/Invalid" | row.val[i] == ""){
      count <- count+1
    }
  }
  return(100*(count/length(column.names)))
}

perc.missing.patient <- function(ds, patient.ID){
  sp = slice.patient(ds, patient.ID)
  n = nrow(sp)
  pct.vector = vector(length = n)
  for(i in 1:n){
    pct.vector[i] = perc.missing(sp, i)
  }
  return(pct.vector)
}


View(slice.patient(data, 1152))
perc.missing.patient(data, 1152)



