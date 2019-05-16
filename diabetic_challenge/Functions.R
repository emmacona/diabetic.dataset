#functions and imports of libraries and data set

#Libraries
library(ggplot2)
library(ggpubr)
library(dplyr)
library(reshape)
library(tidyr)
library(naniar)
library(gridExtra)

#Import data set (csv)
data <- read.csv("diabetic_data.csv")
data.category <- c(names(data))
data.type <- sapply(data, typeof)
total_nrow <- nrow(data)

#Patients
unique_patients <- data[!(duplicated(data$patient_nbr)),]
print(unique_patients)
print(nrow(unique_patients))
all_repeated_patients <- data[duplicated(data$patient_nbr)|duplicated(data$patient_nbr, fromLast=TRUE),]
print(nrow(all_repeated_patients))
repeated_patients <- all_repeated_patients[unique(all_repeated_patients$patient_nbr),]
print(nrow(repeated_patients))

#Helper functions from getAgeFreq.R
getPercentage <- function(val){
  x <- round(100* (val / sum(val)), 1)
  return(x)
}

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

#CLEANING FUNCTIONS
#Find under 30% missing
all_under_30pct <- function(ds){
  pct_in_absolute = 30*(ncol(ds)-3)/100
  ss = subset(ds, ds$na_count < pct_in_absolute)
  return(ss)
}

#find dead patients
clean.dead <- function(ds){
  dppl <- subset(ds, discharge_disposition_id == "11"| discharge_disposition_id == "19" | discharge_disposition_id == "20" |discharge_disposition_id == "21")
  return(ds[!ds$patient_nbr %in% dppl$patient_nbr,])
}



multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}




