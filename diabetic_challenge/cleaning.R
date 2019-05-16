#Author: Emmanuelle Coutu-Nadeau

source("Functions.R")

#Replace all missing values with NA
data[data =='?'] = NA
data[data =='Unknown/Invalid'] = NA
View(data)

#Add column with NA count
data$na_count <- apply(data, 1, function(x) sum(is.na(x)))

clean.data <- all_under_30pct(data)

if(nrow(clean.data) == nrow(data)){
  print("There are no entries with over 30% of missing information.")
}

max(data$na_count)
View(clean.data)

no_dead <- clean.dead(clean.data)

paste("The number of dead patients is ", nrow(clean.data) - nrow(no_dead))

# #Convert rows into columns
# #melt may not be the best option
# melt.data <- melt(data, id = "patient_nbr")
# View(melt.data)
# 
# #Trying gather function
# #not so good either -- melt is better
# gather.data <- gather(data, key = "encounter_id", convert = FALSE)
# View(gather.data)
# 
# #Trying slice function
# slice.data <- slice(data, 1:25)
# View(slice.data)
# 
# View(slice.patient(data, 1152))
# perc.missing.patient(data, 1152)



