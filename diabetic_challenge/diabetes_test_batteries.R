#Author: Emmanuelle Coutu-Nadeau
#Date created: 05/09/2019

#Libraries
library(ggplot2)
library(ggpubr)

#Import data set (csv)
data <- read.csv("diabetic_data.csv")
total_nrow <- nrow(data)
print(nrow)

#set up factors (aka all that are categories-- not numerical, continuous data)
race <- as.factor(data$race)
gender <- as.factor(data$gender)
age <- as.factor(data$age)
weight <- as.factor(data$weight)
admission_type_id <- as.factor(data$admission_type_id)
discharge_disposition_id <- as.factor(data$discharge_disposition_id)

#set up numerical data columns
encounter_id <- data$encounter_id
patient_nb <- data$patient_nbr
admission_type_id_cont <- data$admission_type_id
inPatients <- data$number_inpatient
outPatients <- data$number_outpatient
emergencyPatients <- data$number_emergency
timeInHospital <- data$time_in_hospital
numProcedures <- data$num_procedures


#Getting info about data set
#ummary of data
summary(data)

#Patients
ggplot(data = data, aes(x= patient_nb, y = encounter_id)) + geom_point()
hist(encounter_id)
hist(patient_nb)
patient_nb_plot <- ggplot(data = data, aes(patient_nb))
patient_nb_plot +
  xlab("Patient Number") +
  ylab("Counts") +
  ggtitle("Distribution of patient Numbers")  +
  geom_histogram(breaks=seq(135, 200000, by=1),
                 col = "grey",
                 aes(fill=..count..)) +
  scale_fill_gradient("Counts", low="blue", high="green")

inPatients_plot_cont <- ggplot(data = data, aes(inPatients))
inPatients_plot_cont + geom_freqpoly() + ggtitle("Distribution of in-patients visits")
outPatients_plot_cont <- ggplot(data = data, aes(outPatients))
outPatients_plot_cont + geom_freqpoly() + ggtitle("Distribution of out-patients visits")
emergencyPatients_plot_cont <- ggplot(data = data, aes(emergencyPatients))
emergencyPatients_plot_cont + geom_freqpoly() + ggtitle("Distribution of emergency-patients visits")


female <- subset(data, data$gender == "Female")
female_nb <- nrow(female)
male <- subset(data, data$gender == "Male")
male_nb <- nrow(male)
unknown <- subset(data, data$gender == "Unknown/Invalid")
unknown_nb <- nrow(unknown)
df_gender <- data.frame(
  group = c("Female", "Male", "Unknown/Invalid"),
  value = c(female_nb, male_nb, unknown_nb)
)
x <- df_gender$value
labels <- df_gender$group
head(df_gender)
chart_colors <- c("pink", "cadetblue1", "azure4")
gender_chart <- pie(x, labels, main = "Gender pie chart", col = chart_colors)

age_0 <- subset(data, data$age == "[0-10)")
age_0_nb <- nrow(age_0)
age_1 <- subset(data, data$age == "[10-20)")
age_1_nb <- nrow(age_1)
age_2 <- subset(data, data$age == "[20-30)")
age_2_nb <- nrow(age_2)
age_3 <- subset(data, data$age == "[30-40)")
age_3_nb <- nrow(age_3)
age_4 <- subset(data, data$age == "[40-50)")
age_4_nb <- nrow(age_4)
age_5 <- subset(data, data$age == "[50-60)")
age_5_nb <- nrow(age_5)
age_6 <- subset(data, data$age == "[60-70)")
age_6_nb <- nrow(age_6)
age_7 <- subset(data, data$age == "[70-80)")
age_7_nb <- nrow(age_7)
age_8 <- subset(data, data$age == "[80-90)")
age_8_nb <- nrow(age_8)
age_9 <- subset(data, data$age == "[90-100)")
age_9_nb <- nrow(age_9)

df_age <- data.frame(
  group = c("[0-10)", "[10-20)", "[20-30)","[30-40)", "[40-50)","[50-60)","[60-70)", "[70-80)", "[80-90)", "[90-100)"),
  value = c(age_0_nb, age_1_nb, age_2_nb, age_3_nb, age_4_nb, age_5_nb, age_6_nb, age_7_nb, age_8_nb, age_9_nb)
)
x_age <- df_age$value
labels_age <- df_age$group
head(df_age)
age_chart <- pie(x_age, labels_age, main = "Age pie chart", col = rainbow(length(x_age)))


#Admissions
admission_plot_continuous <- ggplot(data = data, aes(admission_type_id_cont))
admission_plot_continuous + geom_freqpoly() # we observe that this data is not

hist(admission_type_id_cont)

admission_plot <- ggplot(data = data, aes(admission_type_id))
admission_plot +
  xlab("Admission Type ID") +
  ylab("Counts") +
  ggtitle("Distribution of admission types")  +
  geom_histogram(breaks=seq(0, 10, by=1),
                 col = "grey",
                 aes(fill=..count..)) +
  scale_fill_gradient("Counts", low="blue", high="green")

#Time in hospital
time_plot <- ggplot(data = data, aes(timeInHospital))
time_plot +
  xlab("Number of days between admission and discharge") +
  ylab("Frequency") +
  ggtitle("Distribution of time in hospital")  +
  geom_histogram(breaks=seq(0, 10, by=1),
                 col = "grey",
                 aes(fill=..count..)) +
  scale_fill_gradient("Counts", low="blue", high="green") +
  geom_vline(xintercept=mean(timeInHospital), color="red", lwd = 2) +
  geom_vline(xintercept=median(timeInHospital), color="orange", lwd = 2)

#Looking at patients with missing weight data
missing_weight <- subset(data, data$weight == "?")
insulin_down <- subset(missing_weight, missing_weight$insulin == "Down")
down_nb <- nrow(insulin_down)
insulin_no <- subset(missing_weight, missing_weight$insulin == "No")
no_nb <- nrow(insulin_no)
insulin_steady <- subset(missing_weight, missing_weight$insulin == "Steady")
steady_nb <- nrow(insulin_steady)
insulin_up <- subset(missing_weight, missing_weight$insulin == "Up")
up_nb <- nrow(insulin_up)

df_insulin <- data.frame(
  group = c("Down", "No", "Steady", "Up"),
  value = c(down_nb, no_nb, steady_nb, up_nb)
)

x_insulin<- df_insulin$value
labels_insulin <- df_insulin$group
head(df_insulin)
insulin_chart <- pie(x_insulin, labels_insulin, main = "Insulin for missing weight patients pie chart", col = rainbow(length(x_age)))


#Age distribution

# #getting threshold info
# weight <- subset(data, data$weight == "?")
# payer_code <- subset(data, data$payer_code == "?")
# medical_specialty <- subset(data, data$medical_specialty  == "?")
# nb_missing <- max(nrow(weight), nrow(payer_code), nrow(medical_specialty))
# missing_percentage <- (100*(nb_missing)/nrow)
# print(cat("There is ", missing_percentage, "% missing weight, payer_code, and medical specialty information."))
#
# examinde <- subset(data, data$examide != "No" | data$examide != "Yes")
# citoglipon <- subset(data, data$citoglipton != "No" | data$citoglipton != "Yes" )
# check2 <- nrow(examinde) + nrow(citoglipon)
# print(cat("There are ", check2, " incorrect cito/examinde values.", sep = " "))
#
# gender <- subset(data, data$gender != "Male" | data$gender != "Female")
# print(cat("There are ", nrow(gender),  " missing gender information.", sep= " "))
#
# diagnose <- subset(data, is.null(data$number_diagnoses))
# print(cat("There are ", nrow(diagnose),  " missing diagnose information.", sep= " "))
#
# died <- subset(data, data$readmitted == "NO")
# print(cat("There are ", nrow(died),  " patients that weren't readmitted (died).", sep= " "))
