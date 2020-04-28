# LOAD DATASET
patient <- read.csv("PatientInfo.csv", header = TRUE, sep = ",", na.strings=c(""," ","NA"))

# EKSPLORASI
str(patient)
summary(patient)

which(is.na(patient$country))

# PRAPROSES
# penghapusan atribut
patient$global_num <- NULL
patient$birth_year <- NULL
patient$city <- NULL
patient$infection_order <- NULL
patient$released_date <- NULL
patient$deceased_date <- NULL
patient$symptom_onset_date <- NULL

# mengubah tipe data
patient$patient_id = as.factor(patient$patient_id)
patient$infected_by = as.factor(patient$infected_by)
patient$disease = as.factor(patient$disease)

#patient$sex <- factor(patient$sex, levels=c(0,1), labels=c("male","female"))
#patient$confirmed_date = as.factor(patient$confirmed_date)
#patient$disease[is.na(patient$disease)] <- "unknown"

# penanganan missing value
# untuk atribut contact_number
# masih binguuuung
#patient$contact_number[is.na(patient$contact_number)] <- -99

# untuk atribut age
names(which.max(table(patient$age, useNA = "no")))
patient$age[is.na(patient$age)] <- names(sort(-table(patient$age))[1])
patient$country[is.na(patient$country)] <- names(sort(-table(patient$country))[1])

# untuk atribut infection_case
levels_1 <- levels(patient$infection_case)
levels_1[length(levels_1) + 1] <- "unknown"
patient$infection_case <- factor(patient$infection_case, levels = levels_1)
patient$infection_case[is.na(patient$infection_case)] <- "unknown"

# untuk atribut infected_by
levels_2 <- levels(patient$infected_by)
levels_2[length(levels_2) + 1] <- "unknown"
patient$infected_by <- factor(patient$infected_by, levels = levels_2)
patient$infected_by[is.na(patient$infected_by)] <- "unknown"

# untuk atribut sex
levels_3 <- levels(patient$sex)
levels_3[length(levels_3) + 1] <- "decline to state"
patient$sex <- factor(patient$sex, levels = levels_3)
patient$sex[is.na(patient$sex)] <- "decline to state"

# untuk atribut disease
levels_4 <- levels(patient$disease)
levels_4[length(levels_4) + 1] <- "unknown"
patient$disease <- factor(patient$disease, levels = levels_4)
patient$disease[is.na(patient$disease)] <- "unknown"

summary(patient)

