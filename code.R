# LOAD DATASET
patient <- read.csv("PatientInfonew.csv", header = TRUE, sep = ",", na.strings=c(""," ","NA"))

# Eksplorasi Data
str(patient)
summary(patient)
levels(patient$infection_case)
levels(patient$province)
levels(patient$country)

# PRAPROSES
# Mengubah tipe data
patient$patient_id = as.factor(patient$patient_id)
patient$infected_by = as.factor(patient$infected_by)
patient$symptom_onset_date = as.Date(patient$symptom_onset_date)
patient$confirmed_date = as.Date(patient$confirmed_date, "%Y-%m-%d")
patient$released_date = as.Date(patient$released_date)
patient$deceased_date = as.Date(patient$deceased_date)
patient$global_num = as.factor(patient$global_num)
patient$disease = as.factor(patient$disease)

# menghapus instances yang tidak memiliki confirmed_date
patient<-patient[-which(is.na(patient$confirmed_date)),]

# penghapusan atribut
patient$global_num <- NULL
patient$city <- NULL
patient$infection_order <- NULL
patient$released_date <- NULL
patient$deceased_date <- NULL
patient$symptom_onset_date <- NULL
patient$contact_number <-NULL

# mengisi missing value pada atribut infection_case dengan modus
patient$infection_case[is.na(patient$infection_case)] <- names(sort(-table(patient$infection_case))[1])
# menghapus atribut age
patient$age <- NULL
# mengisi missing value pada atribut birth_year dengan mean
patient$birth_year[is.na(patient$birth_year)] <- floor(mean(patient$birth_year, na.rm=TRUE))

### Mencari usia pasien berdasarkan birth_year (tahun lahir) ###
n <- nrow(patient)

for(i in 1:n) {
  today <- 2020
  patient$birth_year[i] = today - patient$birth_year[i]
}
names(patient)[names(patient) == "birth_year"] <- "age"

summary(patient)
str(patient)

# mengisi missing value;
# pada atribut infected_by
levels_2 <- levels(patient$infected_by)
levels_2[length(levels_2) + 1] <- "unknown"
patient$infected_by <- factor(patient$infected_by, levels = levels_2)
patient$infected_by[is.na(patient$infected_by)] <- "unknown"

# pada atribut sex
levels_3 <- levels(patient$sex)
levels_3[length(levels_3) + 1] <- "decline to state"
patient$sex <- factor(patient$sex, levels = levels_3)
patient$sex[is.na(patient$sex)] <- "decline to state"

# pada atribut disease
levels_4 <- levels(patient$disease)
levels_4[length(levels_4) + 1] <- "unknown"
patient$disease <- factor(patient$disease, levels = levels_4)
patient$disease[is.na(patient$disease)] <- "unknown"

# menambahkan atribut numDays pada dataset
patient$numDays <- as.Date("2020-04-30","%Y-%m-%d") - as.Date((patient$confirmed_date),"%Y-%m-%d")
  
patient$numDays <- as.numeric(patient$numDays)

str(patient)
summary(patient)

# Library for Confusion Matrix
library(caret)
# Library for CTree
library(party)

# Sampling
set.seed(423)
ind <- sample(2, nrow(DataF), replace = TRUE, prob = c(0.7, 0.3))
trainData <- patient[ind==1,]
testData <- patient[ind==2,]

# Modelling
myFormula <- state ~ sex + age + country + province + infection_case + numDays
Cov_ctree <- ctree(myFormula, data = trainData, controls = ctree_control(minsplit = 10, maxdepth = 5))

# Plotting
plot(Cov_ctree, type="simple")

# Predicting
Cov_pred <- predict(Cov_ctree, newdata = testData)

# Evaluating
confusionMatrix(Cov_pred, testData$state)

# Tuning Hyper-parameter
accuracy_tune <- function(fit) {
  predict_unseen <- predict(fit, testData)
  table_mat <- table(testData$state, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}

control <- ctree.control(minsplit = 10,
                         minbucket = round(10 / 3),
                         maxdepth = 5)
tune_fit <- ctree(myFormula, data = trainData, control = control)
accuracy_tune(tune_fit)

plot(tune_fit, type="simple")
