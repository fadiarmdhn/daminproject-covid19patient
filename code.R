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
patient$contact_number <- NULL

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

# Confusion Matrix
library(caret)
# CTree
library(party)

# Sampling
set.seed(423)
ind <- sample(2, nrow(DataF), replace = TRUE, prob = c(0.7, 0.3))
trainData <- patient[ind==1,]
testData <- patient[ind==2,]

# Modelling
myFormula <- state ~ sex + age + country + province + infection_case + infected_by
Cov_ctree <- ctree(myFormula, data = trainData, controls = ctree_control(minsplit = 10, maxdepth = 5))

# Plotting
plot(Cov_ctree, type="simple")

# Predicting
Cov_pred <- predict(Cov_ctree, newdata = testData)

# Evaluating
confusionMatrix(Cov_pred, testData$state)

# Tuning Hyper-parameter
accuracy_tune <- function(fit) {
  predict_unseen <- predict(fit, testData, type = 'class')
  table_mat <- table(testData$state, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}

control <- ctree.control(minsplit = 10,
                         minbucket = round(5 / 3),
                         maxdepth = 5,
                         cp = 0)
tune_fit <- ctree(myFormula, data = trainData, method = 'class', control = control)
accuracy_tune(tune_fit)
