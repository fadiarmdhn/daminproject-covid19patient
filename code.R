# LOAD DATASET
patient <- read.csv("PatientInfonew.csv", header = TRUE, sep = ",", na.strings=c(""," ","NA"))

# Eksplorasi Data
str(patient)
summary(patient)
levels(patient$infection_case)
levels(patient$province)
levels(patient$country)

plot(ylim=c(0,2000),patient$state, xlab="state")

# Menghapus class level deceased
patient <- subset(patient, state != "deceased")
levels(patient$state)
patient$state <- factor(patient$state)
levels(patient$state)

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

str(patient)
summary(patient)


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

#------Eksplorasi data setelah pre-processing------#
# Plotting
counts_date <- table(patient$state, patient$confirmed_date)

barplot(counts_date, main="Distribution of Isolated and Released patient over time",
        xlab=" ", col=c("firebrick4","lightblue"),
        legend=rownames(counts_date), beside=TRUE, ylim=c(0,120))

# Patient's Sex
counts_sex <- table(patient$state, patient$sex)

barplot(counts_sex, main="Distribution of Isolated and Released patient by Patient's sex",
        xlab=" ", col=c("firebrick4","lightblue"),
        legend=rownames(counts_sex), beside=TRUE, ylim=c(0,1200))


library(dplyr)
library(plyr)
# Patient's Age
patient <- patient %>% mutate(agegroup = case_when(age > 78  & age <= 104 ~ '4',
                                              age > 52  & age <= 78 ~ '3',
                                              age > 26  & age <= 52 ~ '2',
                                              age >= 0  & age <= 26 ~ '1'))

counts_age <- table(patient$state, patient$agegroup)
counts_age

barplot(counts_age, main="Distribution of Isolated and Released patient by Patient's age",
        xlab=" ", col=c("firebrick4","lightblue"),
        legend=rownames(counts_age), beside=TRUE, ylim=c(0,1200))

# Infection Case
y <- count(patient$infection_case)
freq <- as.character(names(which(table(patient$infection_case)>100)))
freq
barplot(c(1692,556,111,604,105), main="Patient Distribution by Infection Case",
        xlab="Infection Case", col = c("lightblue4","lightblue2","lightblue1","lightblue3","lightblue"), 
        beside=TRUE, ylim=c(0,2000))
legend("topright", 
       legend = freq, 
       fill = c("lightblue4","lightblue2","lightblue1","lightblue3","lightblue"), ncol = 2,
       cex = 0.65)

# Library for Confusion Matrix
library(caret)
# Library for CTree
library(party)
# Library for ROSE
library(ROSE)

# Sampling
set.seed(423)
ind <- sample(2, nrow(patient), replace = TRUE, prob = c(0.7, 0.3))
trainData <- patient[ind==1,]
testData <- patient[ind==2,]

# Modelling
myFormula <- state ~ sex + age + country + province + infection_case + numDays
Cov_ctree <- ctree(myFormula, data = trainData, controls = ctree_control(minsplit = 10, maxdepth = 5))

# Plotting
plot(Cov_ctree, type="simple")

# Predicting
Cov_pred <- predict(Cov_ctree, newdata = testData)
Cov_pred

# Evaluating
confusionMatrix(Cov_pred, testData$state)


#------Re-sampling------#

table(trainData$state)

# oversampling
over <- ovun.sample(myFormula, data = trainData, method = 'over', N = (1346*2))$data
table(over$state)

# undersampling
under <- ovun.sample(myFormula, data = trainData, method = 'under', N = (985*2))$data
table(under$state)

# both
both <- ovun.sample(myFormula, data = trainData, method = 'both', N = (1346+985))$data
table(both$state)

# Modelling for resampling
Cov_ctree.both <- ctree(myFormula, data = both, controls = ctree_control(minsplit = 10, maxdepth = 5))
Cov_ctree.under <- ctree(myFormula, data = under, controls = ctree_control(minsplit = 10, maxdepth = 5))
Cov_ctree.over <- ctree(myFormula, data = over, controls = ctree_control(minsplit = 10, maxdepth = 5))

# Predicting for resampling
Cov_pred.both <- predict(Cov_ctree.both, newdata = testData)
Cov_pred.under <- predict(Cov_ctree.under, newdata = testData)
Cov_pred.over <- predict(Cov_ctree.over, newdata = testData)

# Evaluating for resampling
confusionMatrix(Cov_pred.both, testData$state, positive="released")
confusionMatrix(Cov_pred.under, testData$state, positive="released") # under = the best (81.52%)
confusionMatrix(Cov_pred.over, testData$state, positive="released")

# Plotting ctree for undersampling
plot(Cov_ctree.under, type="simple")

# Tuning parameter
control <- ctree_control(minsplit = 10,
                          minbucket = round(10 / 3),
                          maxdepth = 6)

tune_fit.under <- ctree(myFormula, data = under, controls = control)
pred.under <- predict(tune_fit.under, newdata = testData)
confusionMatrix(pred.under, testData$state, positive="released")

plot(tune_fit.under, type="simple")

# Generate tree into png
png("undersamplingDT.png", res=80, height=8000, width=24000) 
plot(tune_fit.under) 
dev.off()
