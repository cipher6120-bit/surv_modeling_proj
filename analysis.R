library(readr)
library(survival)
library(tidyverse)
library(chattr)
library(mice)
library(MASS)
data <- read_csv("archive/healthcare-dataset-stroke-data.csv")

is.tibble(data)
data %>% print(width 
               = 100)
summary(data)


#Convert the categorical variables to factors
data$gender <- as.factor(data$gender)
data$hypertension <- as.factor(data$hypertension)
data$heart_disease <- as.factor(data$heart_disease)
data$ever_married <- as.factor(data$ever_married)
data$work_type <- as.factor(data$work_type)
data$Residence_type <- as.factor(data$Residence_type)
data$smoking_status <- as.factor(data$smoking_status)
data$bmi <- as.numeric(data$bmi)
stroke.surv <- Surv(data$age, data$stroke)
#plot the survival curves based on genders
km_fit_gender <- survfit(stroke.surv ~ gender, data = data)
plot(km_fit_gender, col = 1:3, xlab = "Age", ylab = "Survival Probability", 
     main = "Kaplan-Meier Survival Curves by Gender", xlim = c(40,90))
legend("bottomright", 
       legend = levels(data$gender), col = 1:3, lty = 1, cex = 0.8)
unique(data$gender)
#it seems like gender_other does not have any stroke events
#let's check how many rows have gender other
data %>% filter (gender == "Other") %>% nrow()
#it is only one row
#let's remove those rows from the data
data <- data %>% filter(gender != "Other")
#fit and plot again
stroke.surv <- Surv(data$age, data$stroke)
km_fit_gender <- survfit(stroke.surv ~ gender, data = data)
plot(km_fit_gender, xlab = "Age", col = c(1,2), ylab = "Survival Probability", 
     main = "Kaplan-Meier Survival Curves by Gender", xlim = c(40,90))
legend("bottomright", 
       legend = levels(data$gender)[c(1,2)], col = 1:2, lty = 1, cex = 0.8)
# Fit a Cox proportional hazards model
coxph_model <- coxph(stroke.surv~smoking_status, data = data)
summary(coxph_model) 
# Plot the survival curves for different smoking statuses
km_fit_smoking <- survfit(stroke.surv ~ smoking_status, data = data)
plot(km_fit_smoking, col = 1:4, xlab = "Age", ylab = "Survival Probability", 
main = "Kaplan-Meier Survival Curves by Smoking Status", xlim = c(40,90))
legend("bottomright", 
legend = levels(data$smoking_status), col = 1:4, lty = 1, cex = 0.8)
# Check proportional hazards assumption
ph_test <- cox.zph(coxph_model)
print(ph_test)
plot(ph_test)
#Looks good, the smoking status variable seems to satisfy the proportional hazards assumption.

#but the unknown smoking status seems to have a high p value, lets true imputing them using mice
#let's see how many rows does it have
nrow(data %>% filter(smoking_status == "Unknown"))
#1544 rows of unknown status is kind of a lot
#let's impute them using mice
#make the unknown status of smoking into NA
data$smoking_status <- ifelse(data$smoking_status == "Unknown", NA, as.character(data$smoking_status))
data$smoking_status <- as.factor(data$smoking_status)
imputed_data <- mice(data, m = 5, maxit = 50, method = 'pmm', seed = 123)
#let's check one instance of the imputed data
complete(imputed_data)$smoking_status %>% is.na() %>% sum()

imputed_ph <- with(imputed_data, coxph(stroke.surv ~ smoking_status))
pooled_ph <- pool(imputed_ph)
summary(pooled_ph)

#and let's plot the keplan meier curve again for different smoking status
#imputed_complete data is one instance of the imputed data set
imputed_complete_data <- complete(imputed_data)
print(imputed_complete_data, width = 100)

km_fit_smoking_imputed <- survfit(Surv(age, stroke) ~ smoking_status, data = imputed_complete_data)
plot(km_fit_smoking_imputed, col = 1:4, xlab = "Age", ylab = "Survival Probability", 
     main = "Kaplan-Meier Survival Curves by Smoking Status (Imputed)", xlim = c(40,90))
legend("bottomright", 
       legend = levels(imputed_complete_data$smoking_status), col = 1:3, lty = 1, cex = 0.8)
#both coxph and the KM curve suggest that smoking status isn't a significant predictor for stroke
#interesting

#let's try to find the model that maximizes AIC
#exclude age as it is already used as the event time
aic_model <- coxph(stroke.surv ~ ., data = imputed_complete_data[,c(2,4:11)]) %>% 
  stepAIC(direction = "both")
summary(aic_model)
#it has some pretty garbage result
#work type has infinite upper limit
#let's try to remove work type and see the result
aic_model2 <- coxph(stroke.surv ~ . - work_type, data = imputed_complete_data[,c(2,4:11)]) %>% 
  stepAIC(direction = "both")
summary(aic_model2)
#it looks like formerly smoked and never smoked have similar effect
#let's combine them into one level named not_smoking
imputed_complete_data <- imputed_complete_data %>%
  mutate(smoking_status = recode(smoking_status,
                                 "formerly smoked" = "not_smoking",
                                 "never smoked" = "not_smoking"))
aic_model3 <- coxph(stroke.surv ~ . - work_type, data = imputed_complete_data[,c(2,4:11)]) %>% 
  stepAIC(direction = "both")
summary(aic_model3)
km_fit_smoking_imputed2 <- survfit(Surv(age, stroke) ~ smoking_status, data = imputed_complete_data)

#plot km curve of not smoking against smokes
plot(km_fit_smoking_imputed2, col = 1:2, xlab = "Age", ylab = "Survival Probability", 
     main = "Kaplan-Meier Survival Curves by Smoking Status (Imputed)", xlim = c(40,90))
legend("bottomright", 
       legend = levels(imputed_complete_data$smoking_status), col = 1:2, lty = 1, cex = 0.8)
