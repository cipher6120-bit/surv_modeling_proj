library(readr)
library(survival)
library(tidyverse)
library(mice)
library(MASS)

#load the data
data <- read_csv("archive/healthcare-dataset-stroke-data.csv")

#view the data
print(data, n = 5, width = Inf)

#convert the data to appropriate data types
data <- data %>% mutate(
  id = as.character(id),
  age = as.numeric(age),
  avg_glucose_level = as.numeric(avg_glucose_level),
  bmi = as.numeric(bmi),
  across(c(gender, hypertension, heart_disease, ever_married,
           work_type, Residence_type, smoking_status), as.factor)
  
)

#view the table again
print(data, n = 5, width = Inf)

#show tables of factor variables
table(data$gender)
table(data$hypertension, useNA = "always")
table(data$heart_disease, useNA = "always")
table(data$ever_married, useNA = "always")
table(data$work_type, useNA = "always")
table(data$Residence_type, useNA = "always")
table(data$smoking_status, useNA = "always")
table(data$stroke, useNA = "always")

#there is only one "Other" observation in gender
#we should drop it so it doesn't create anomalies in the analysis
data <- data %>% filter(gender != "Other")
data <- data %>% droplevels.data.frame()

#and there are 1483 observations of "Unkown" in smoking status
#drop it as well for complete case analysis
data <- data %>% filter(smoking_status != "Unknown")
data <- data %>% droplevels.data.frame()
#check for missing values in numeric variables
sapply(data %>% dplyr::select(age, avg_glucose_level, bmi), function(x) sum(is.na(x)))

#drop the rows with NA values for bmi for a complete case analysis
data <- data %>% drop_na()

#create the Surv object
stroke_surv <- Surv(data$age, data$stroke)

#create the KM object and plot it out
km_fit_gender <- survfit(stroke_surv ~ gender, data = data)
plot(km_fit_gender, col = 1:2, xlab = "Age", ylab = "Survival Probability", 
     main = "Kaplan-Meier Survival Curves by Gender", xlim = c(40,90))
legend("bottomright", legend = levels(data_complete$gender), 
       col = 1:2, lty = 1, cex = 0.8)

coxph_model <- coxph(stroke_surv ~ smoking_status, data = data)
summary(coxph_model)

#so it seems like we still have the same conclusion
#smoking kills but formerly smoked has no significant difference from never smoked
#let's try to combine them
data <- data %>% mutate(
  smoking_status = fct_collapse(smoking_status,
    not_smoking = c("never smoked", "formerly smoked"),
    smokes = "smokes"
  )
)

#fit a multivariate coxph model
coxph_full <- coxph(stroke_surv ~ . - age - stroke - id, 
                    data = data)
aic_model <- stepAIC(coxph_full, direction = "both")
summary(aic_model)

