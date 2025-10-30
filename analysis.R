library(readr)
library(survival)
library(tidyverse)
library(mice)
library(MASS)

# Load and inspect data
#https://www.kaggle.com/datasets/fedesoriano/stroke-prediction-dataset/data
#massive thanks to fedesoriano for providing the dataset
data <- read_csv("archive/healthcare-dataset-stroke-data.csv")

# Data overview
cat("Data overview:\n")
print(data, width = 100)
summary(data)

# Data preprocessing
data_clean <- data %>%
  # Remove single "Other" gender observation
  filter(gender != "Other") %>%
  # Convert variables to appropriate types
  mutate(
    across(c(gender, hypertension, heart_disease, ever_married, 
             work_type, Residence_type, smoking_status), as.factor),
    bmi = as.numeric(bmi),
    # Convert smoking_status "Unknown" to NA for imputation
    smoking_status = ifelse(smoking_status == "Unknown", NA, 
                            as.character(smoking_status)) %>% as.factor()
  )

# Create survival object
stroke.surv <- Surv(data_clean$age, data_clean$stroke)

# Kaplan-Meier analysis by gender
km_fit_gender <- survfit(stroke.surv ~ gender, data = data_clean)
plot(km_fit_gender, col = 1:2, xlab = "Age", ylab = "Survival Probability", 
     main = "Kaplan-Meier Survival Curves by Gender", xlim = c(40,90))
legend("bottomright", legend = levels(data_clean$gender), 
       col = 1:2, lty = 1, cex = 0.8)

# Initial smoking status analysis
cat("\nInitial smoking status counts:\n")
table(data_clean$smoking_status, useNA = "always")

coxph_model <- coxph(stroke.surv ~ smoking_status, data = data_clean)
summary(coxph_model)

km_fit_smoking <- survfit(stroke.surv ~ smoking_status, data = data_clean)
plot(km_fit_smoking, col = 1:3, xlab = "Age", ylab = "Survival Probability", 
     main = "Kaplan-Meier Survival Curves by Smoking Status", xlim = c(40,90))
legend("bottomright", legend = levels(data_clean$smoking_status), 
       col = 1:3, lty = 1, cex = 0.8)

# Check proportional hazards assumption
ph_test <- cox.zph(coxph_model)
print(ph_test)

# Impute missing smoking status
imputed_data <- mice(data_clean, m = 5, maxit = 50, method = 'pmm', seed = 123)
imputed_complete_data <- as_tibble(complete(imputed_data))

cat("\nMissing values after imputation:", sum(is.na(imputed_complete_data$smoking_status)), "\n")

# Analyze imputed data
imputed_ph <- with(imputed_data, coxph(stroke.surv ~ smoking_status))
pooled_ph <- pool(imputed_ph)
summary(pooled_ph)

# Recode smoking status (combine former and never smokers)
imputed_complete_data <- imputed_complete_data %>%
  mutate(smoking_status = fct_collapse(smoking_status,
                                       not_smoking = c("never smoked", "formerly smoked"),
                                       smokes = "smokes"
  ))

# Model selection on imputed data
coxph_full <- coxph(stroke.surv ~ . - work_type, 
                    data = imputed_complete_data[, c(2, 4:11)])
aic_model <- stepAIC(coxph_full, direction = "both")
summary(aic_model)

# Final Kaplan-Meier plot for smoking status
km_fit_smoking_final <- survfit(Surv(age, stroke) ~ smoking_status, 
                                data = imputed_complete_data)
plot(km_fit_smoking_final, col = 1:2, xlab = "Age", ylab = "Survival Probability", 
     main = "Kaplan-Meier Survival Curves by Smoking Status (Imputed)", xlim = c(40,90))
legend("bottomright", legend = levels(imputed_complete_data$smoking_status), 
       col = 1:2, lty = 1, cex = 0.8)


