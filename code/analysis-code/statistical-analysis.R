###############################
# analysis script
#
#this script loads the processed, cleaned data, does a simple analysis
#and saves the results to the results folder

#load needed packages. make sure they are installed.
library(ggplot2) #for plotting
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving
library(tidyverse)
library(tidymodels)
library(caret) # For modeling and evaluation
library(e1071)  
library(rsample)
library(parsnip)

####################################
# Loading Data
data_location = here::here("data","processed-data","HRprocesseddata.rds")
hr_data = readRDS(data_location)

####################################


#Bivariate

#####################################
## Age Logistic Regression
age_glm = glm(Attrition ~ Age, data = hr_data, family = binomial())
summary(age_glm)

## Age Export table
###tidy function
lgAge = broom::tidy(age_glm)
lgAge$p.value <- format(lgAge$p.value, digits = 6)
#look at fit results
print(lgAge)
# save fit results table  
table_lgAge = here("results", "tables", "logistic_age.rds")
saveRDS(lgAge, file = table_lgAge)
####################################


####################################
## Income Logistic Regression
inc_glm = glm(Attrition ~ MonthlyIncome, data = hr_data, family = binomial())
summary(inc_glm)

## Income Export table
###tidy function
lgInc = broom::tidy(inc_glm)
lgInc$p.value <- format(lgInc$p.value, digits = 6)
#look at fit results
print(lgInc)
# save fit results table  
table_lgInc = here("results", "tables", "logistic_income.rds")
saveRDS(lgInc, file = table_lgInc)
####################################


####################################
## Chi-squared
education_table <- table(hr_data$Attrition, hr_data$EducationField)
chisq_test <- chisq.test(education_table)
chisq_test

####################################


####################################
# Multivariable Analysis

hr_data$Attrition = ifelse(hr_data$Attrition == "Yes", 1, 0)
glm_model <- glm(Attrition ~ Age + MonthlyIncome + JobSatisfaction, data = hr_data, family = binomial())

# Summary of the model
summary(glm_model)

## Income Export table
###tidy function
glmAMJ = broom::tidy(glm_model)
glmAMJ$p.value <- format(glmAMJ$p.value, digits = 6)
#look at fit results
print(glmAMJ)
# save fit results table  
table_GLM = here("results", "tables", "glm_model.rds")
saveRDS(glmAMJ, file = table_GLM)

####################################

















################
##IN PROGRESS FOR PART 4

set.seed(42)

hr_data$Attrition <- factor(hr_data$Attrition, levels = c("No", "Yes"))
table(hr_data$Attrition)

# Ensure all categorical variables are factors and check initial levels
categorical_cols <- sapply(hr_data, is.character)
hr_data[categorical_cols] <- lapply(hr_data[categorical_cols], factor)

# Splitting data with stratification
split <- createDataPartition(y = hr_data$Attrition, p = 0.7, list = FALSE, times = 1)
training_set <- hr_data[split, ]
testing_set <- hr_data[-split, ]


# Check and adjust factor levels in training set
training_set <- training_set %>%
  mutate_if(is.factor, function(x) {
    if (length(levels(x)) < 2) {
      x <- NULL  # Drop the variable if only one level
    } else {
      x
    }
  })


# Pre-processing data
preproc <- preProcess(training_set, method = c("center", "scale"))
train_processed <- predict(preproc, training_set)
test_processed <- predict(preproc, testing_set)


########## Random Forest Model
fit_rf <- train(Attrition ~ ., data = train_processed, method = "rf", 
                trControl = trainControl(method = "cv", number = 10))

# Evaluating the Random Forest Model
rf_predictions <- predict(fit_rf, newdata = test_processed)
rf_conf_matrix <- confusionMatrix(rf_predictions, testing_set$Attrition)
print(rf_conf_matrix)


importance <- varImp(fit_rf, scale = FALSE)
print(importance)

hr_matrix = here("results", "tables", "hr_matrix.rds")
saveRDS(rf_conf_matrix, file = hr_matrix)

var_importance = here("results", "tables", "importance.rds")
saveRDS(importance, file = var_importance)
##########################################
