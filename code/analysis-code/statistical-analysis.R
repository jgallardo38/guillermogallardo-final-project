###############################
# analysis script
#
#this script loads the processed, cleaned data, does a simple analysis
#and saves the results to the results folder

#load needed packages. make sure they are installed.
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving
library(tidyverse)
library(tidymodels)
library(caret) # For modeling and evaluation
library(e1071)  
library(rsample)
library(parsnip)

####################################
####################################
# Loading Data
data_location = here::here("data","processed-data","HRprocesseddata.rds")
hr_data = readRDS(data_location)
hr_data$Attrition = ifelse(hr_data$Attrition == "Yes", 1, 0)
table(hr_data$Attrition)
####################################
####################################




#Bivariate
####################################
####################################
## Age Logistic Regression
age_glm = glm(Attrition ~ Age, data = hr_data, family = binomial())
summary(age_glm)

## Age Export table
###tidy function
lgAge = broom::tidy(age_glm)
lgAge$p.value = format(lgAge$p.value, digits = 6)
#look at fit results
print(lgAge)
# save fit results table  
table_lgAge = here("results", "tables", "logistic_age.rds")
saveRDS(lgAge, file = table_lgAge)
####################################
####################################




####################################
####################################
## Income Logistic Regression
inc_glm = glm(Attrition ~ MonthlyIncome, data = hr_data, family = binomial())
summary(inc_glm)

## Income Export table
###tidy function
lgInc = broom::tidy(inc_glm)
lgInc$p.value = format(lgInc$p.value, digits = 6)
#look at fit results
print(lgInc)
# save fit results table  
table_lgInc = here("results", "tables", "logistic_income.rds")
saveRDS(lgInc, file = table_lgInc)
####################################
####################################



####################################
####################################
## Chi-squared
education_table = table(hr_data$Attrition, hr_data$EducationField)
chisq_test = chisq.test(education_table)
chisq_test

####################################
####################################




####################################
####################################
# Multivariable Analysis

glm_model = glm(Attrition ~ Age + MonthlyIncome + JobSatisfaction, data = hr_data, family = binomial())

# Summary of the model
summary(glm_model)

## Income Export table
###tidy function
glmAMJ = broom::tidy(glm_model)
glmAMJ$p.value = format(glmAMJ$p.value, digits = 6)
#look at fit results
print(glmAMJ)
# save fit results table  
table_GLM = here("results", "tables", "glm_model.rds")
saveRDS(glmAMJ, file = table_GLM)

####################################
####################################



####################################
####################################
#Random Forest
# Ensure Attrition is a factor
hr_data$Attrition = as.factor(hr_data$Attrition)
set.seed(123)  # for reproducibility

# Splitting the data into training (80%) and testing (20%) sets
library(caret)
library(randomForest)
library(ROSE)
splitIndex = createDataPartition(hr_data$Attrition, p = 0.80, list = FALSE)
train_data = hr_data[splitIndex, ]
test_data = hr_data[-splitIndex, ]

# Calculating the total desired sample size for the balanced data
# This calculation assumes doubling the number of minority class instances
num_minority <- sum(train_data$Attrition == "1")
num_majority <- sum(train_data$Attrition == "0")
desired_minority_size <- num_minority * 2  # Target double the minority size
total_desired_size <- num_majority + desired_minority_size  # Total size after balancing

# Applying ROSE to balance the training data
train_data_balanced <- ovun.sample(Attrition ~ ., data = train_data, method = "over", 
                                   N = total_desired_size)$data

# Train Random Forest on the training set
rf_model = randomForest(Attrition ~ ., data = train_data_balanced, importance = TRUE, ntree = 1000)

# Print model summary
print(rf_model)

# Checking variable importance
importance(rf_model)
plot(rf_model)
# Checking variable importance
importance_scores = importance(rf_model)

# Convert the importance scores to a data frame for easier handling
importance_df = as.data.frame(importance_scores)

# Adding row names as a new column for reference
importance_df$Variable = rownames(importance_df)

# Order the data frame by MeanDecreaseAccuracy in descending order to get the most important variables
# Assuming MeanDecreaseAccuracy is the equivalent of %IncMSE from your description
top_predictors = importance_df[order(-importance_df$MeanDecreaseAccuracy), ]

# Selecting the top 5 predictors based on MeanDecreaseAccuracy
top_5_predictors = head(top_predictors, 5)

# Print the top 5 predictors
print(top_5_predictors)

## RandomForest Predictors table
rf_top5Pred = here("results", "tables", "rd_top_predictors.rds")
saveRDS(top_5_predictors, file = rf_top5Pred)


## RandomForest Model
rf_summary = capture.output(print(rf_model))
rf_summary_path <- here("results", "tables", "rf_summary.rds")
saveRDS(rf_summary, file = rf_summary_path)
####################################
####################################



