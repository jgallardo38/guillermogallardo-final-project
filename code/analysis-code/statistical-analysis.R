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

update.packages(ask = FALSE)

#path to data
#note the use of the here() package and not absolute paths
data_location = here::here("data","processed-data","HRprocesseddata.rds")

#load data. 
mydata = readRDS(data_location)


######################################
#Data fitting/statistical analysis
######################################

#### Linear Model Job Satisfaction ~ Department

#lmfitHR = lm(JobSatisfaction~Age, data = mydata)

#summary(lmfitHR)

glm_spec <- logistic_reg() %>%
  set_engine("glm")

# Split the data
set.seed(123)
data_split <- initial_split(mydata, prop = 0.75, strata = Attrition)
train_data <- training(data_split)
test_data <- testing(data_split)

# Fit the model
glm_fit <- glm_spec %>%
  fit(Attrition ~ Age + DistanceFromHome + Education + EnvironmentSatisfaction + JobInvolvement + JobLevel + JobRole + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager + TenureCategory, data = train_data)

# Model summary
summary(glm_fit$fit)

# Predict and evaluate the model
glm_preds <- predict(glm_fit, test_data, type = "prob")
glm_roc <- roc_auc(truth = test_data$Attrition, .pred_Yes = glm_preds$.pred_Yes)

# Save the results



############################
#### First model fit
# fit linear model using height as outcome, weight as predictor

lmfit1 <- lm(Height ~ Weight, mydata)  

# place results from fit into a data frame with the tidy function
lmtable1 <- broom::tidy(lmfit1)

#look at fit results
print(lmtable1)

# save fit results table  
table_file1 = here("results", "tables", "resulttable1.rds")
saveRDS(lmtable1, file = table_file1)

############################
#### Second model fit
# fit linear model using height as outcome, weight and gender as predictor

lmfit2 <- lm(Height ~ Weight + Gender, mydata)  

# place results from fit into a data frame with the tidy function
lmtable2 <- broom::tidy(lmfit2)

#look at fit results
print(lmtable2)

# save fit results table  
table_file2 = here("results", "tables", "resulttable2.rds")
saveRDS(lmtable2, file = table_file2)

  