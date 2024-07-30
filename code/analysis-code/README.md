# analysis-code

This directory houses the R script responsible for performing comprehensive statistical analyses on the processed data. It is designed to provide flexibility, allowing modifications to align with specific research questions or hypotheses within the project.

# File Description

## Statistical Analysis File

File: statistical-analysis.qmd

This Quarto (QMD) document contains detailed code sections that execute various statistical models and tests, utilizing the cleaned and processed data. The file is structured to guide the user through each analysis step, making it easy to follow and replicate.

## Included Models and Analyses

The statistical-analysis.qmd script encompasses a broad range of statistical techniques, each tailored to extract specific insights from the data:

- Age Logistic Regression: Analyzes the impact of age on a specific outcome using logistic regression.

- Income Logistic Regression: Investigates how income influences a particular response variable.

- Chi-Squared Test: Examines the association between categorical variables.

- Multivariable Analysis: Employs multiple variables to assess their collective impact on a response variable.

- Random Forest: Utilizes this machine learning technique to predict outcomes based on numerous predictors and determine variable importance.

The statistical-analysis file also include how the results were exported into RDS files to be placed in the manuscript file.