\# Diabetes Predictive Analytics Using R



\## Project Overview



This project applies predictive analytics and statistical analysis to diabetes datasets using R. It explores data quality, visual patterns, statistical associations, and the performance of machine learning classification models.



\## Datasets



\### PIMA Indians Diabetes Dataset

The PIMA dataset contains 768 observations and 8 predictor variables, along with a binary diabetes outcome.



Source: https://www.kaggle.com/datasets/kumargh/pimaindiansdiabetescsv



\### Early Stage Diabetes Risk Prediction Dataset

The secondary dataset contains 520 original records. Duplicate removal produces a dataset of 251 unique records.



\- `early\_clean.csv` — 251 deduplicated rows.

\- `early\_clean\_all\_rows.csv` — 520 rows, retaining all original records after cleaning.



Source: https://archive.ics.uci.edu/dataset/529/early+stage+diabetes+risk+prediction+dataset



\## Project Workflow



1\. Data cleaning and preprocessing.

2\. Exploratory data analysis.

3\. Statistical testing and analysis.

4\. Classification model development and evaluation.

5\. Comparison of model performance.



\## Technologies



\- R and RStudio

\- tidyverse

\- caret

\- ggplot2

\- pROC

\- randomForest

\- rpart and rpart.plot

\- e1071



\## Reproducibility



The project includes R scripts for running the analysis. Review `00\_setup.R` and `run\_all.R` for configuration and execution instructions before running the workflow.



\## Important Note



The Early Stage Diabetes dataset is a secondary dataset and should be interpreted with its limited sample size and duplicate structure in mind. Predictive results are not a substitute for clinical diagnosis.



