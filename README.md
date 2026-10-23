# 🩺 Diabetes Predictive Analytics Using R

### Exploring Diabetes Data • Statistical Insights • Machine Learning

A data analytics and machine learning project that investigates diabetes-related patterns, evaluates statistical associations, and builds predictive classification models using **R and RStudio**.

The project analyzes two datasets — the PIMA Indians Diabetes Dataset and the Early Stage Diabetes Risk Prediction Dataset — to explore data quality, identify informative features, visualize patterns, and compare model performance.

---

## 📌 Project at a Glance

| Component | Details |
|---|---|
| **Domain** | Healthcare Analytics |
| **Language** | R |
| **Environment** | RStudio on Windows |
| **Datasets** | PIMA Indians Diabetes & Early Stage Diabetes |
| **Approach** | Exploratory analysis, statistical testing, supervised learning |
| **Models** | Logistic Regression, Decision Tree, Random Forest, K-Nearest Neighbors |
| **Evaluation** | Cross-validation, ROC-AUC, confusion matrices, classification metrics |

---

## 🎯 Objectives

- Clean and prepare diabetes datasets for analysis.
- Investigate distributions, relationships, class balance, and potential outliers.
- Examine statistical associations between predictors and diabetes outcomes.
- Train and compare multiple classification algorithms.
- Evaluate predictive performance using cross-validation and test-set metrics.
- Present findings through visualizations, statistical summaries, and model evaluation outputs.

---

## 📂 Datasets

### 1. PIMA Indians Diabetes Dataset

Contains **768 observations**, eight predictor variables, and a binary diabetes outcome.

The dataset is used to explore relationships between clinical measurements and diabetes status, including glucose, BMI, age, insulin, and other health-related variables.

**Source:** [PIMA Indians Diabetes Dataset — Kaggle](https://www.kaggle.com/datasets/kumargh/pimaindiansdiabetescsv)

### 2. Early Stage Diabetes Risk Prediction Dataset

The original dataset contains **520 records** and 16 predictor attributes, including demographic information and reported symptoms. Duplicate removal produced **251 unique records** for the deduplicated analysis.

Both cleaned versions are retained in the project:

| File | Description |
|---|---|
| `data/early_clean.csv` | 251 deduplicated records |
| `data/early_clean_all_rows.csv` | 520 cleaned records, retaining duplicates |

**Source:** [Early Stage Diabetes Risk Prediction Dataset — UCI Machine Learning Repository](https://archive.ics.uci.edu/dataset/529/early+stage+diabetes+risk+prediction+dataset)

The deduplicated dataset is used for the primary secondary-dataset modeling analysis to reduce the risk of duplicate records inflating evaluation results.

---

## ⚙️ Project Workflow

The analysis is organized into reproducible R scripts.

| Script | Purpose |
|---|---|
| `00_setup.R` | Project configuration and reproducibility settings |
| `01_data_cleaning.R` | Data cleaning, preprocessing, and cleaning outputs |
| `02_eda.R` | Exploratory data analysis and visualizations |
| `03_statistical_analysis.R` | Statistical tests and association analysis |
| `04_model_development.R` | Model training, validation, and comparison |
| `run_all.R` | Runs the complete project workflow |

### Analysis Pipeline

**Raw Data → Cleaning & Preprocessing → Exploratory Data Analysis → Statistical Analysis → Model Development → Evaluation & Comparison**

---

## 📊 Exploratory Data Analysis

The project includes visualizations designed to investigate:

- Class distribution and potential imbalance.
- Predictor distributions and possible outliers.
- Correlations and relationships among clinical measurements.
- Glucose and BMI patterns across diabetes outcomes.
- Age and gender patterns in the Early Stage dataset.
- Prevalence of reported symptoms and symptom counts.

Figures are available in `outputs/figures/`.

---

## 🔬 Statistical Analysis

Statistical testing is used to investigate differences and associations between predictors and diabetes outcomes.

The project includes:

- Group comparisons for numerical variables.
- Tests of association for categorical predictors.
- Effect-size analysis to help interpret the magnitude of observed relationships.
- Multiple-testing correction using the Holm procedure.

Results are saved in `outputs/tables/statistical_tests.csv`, alongside other analysis outputs.

---

## 🤖 Machine Learning Models

Four classification algorithms are implemented and compared:

| Model | Purpose |
|---|---|
| **Logistic Regression** | Baseline statistical classification model |
| **Decision Tree** | Interpretable, rule-based classification |
| **Random Forest** | Ensemble learning using multiple decision trees |
| **K-Nearest Neighbors (KNN)** | Distance-based classification |

### Evaluation Strategy

The project uses an 80/20 train-test split and repeated 10-fold cross-validation with three repeats. Model selection is based on cross-validation ROC-AUC.

Evaluation outputs include:

- ROC-AUC and confidence intervals for test-set performance.
- Accuracy, precision, recall, specificity, and F1-score.
- Confusion matrices.
- Variable-importance and ROC curve visualizations.

The selected models and evaluation outputs are saved in `outputs/models/` and `outputs/tables/`.

---

## 🛠️ Technology Stack

- **R 4.6.1**
- **RStudio**
- **tidyverse** and **dplyr** — data manipulation
- **ggplot2** — data visualization
- **caret** — model training and cross-validation
- **pROC** — ROC analysis and AUC evaluation
- **randomForest** — Random Forest modeling
- **rpart** and **rpart.plot** — decision trees and visualization
- **e1071** — supporting machine learning functionality
- **knitr** — reporting support

---

## 🚀 Getting Started

### Prerequisites

Install R and RStudio, then open the project directory in RStudio.

### Run the Analysis

1. Clone or download this repository.
2. Open the project folder in RStudio.
3. Check the data paths and configuration in `00_setup.R`.
4. Ensure the required R packages are installed.
5. Run `run_all.R` to execute the complete workflow.

If you prefer to run the scripts individually, follow the numbered sequence from `00_setup.R` through `04_model_development.R`.

---

## 📁 Repository Structure

```text
diabetes-predictive-analytics-r/
├── 00_setup.R
├── 01_data_cleaning.R
├── 02_eda.R
├── 03_statistical_analysis.R
├── 04_model_development.R
├── run_all.R
├── README.md
├── .gitignore
├── data/
│   ├── pima_diabetes.csv
│   ├── pima_clean.csv
│   ├── pima_model_input.csv
│   ├── early_stage_diabetes.csv
│   ├── early_clean.csv
│   └── early_clean_all_rows.csv
└── outputs/
    ├── figures/
    ├── models/
    └── tables/
```

---

## 📈 Outputs

The repository contains the generated analysis artifacts:

- **16 figures** covering exploratory analysis and model evaluation.
- **7 CSV tables** containing cleaning, statistical, and model-evaluation results.
- **2 saved model objects** for the selected PIMA and deduplicated Early Stage analyses.

These outputs support review of the analytical workflow and its reported findings.

---

## ⚠️ Limitations and Responsible Use

- The PIMA dataset has a relatively small sample size and a specific population context.
- The Early Stage dataset is also limited in size; deduplication reduces it from 520 records to 251 unique records.
- Model performance can vary with data splits, preprocessing, and dataset characteristics.
- Performance on these datasets does not establish generalizability to other populations or clinical settings.
- This project is for educational and analytical purposes. **Its predictions are not a substitute for professional medical advice or clinical diagnosis.**

---

## 🔮 Future Enhancements

Potential extensions include:

- External validation using additional diabetes datasets.
- More systematic hyperparameter tuning.
- Explainability techniques such as SHAP, where appropriate.
- Stronger validation of preprocessing within each cross-validation fold.
- Model calibration and decision-threshold analysis.
- A simple interactive dashboard for exploring model predictions and results.

---

## 👨‍💻 Author

**Mohit Thakur**

M.Tech — Data Science and Analytics

*Predictive Analytics Academic Project*

---

### 📚 Dataset References

1. [PIMA Indians Diabetes Dataset — Kaggle](https://www.kaggle.com/datasets/kumargh/pimaindiansdiabetescsv)
2. [Early Stage Diabetes Risk Prediction Dataset — UCI Machine Learning Repository](https://archive.ics.uci.edu/dataset/529/early+stage+diabetes+risk+prediction+dataset)
