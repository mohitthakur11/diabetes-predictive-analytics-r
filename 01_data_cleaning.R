# 01_data_cleaning.R

if (!file.exists("00_setup.R")) {
  stop("Set the working directory to the folder containing 00_setup.R.")
}

source("00_setup.R")
load_packages("tidyverse")

REMOVE_DUPLICATES <- TRUE
CAP_OUTLIERS_FOR_EDA <- TRUE

PIMA_FILE <- "data/pima_diabetes.csv"
EARLY_FILE <- "data/early_stage_diabetes.csv"

cleaning_log <- list()

log_step <- function(dataset, step, detail) {
  cleaning_log[[length(cleaning_log) + 1]] <<- data.frame(
    Dataset = dataset,
    Step = step,
    Detail = detail
  )
  cat(sprintf("[%s] %-28s %s\n", dataset, step, detail))
}

require_file(PIMA_FILE, "Place the raw PIMA CSV in the data folder.")
require_file(EARLY_FILE, "Place the raw early-stage CSV in the data folder.")

pima_raw <- read.csv(PIMA_FILE, stringsAsFactors = FALSE)
early_raw <- read.csv(EARLY_FILE, stringsAsFactors = FALSE)

pima_cols <- c(
  "Pregnancies", "Glucose", "BloodPressure", "SkinThickness", "Insulin",
  "BMI", "DiabetesPedigreeFunction", "Age", "Outcome"
)

if (!all(pima_cols %in% names(pima_raw))) {
  stop(
    "PIMA file is missing column(s): ",
    paste(setdiff(pima_cols, names(pima_raw)), collapse = ", "),
    "\nFound: ", paste(names(pima_raw), collapse = ", "),
    call. = FALSE
  )
}

if (!all(c("Age", "Gender", "class") %in% names(early_raw))) {
  stop(
    "Early-stage file must contain Age, Gender and class columns. Found: ",
    paste(names(early_raw), collapse = ", "),
    call. = FALSE
  )
}

numeric_pima <- setdiff(pima_cols, "Outcome")
for (v in numeric_pima) {
  if (!is.numeric(pima_raw[[v]])) {
    stop("PIMA column '", v, "' must be numeric.", call. = FALSE)
  }
}

if (!all(pima_raw$Outcome %in% c(0, 1))) {
  stop("PIMA Outcome must contain only 0 and 1.", call. = FALSE)
}

log_step("PIMA", "Raw data", sprintf("%d rows x %d columns", nrow(pima_raw), ncol(pima_raw)))
log_step("EARLY", "Raw data", sprintf("%d rows x %d columns", nrow(early_raw), ncol(early_raw)))

# PIMA model input: retain missing values for fold-specific imputation later.
pima_model <- pima_raw
zero_cols <- c("Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI")
zero_counts <- colSums(pima_model[zero_cols] == 0, na.rm = TRUE)

pima_model[zero_cols] <- lapply(
  pima_model[zero_cols],
  function(x) replace(x, which(x == 0), NA_real_)
)
pima_model$Outcome <- factor(pima_model$Outcome, levels = c(0, 1), labels = c("No", "Yes"))

log_step(
  "PIMA", "Zero values -> NA",
  paste(sprintf("%s=%d", names(zero_counts), zero_counts), collapse = ", ")
)
write.csv(pima_model, "data/pima_model_input.csv", row.names = FALSE, na = "NA")

# Descriptive-analysis copy: imputation and outlier capping are only used for EDA tables/plots.
pima_eda <- pima_model
cap_cols <- c(
  "Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI",
  "DiabetesPedigreeFunction"
)

if (CAP_OUTLIERS_FOR_EDA) {
  n_capped <- integer(0)

  for (v in cap_cols) {
    observed <- pima_eda[[v]][!is.na(pima_eda[[v]])]
    q <- quantile(observed, c(0.25, 0.75), names = FALSE)
    iqr <- q[2] - q[1]
    lo <- q[1] - 1.5 * iqr
    hi <- q[2] + 1.5 * iqr

    n_capped[v] <- sum(pima_eda[[v]] < lo | pima_eda[[v]] > hi, na.rm = TRUE)
    pima_eda[[v]] <- pmin(pmax(pima_eda[[v]], lo), hi)
  }

  log_step(
    "PIMA", "EDA outlier capping",
    paste(sprintf("%s=%d", names(n_capped), n_capped), collapse = ", ")
  )
}

miss_before <- colSums(is.na(pima_eda))

for (v in zero_cols) {
  med <- median(pima_eda[[v]], na.rm = TRUE)
  pima_eda[[v]][is.na(pima_eda[[v]])] <- med
}

log_step(
  "PIMA", "EDA median imputation",
  paste(
    sprintf("%s=%d", names(miss_before[miss_before > 0]), miss_before[miss_before > 0]),
    collapse = ", "
  )
)

if (anyNA(pima_eda)) {
  stop("PIMA descriptive dataset still contains missing values.", call. = FALSE)
}

write.csv(pima_eda, "data/pima_clean.csv", row.names = FALSE)

# Standardize the early-stage dataset.
early <- early_raw %>%
  mutate(across(where(is.character), ~ stringr::str_to_title(stringr::str_trim(.x))))

sym_cols <- setdiff(names(early), c("Age", "Gender", "class"))

bad <- sym_cols[
  !vapply(early[sym_cols], function(x) all(x %in% c("Yes", "No")), logical(1))
]

if (length(bad)) {
  stop(
    "Symptom column(s) with values other than Yes/No: ",
    paste(bad, collapse = ", "),
    call. = FALSE
  )
}

if (!all(early$Gender %in% c("Male", "Female"))) {
  stop("Gender must be Male/Female.", call. = FALSE)
}

if (!all(early$class %in% c("Positive", "Negative"))) {
  stop("class must be Positive/Negative.", call. = FALSE)
}

n_na <- sum(is.na(early))
if (n_na > 0) early <- drop_na(early)

log_step(
  "EARLY", "Missing values",
  sprintf("%d found%s", n_na, if (n_na > 0) " (rows dropped)" else "")
)

if (!nrow(early)) stop("No early-stage records remain after removing missing values.", call. = FALSE)

# Keep an all-record version for a sensitivity analysis.
early_all <- early
early_all[sym_cols] <- lapply(early_all[sym_cols], factor, levels = c("No", "Yes"))
early_all$Gender <- factor(early_all$Gender, levels = c("Female", "Male"))
early_all$class <- factor(early_all$class, levels = c("Negative", "Positive"))
write.csv(early_all, "data/early_clean_all_rows.csv", row.names = FALSE)

n_dup <- sum(duplicated(early))
if (REMOVE_DUPLICATES) early <- distinct(early)

log_step(
  "EARLY", "Duplicate rows",
  sprintf("%d found, %s -> %d rows remain", n_dup,
          if (REMOVE_DUPLICATES) "removed" else "kept", nrow(early))
)

if (!nrow(early)) stop("No early-stage records remain after duplicate handling.", call. = FALSE)

log_step("EARLY", "Age range", sprintf("%d - %d years", min(early$Age), max(early$Age)))

early[sym_cols] <- lapply(early[sym_cols], factor, levels = c("No", "Yes"))
early$Gender <- factor(early$Gender, levels = c("Female", "Male"))
early$class <- factor(early$class, levels = c("Negative", "Positive"))

log_step(
  "EARLY", "Class balance",
  paste(sprintf("%s=%d", names(table(early$class)), table(early$class)), collapse = ", ")
)

write.csv(early, "data/early_clean.csv", row.names = FALSE)
save_table(do.call(rbind, cleaning_log), "cleaning_log.csv")

cat(
  "\nCleaning complete.\n",
  "  data/pima_clean.csv        : ", nrow(pima_eda), " rows (EDA)\n",
  "  data/pima_model_input.csv  : ", nrow(pima_model), " rows (model input)\n",
  "  data/early_clean.csv       : ", nrow(early), " rows (deduplicated)\n",
  "  data/early_clean_all_rows.csv: ", nrow(early_all), " rows (sensitivity analysis)\n",
  sep = ""
)
