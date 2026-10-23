# ============================================================
# Script 02: Exploratory Data Analysis & Visualization
# Run AFTER 01_data_cleaning.R
# Outputs: outputs/figures/*.png, outputs/tables/*.csv
# ============================================================
if (!file.exists("00_setup.R")) {
  stop("Set the working directory to the project folder (the one containing 00_setup.R).")
}
source("00_setup.R")
load_packages(c("tidyverse", "corrplot", "scales"))
theme_set(theme_minimal(base_size = 12))

pima  <- load_pima_clean()     # typed loaders: see 00_setup.R
early <- load_early_clean()

pima_predictors <- setdiff(names(pima), "Outcome")
sym_cols        <- early_symptom_cols(early)
stopifnot("No symptom columns detected in early-stage data" = length(sym_cols) > 0)

cat("PIMA  :", nrow(pima),  "rows |", length(pima_predictors), "predictors\n")
cat("EARLY :", nrow(early), "rows |", length(sym_cols), "symptoms + Age + Gender\n\n")

# ============================================================
# PIMA
# ============================================================
## 1. Target balance
p <- pima %>%
  count(Outcome) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(Outcome, n, fill = Outcome)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%d (%s)", n, percent(pct, accuracy = 0.1))), vjust = -0.4) +
  scale_fill_manual(values = OUTCOME_COLORS) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "PIMA: Class Distribution", x = "Diabetes outcome", y = "Count") +
  theme(legend.position = "none")
save_plot(p, "pima_class_balance.png", 6, 4)

## 2. Histograms of all predictors by outcome
pima_long <- pima %>%
  pivot_longer(all_of(pima_predictors), names_to = "variable", values_to = "value")

p <- ggplot(pima_long, aes(value, fill = Outcome)) +
  geom_histogram(bins = 25, alpha = 0.7, position = "identity") +
  facet_wrap(~ variable, scales = "free", ncol = 4) +
  scale_fill_manual(values = OUTCOME_COLORS) +
  labs(title = "PIMA: Feature Distributions by Outcome", x = NULL, y = "Count")
save_plot(p, "pima_histograms.png", 13, 7)

## 3. Boxplots by outcome
p <- ggplot(pima_long, aes(Outcome, value, fill = Outcome)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.4) +
  facet_wrap(~ variable, scales = "free_y", ncol = 4) +
  scale_fill_manual(values = OUTCOME_COLORS) +
  labs(title = "PIMA: Boxplots by Outcome", x = NULL, y = NULL) +
  theme(legend.position = "none")
save_plot(p, "pima_boxplots.png", 13, 7)

## 4. Correlation heatmap (Outcome coded 0/1)
corr_matrix <- cor(mutate(pima, Outcome = as.integer(Outcome) - 1L))
save_base_plot("pima_corrplot.png", {
  corrplot(corr_matrix, method = "color", type = "upper", addCoef.col = "black",
           number.cex = 0.7, tl.cex = 0.9, tl.col = "black",
           title = "PIMA: Correlation Matrix", mar = c(0, 0, 2, 0))
})

## 5. Descriptive statistics by outcome (tidy table)
summary_stats <- pima_long %>%
  group_by(variable, Outcome) %>%
  summarise(n = n(), mean = mean(value), sd = sd(value), median = median(value),
            min = min(value), max = max(value), .groups = "drop") %>%
  mutate(across(where(is.double), ~ round(.x, 3)))
save_table(summary_stats, "pima_summary_stats.csv")

## 6. Glucose vs BMI
p <- ggplot(pima, aes(Glucose, BMI, color = Outcome)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = OUTCOME_COLORS) +
  labs(title = "PIMA: Glucose vs BMI by Outcome")
save_plot(p, "pima_glucose_bmi.png", 7, 5)

# ============================================================
# EARLY-STAGE
# ============================================================
pos <- levels(early$class)[2]

## 7. Class balance
p <- early %>%
  count(class) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(class, n, fill = class)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%d (%s)", n, percent(pct, accuracy = 0.1))), vjust = -0.4) +
  scale_fill_manual(values = OUTCOME_COLORS) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Early-Stage: Class Distribution", x = "Class", y = "Count") +
  theme(legend.position = "none")
save_plot(p, "early_class_balance.png", 6, 4)

## 8. Symptom prevalence by class
prev <- early %>%
  select(class, all_of(sym_cols)) %>%
  pivot_longer(-class, names_to = "symptom", values_to = "response") %>%
  group_by(class, symptom) %>%
  summarise(prevalence = mean(response == "Yes"), .groups = "drop") %>%
  mutate(symptom = gsub("\\.", " ", symptom))

symptom_order <- prev %>% filter(class == pos) %>% arrange(prevalence) %>% pull(symptom)
prev$symptom  <- factor(prev$symptom, levels = symptom_order)

p <- ggplot(prev, aes(symptom, prevalence, fill = class)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = OUTCOME_COLORS) +
  labs(title = "Early-Stage: Symptom Prevalence by Class", x = NULL, y = "Proportion of patients")
save_plot(p, "early_symptom_prevalence.png", 9, 7)
save_table(prev %>% mutate(prevalence = round(prevalence, 3)) %>% arrange(class, desc(prevalence)),
           "early_symptom_prevalence.csv")

## 9. Number of symptoms per patient
p <- early %>%
  mutate(n_symptoms = rowSums(across(all_of(sym_cols), ~ .x == "Yes"))) %>%
  ggplot(aes(class, n_symptoms, fill = class)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = OUTCOME_COLORS) +
  labs(title = "Early-Stage: Number of Symptoms Reported", x = NULL, y = "Symptoms (count)") +
  theme(legend.position = "none")
save_plot(p, "early_symptom_count.png", 6, 4)

## 10. Gender vs class
p <- ggplot(early, aes(Gender, fill = class)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = OUTCOME_COLORS) +
  labs(title = "Early-Stage: Class Proportion by Gender", y = "Proportion")
save_plot(p, "early_gender_class.png", 6, 4)

## 11. Age by class
p <- ggplot(early, aes(Age, fill = class)) +
  geom_histogram(bins = 20, position = "identity", alpha = 0.7) +
  scale_fill_manual(values = OUTCOME_COLORS) +
  labs(title = "Early-Stage: Age Distribution by Class", y = "Count")
save_plot(p, "early_age_class.png", 7, 4)

cat("\nEDA complete. Figures: outputs/figures/ | Tables: outputs/tables/\n")
