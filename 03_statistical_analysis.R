# ============================================================
# Script 03: Statistical Analysis (Hypothesis Testing)
# Run AFTER 01_data_cleaning.R
# Output: outputs/tables/statistical_tests.csv (uses raw/NA-preserving PIMA input)
# ============================================================
if (!file.exists("00_setup.R")) {
  stop("Set the working directory to the project folder (the one containing 00_setup.R).")
}
source("00_setup.R")
load_packages("tidyverse")
options(width = 140)

# Use the analysis/model-input version: invalid zero measurements remain NA.
# Do not use the EDA copy here because its values are median-imputed and
# outlier-capped, which can alter hypothesis-test results.
pima  <- load_pima_model_input()
early <- load_early_clean()

ALPHA <- 0.05

# ---- Effect-size helpers ---------------------------------------------
cohens_d <- function(x, g) {                       # pooled-SD Cohen's d
  g  <- droplevels(g)
  m  <- tapply(x, g, mean); s <- tapply(x, g, sd); n <- tapply(x, g, length)
  sp <- sqrt(((n[1] - 1) * s[1]^2 + (n[2] - 1) * s[2]^2) / (n[1] + n[2] - 2))
  unname(abs(m[1] - m[2]) / sp)
}

# Compare one numeric variable between the two classes with BOTH a Welch
# t-test and a Wilcoxon rank-sum test (robust: several variables such as
# Insulin / DiabetesPedigreeFunction / Age are clearly right-skewed).
compare_numeric <- function(x, g, dataset, variable) {
  # Pairwise complete cases only; do not impute for inferential tests.
  keep <- complete.cases(x, g)
  x <- x[keep]
  g <- droplevels(g[keep])
  if (nlevels(g) != 2L || length(x) < 4L) {
    stop("Not enough complete observations for ", dataset, " / ", variable)
  }
  tt <- t.test(x ~ g)                                   # Welch (unequal variances)
  wt <- suppressWarnings(wilcox.test(x ~ g))
  n  <- as.numeric(table(g))
  bind_rows(
    tibble(Dataset = dataset, Variable = variable, Test = "Welch t-test",
           Statistic = unname(tt$statistic), P_value = tt$p.value,
           Effect_size = cohens_d(x, g), Effect_type = "Cohen's d"),
    tibble(Dataset = dataset, Variable = variable, Test = "Wilcoxon rank-sum",
           Statistic = unname(wt$statistic), P_value = wt$p.value,
           Effect_size = abs(2 * unname(wt$statistic) / prod(n) - 1),
           Effect_type = "rank-biserial r")
  )
}

# Association between one categorical variable and the class.
# Chi-square when all expected counts >= 5, otherwise Fisher's exact test.
compare_categorical <- function(x, g, dataset, variable) {
  tab      <- table(x, g)
  expected <- suppressWarnings(chisq.test(tab)$expected)
  phi      <- sqrt(unname(suppressWarnings(chisq.test(tab, correct = FALSE)$statistic)) / sum(tab))
  if (any(expected < 5)) {
    ft <- fisher.test(tab)
    tibble(Dataset = dataset, Variable = variable, Test = "Fisher exact",
           Statistic = NA_real_, P_value = ft$p.value, Effect_size = phi, Effect_type = "phi")
  } else {
    ct <- chisq.test(tab)
    tibble(Dataset = dataset, Variable = variable, Test = "Chi-square",
           Statistic = unname(ct$statistic), P_value = ct$p.value,
           Effect_size = phi, Effect_type = "phi")
  }
}

# ---------- PIMA: numeric predictors vs Outcome -----------------------
pima_vars  <- setdiff(names(pima), "Outcome")
pima_tests <- map_dfr(pima_vars, ~ compare_numeric(pima[[.x]], pima$Outcome, "PIMA", .x))

# ---------- EARLY-STAGE: symptoms + Gender vs class (chi-square) -------
sym_cols    <- early_symptom_cols(early)
early_cat   <- map_dfr(c(sym_cols, "Gender"),
                       ~ compare_categorical(early[[.x]], early$class, "EARLY", .x))
early_age   <- compare_numeric(early$Age, early$class, "EARLY", "Age")

# ---------- Combine + multiple-testing correction ----------------------
# ~50 tests are run, so some "significant" results are expected by chance alone.
# Holm-adjusted p-values (applied within each type of test) control this.
results <- bind_rows(pima_tests, early_cat, early_age) %>%
  group_by(Dataset, Test) %>%
  mutate(P_adj_Holm = p.adjust(P_value, method = "holm")) %>%
  ungroup() %>%
  mutate(Interpretation = case_when(
    P_adj_Holm < ALPHA ~ "Significant (Holm-adjusted)",
    P_value    < ALPHA ~ "Significant only before adjustment",
    TRUE               ~ "Not significant")) %>%
  mutate(across(c(Statistic, Effect_size), ~ round(.x, 3)),
         P_value = signif(P_value, 4), P_adj_Holm = signif(P_adj_Holm, 4)) %>%
  arrange(Dataset, Test, P_value)

save_table(results, "statistical_tests.csv")

# ---------- Console summary -------------------------------------------
cat("\n=== PIMA: Welch t-tests (sorted by p-value) ===\n")
print(as.data.frame(filter(results, Dataset == "PIMA", Test == "Welch t-test") %>%
                      select(Variable, Statistic, P_value, P_adj_Holm, Effect_size, Interpretation)),
      row.names = FALSE)

cat("\n=== EARLY-STAGE: association with class (sorted by p-value) ===\n")
print(as.data.frame(filter(results, Dataset == "EARLY", Test != "Wilcoxon rank-sum") %>%
                      select(Variable, Test, Statistic, P_value, P_adj_Holm, Effect_size, Interpretation)),
      row.names = FALSE)

cat("\n----- KEY TAKEAWAYS (for the report) -----\n")
sig <- filter(results, Interpretation == "Significant (Holm-adjusted)")
cat(sprintf("Significant after Holm adjustment: %d of %d tests\n", nrow(sig), nrow(results)))
cat("Strongest PIMA effects (Cohen's d):\n")
print(as.data.frame(pima_tests %>% filter(Test == "Welch t-test") %>% arrange(desc(Effect_size)) %>%
                      transmute(Variable, Cohens_d = round(Effect_size, 2)) %>% head(3)), row.names = FALSE)
cat("Strongest EARLY-STAGE associations (phi):\n")
print(as.data.frame(early_cat %>% arrange(desc(Effect_size)) %>%
                      transmute(Variable, phi = round(Effect_size, 2)) %>% head(3)), row.names = FALSE)

cat("\nStatistical analysis complete. Saved to outputs/tables/statistical_tests.csv\n")
