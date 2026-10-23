# 00_setup.R

SEED <- 42
OUTCOME_COLORS <- c("#2a9d8f", "#e76f51")

load_packages <- function(required, optional = character()) {
  missing <- setdiff(c(required, optional), rownames(installed.packages()))

  if (length(missing)) {
    message("Packages not installed: ", paste(missing, collapse = ", "))
    message("Install them with install.packages() before running the pipeline.")
  }

  missing_required <- setdiff(required, rownames(installed.packages()))
  if (length(missing_required)) {
    stop(
      "Required package(s) missing: ", paste(missing_required, collapse = ", "),
      call. = FALSE
    )
  }

  for (p in required) {
    suppressPackageStartupMessages(library(p, character.only = TRUE))
  }

  invisible(TRUE)
}

for (d in c("data", "outputs/figures", "outputs/tables", "outputs/models")) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

require_file <- function(path, hint = "") {
  if (!file.exists(path)) {
    stop(
      "File not found: ", path,
      if (nzchar(hint)) paste0("\n", hint) else "",
      "\nCurrent working directory: ", getwd(),
      call. = FALSE
    )
  }
  invisible(path)
}

save_table <- function(df, filename) {
  path <- file.path("outputs/tables", filename)
  write.csv(df, path, row.names = FALSE)
  cat("Saved table:", path, "\n")
  invisible(path)
}

save_plot <- function(plot, filename, width = 7, height = 5) {
  path <- file.path("outputs/figures", filename)
  ggplot2::ggsave(path, plot = plot, width = width, height = height, dpi = 300)
  cat("Saved figure:", path, "\n")
  invisible(path)
}

save_base_plot <- function(filename, expr, width = 900, height = 900, res = 120) {
  path <- file.path("outputs/figures", filename)
  grDevices::png(path, width = width, height = height, res = res)
  on.exit(grDevices::dev.off(), add = TRUE)
  force(expr)
  cat("Saved figure:", path, "\n")
  invisible(path)
}

to_yes_no <- function(x, col) {
  x <- trimws(as.character(x))
  out <- ifelse(
    x %in% c("Yes", "1"), "Yes",
    ifelse(x %in% c("No", "0"), "No", NA_character_)
  )

  if (anyNA(out)) {
    stop(
      "Column '", col, "' contains unexpected values: ",
      paste(unique(x[is.na(out)]), collapse = ", "),
      call. = FALSE
    )
  }

  factor(out, levels = c("No", "Yes"))
}

load_pima_clean <- function(path = "data/pima_clean.csv") {
  require_file(path, "Run 01_data_cleaning.R first.")
  df <- read.csv(path, stringsAsFactors = FALSE)
  df$Outcome <- factor(df$Outcome, levels = c("No", "Yes"))

  if (anyNA(df$Outcome)) {
    stop("PIMA Outcome has unexpected values. Re-run 01_data_cleaning.R.", call. = FALSE)
  }

  df
}

load_pima_model_input <- function(path = "data/pima_model_input.csv") {
  require_file(path, "Run 01_data_cleaning.R first.")
  df <- read.csv(path, stringsAsFactors = FALSE, na.strings = c("NA", ""))
  df$Outcome <- factor(df$Outcome, levels = c("No", "Yes"))

  if (anyNA(df$Outcome)) {
    stop("PIMA Outcome has unexpected values. Re-run 01_data_cleaning.R.", call. = FALSE)
  }

  df
}

load_early_clean <- function(path = "data/early_clean.csv") {
  require_file(path, "Run 01_data_cleaning.R first.")
  df <- read.csv(path, stringsAsFactors = FALSE)
  sym <- setdiff(names(df), c("Age", "Gender", "class"))

  if (!length(sym)) {
    stop("No symptom columns found in early_clean.csv.", call. = FALSE)
  }

  for (v in sym) df[[v]] <- to_yes_no(df[[v]], v)
  df$Gender <- factor(df$Gender, levels = c("Female", "Male"))
  df$class <- factor(df$class, levels = c("Negative", "Positive"))

  if (anyNA(df$Gender) || anyNA(df$class)) {
    stop("Gender or class contains unexpected values. Re-run 01_data_cleaning.R.", call. = FALSE)
  }

  df
}

early_symptom_cols <- function(df) {
  setdiff(names(df), c("Age", "Gender", "class"))
}
