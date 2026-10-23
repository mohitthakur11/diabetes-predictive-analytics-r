# ============================================================
# run_all.R -- runs the whole pipeline in order.
# Set the working directory to the project folder first, then:
#   source("run_all.R")
# ============================================================
if (!file.exists("00_setup.R")) {
  stop("Set the working directory to the project folder (the one containing 00_setup.R).")
}
scripts <- c("01_data_cleaning.R", "02_eda.R", "03_statistical_analysis.R", "04_model_development.R")
for (s in scripts) {
  cat("\n", strrep("#", 70), "\n#  ", s, "\n", strrep("#", 70), "\n", sep = "")
  source(s)
}
cat("\nPipeline finished. See outputs/figures, outputs/tables and outputs/models.\n")
