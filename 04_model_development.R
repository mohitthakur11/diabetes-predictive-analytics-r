# 04_model_development.R

if (!file.exists("00_setup.R")) {
  stop("Set the working directory to the folder containing 00_setup.R.")
}

source("00_setup.R")

load_packages(
  c("tidyverse", "caret", "rpart", "randomForest", "pROC", "e1071"),
  optional = "rpart.plot"
)

theme_set(theme_minimal(base_size = 12))
options(width = 140)

TRAIN_FRACTION <- 0.8
CV_FOLDS <- 10
CV_REPEATS <- 3
UPSAMPLE <- TRUE

importance_table <- function(model) {
  imp <- caret::varImp(model)$importance
  imp$Feature <- rownames(imp)
  num <- setdiff(names(imp), "Feature")

  imp$Importance <- if ("Overall" %in% num) {
    imp$Overall
  } else {
    rowMeans(imp[num], na.rm = TRUE)
  }

  imp[order(-imp$Importance), c("Feature", "Importance")]
}

positive_class_summary <- function(data, lev = NULL, model = NULL) {
  r <- pROC::roc(
    response = data$obs,
    predictor = data[[lev[2]]],
    levels = lev,
    direction = "<",
    quiet = TRUE
  )

  c(ROC = as.numeric(pROC::auc(r)))
}

# Models are trained with caret's x/y interface. This avoids model.frame()
# rejecting the intentional missing values before caret can impute them.
predictors_only <- function(data, target) {
  data[, setdiff(names(data), target), drop = FALSE]
}

evaluate <- function(model, x_test, truth, positive) {
  pred <- predict(model, newdata = x_test)
  prob <- predict(model, newdata = x_test, type = "prob")[[positive]]

  cm <- caret::confusionMatrix(pred, truth, positive = positive)

  r <- pROC::roc(
    response = truth,
    predictor = prob,
    levels = levels(truth),
    direction = "<",
    quiet = TRUE
  )

  auc_ci <- as.numeric(pROC::ci.auc(r))

  data.frame(
    Accuracy = unname(cm$overall["Accuracy"]),
    Precision = unname(cm$byClass["Precision"]),
    Recall = unname(cm$byClass["Sensitivity"]),
    Specificity = unname(cm$byClass["Specificity"]),
    F1 = unname(cm$byClass["F1"]),
    Test_AUC = as.numeric(pROC::auc(r)),
    Test_AUC_CI_Lower = auc_ci[1],
    Test_AUC_CI_Upper = auc_ci[3]
  )
}

run_modeling <- function(df, target, name, tag) {
  cat(
    "\n", strrep("=", 62),
    "\nDATASET: ", name, " (", nrow(df), " rows)\n",
    strrep("=", 62), "\n",
    sep = ""
  )

  df <- as.data.frame(df)
  df[[target]] <- droplevels(as.factor(df[[target]]))

  if (nlevels(df[[target]]) != 2L) {
    stop(name, " must have exactly two target classes.", call. = FALSE)
  }

  positive <- levels(df[[target]])[2]

  set.seed(SEED)
  idx <- caret::createDataPartition(
    df[[target]],
    p = TRAIN_FRACTION,
    list = FALSE
  )

  train_data <- df[idx, , drop = FALSE]
  test_data <- df[-idx, , drop = FALSE]
  y_train <- train_data[[target]]
  y_test <- test_data[[target]]

  x_train_raw <- train_data[, setdiff(names(train_data), target), drop = FALSE]
  x_test_raw <- test_data[, setdiff(names(test_data), target), drop = FALSE]

  cat(
    "Train:", nrow(train_data),
    "| Test:", nrow(test_data),
    "| Positive class:", positive, "\n"
  )

  if (nrow(test_data) < 60) {
    cat("Note: the held-out test set is small; metrics may be unstable.\n")
  }

  # Convert categorical predictors to numeric indicator columns. This is
  # especially important for the early-stage dataset, which has many factors.
  encoder <- caret::dummyVars(
    ~ .,
    data = x_train_raw,
    fullRank = TRUE
  )

  x_train <- as.data.frame(predict(encoder, newdata = x_train_raw))
  x_test <- as.data.frame(predict(encoder, newdata = x_test_raw))

  # Treat any non-finite values as missing so caret can impute them.
  x_train[] <- lapply(x_train, function(x) {
    x <- as.numeric(x)
    x[!is.finite(x)] <- NA_real_
    x
  })
  x_test[] <- lapply(x_test, function(x) {
    x <- as.numeric(x)
    x[!is.finite(x)] <- NA_real_
    x
  })

  # Remove columns that are entirely missing or constant in the training set.
  all_missing <- vapply(x_train, function(x) all(is.na(x)), logical(1))
  if (any(all_missing)) {
    x_train <- x_train[, !all_missing, drop = FALSE]
    x_test <- x_test[, !all_missing, drop = FALSE]
  }

  nzv <- caret::nearZeroVar(x_train)
  if (length(nzv) > 0L) {
    x_train <- x_train[, -nzv, drop = FALSE]
    x_test <- x_test[, -nzv, drop = FALSE]
  }

  if (ncol(x_train) == 0L) {
    stop("No usable predictor columns remain for ", name, ".", call. = FALSE)
  }

  set.seed(SEED)
  folds <- caret::createMultiFolds(
    y_train,
    k = min(CV_FOLDS, min(table(y_train))),
    times = CV_REPEATS
  )

  ctrl <- caret::trainControl(
    method = "repeatedcv",
    number = min(CV_FOLDS, min(table(y_train))),
    repeats = CV_REPEATS,
    index = folds,
    classProbs = TRUE,
    summaryFunction = positive_class_summary,
    sampling = if (UPSAMPLE) "up" else NULL,
    savePredictions = "final"
  )

  fit <- function(method, ...) {
    set.seed(SEED)
    suppressWarnings(
      caret::train(
        x = x_train,
        y = y_train,
        method = method,
        metric = "ROC",
        trControl = ctrl,
        preProcess = c("medianImpute", "center", "scale"),
        ...
      )
    )
  }

  cat("Training models...\n")

  models <- list(
    "Logistic Regression" = fit("glm", family = binomial),
    "Decision Tree" = fit("rpart", tuneLength = 10),
    "Random Forest" = fit("rf", tuneLength = 5, ntree = 300),
    "KNN" = fit("knn", tuneLength = 10)
  )

  cv_auc <- purrr::map_dfr(names(models), function(model_name) {
    res <- models[[model_name]]$results
    best <- res[which.max(res$ROC), , drop = FALSE]

    data.frame(
      Model = model_name,
      CV_AUC = best$ROC,
      CV_AUC_SD = best$ROCSD
    )
  })

  test_metrics <- purrr::map_dfr(names(models), function(model_name) {
    data.frame(
      Model = model_name,
      evaluate(models[[model_name]], x_test, y_test, positive)
    )
  })

  metrics <- dplyr::left_join(cv_auc, test_metrics, by = "Model") %>%
    dplyr::mutate(Dataset = name, .before = 1) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4)))

  cat("\nModel comparison\n")
  print(metrics, row.names = FALSE)

  best_name <- metrics$Model[which.max(metrics$CV_AUC)]
  best_model <- models[[best_name]]

  cat("\nBest model by cross-validated AUC:", best_name, "\n")

  best_pred <- predict(best_model, newdata = x_test)
  cm <- caret::confusionMatrix(
    best_pred,
    y_test,
    positive = positive
  )

  print(cm$table)

  save_table(
    as.data.frame(cm$table),
    paste0(
      tag, "_confusion_matrix_",
      gsub(" ", "_", tolower(best_name)),
      ".csv"
    )
  )

  rocs <- purrr::map(models, function(model) {
    prob <- predict(model, newdata = x_test, type = "prob")[[positive]]

    pROC::roc(
      response = y_test,
      predictor = prob,
      levels = levels(y_test),
      direction = "<",
      quiet = TRUE
    )
  })

  names(rocs) <- sprintf(
    "%s (AUC = %.3f)",
    names(models),
    purrr::map_dbl(rocs, ~ as.numeric(pROC::auc(.x)))
  )

  p <- pROC::ggroc(rocs, linewidth = 1) +
    ggplot2::geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      colour = "grey60"
    ) +
    ggplot2::labs(
      title = paste0(name, ": ROC Curves (Test Set)"),
      colour = "Model"
    ) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(colour = ggplot2::guide_legend(nrow = 2))

  save_plot(p, paste0(tag, "_roc_curves.png"), 7, 6)

  imp <- head(importance_table(best_model), 15)

  p <- ggplot2::ggplot(
    imp,
    ggplot2::aes(reorder(Feature, Importance), Importance)
  ) +
    ggplot2::geom_col(fill = "#2a9d8f") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = paste0(name, ": Variable Importance - ", best_name),
      x = NULL,
      y = "Importance"
    )

  save_plot(p, paste0(tag, "_variable_importance.png"), 7, 5)

  if (requireNamespace("rpart.plot", quietly = TRUE)) {
    save_base_plot(
      paste0(tag, "_decision_tree.png"),
      rpart.plot::rpart.plot(
        models[["Decision Tree"]]$finalModel,
        main = paste0(name, ": Decision Tree")
      ),
      width = 1100,
      height = 700
    )
  } else {
    cat("Decision tree plot skipped: rpart.plot is not installed.\n")
  }

  model_path <- file.path(
    "outputs/models",
    paste0("best_model_", tag, ".rds")
  )

  # Save the encoder and retained columns with the model so future predictions
  # can use the same feature transformation.
  saveRDS(
    list(
      model = best_model,
      encoder = encoder,
      retained_columns = names(x_train),
      target = target,
      positive_class = positive
    ),
    model_path
  )
  cat("Saved model pipeline:", model_path, "\n")

  metrics
}

pima <- read.csv("data/pima_model_input.csv", stringsAsFactors = FALSE)
early_dedup <- load_early_clean()

pima$Outcome <- factor(pima$Outcome, levels = c("No", "Yes"))
early_dedup$class <- factor(early_dedup$class, levels = c("Negative", "Positive"))

# Symptom and gender predictors are categorical.
early_sym_cols <- setdiff(names(early_dedup), c("Age", "Gender", "class"))
for (v in early_sym_cols) {
  early_dedup[[v]] <- factor(early_dedup[[v]], levels = c("No", "Yes"))
}
early_dedup$Gender <- factor(early_dedup$Gender, levels = c("Female", "Male"))

# The 520-row file contains 269 exact duplicate records. Do not evaluate
# models on that file with a random row split: copies can enter both train
# and test sets and inflate performance. Use the 251-row deduplicated file
# for primary early-stage model evaluation; retain all rows for descriptive
# reporting and a transparent duplicate-handling sensitivity discussion.
cat("\nNOTE: Early-stage model evaluation uses the deduplicated 251-row dataset.\n")
cat("The 520-row file is excluded from model scoring because exact duplicates can leak across a random split.\n")

all_metrics <- dplyr::bind_rows(
  run_modeling(pima, "Outcome", "PIMA", "pima"),
  run_modeling(early_dedup, "class", "EARLY (deduplicated)", "early_deduplicated")
)

save_table(all_metrics, "model_comparison.csv")

cat("\nBest model per dataset, selected by cross-validated AUC\n")
print(
  as.data.frame(
    all_metrics %>%
      dplyr::group_by(Dataset) %>%
      dplyr::slice_max(CV_AUC, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup()
  ),
  row.names = FALSE
)

cat("\nModeling complete. Primary results saved in outputs/.\n")
