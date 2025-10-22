# Hyperparameter Tuning with Caret

cat("Starting hyperparameter tuning...\n")

# Set up cross-validation
train_control <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = FALSE,
  savePredictions = "final"
)

# Tune Random Forest
cat("Tuning Random Forest...\n")
set.seed(123)
tryCatch({
  rf_tune <- train(
    price ~ .,
    data = train_data,
    method = "rf",
    trControl = train_control,
    tuneGrid = expand.grid(mtry = c(2, 4, 6, 8)),
    importance = TRUE,
    ntree = 100  # Reduced for faster tuning
  )
  
  cat("Random Forest tuning completed\n")
  print(rf_tune$results)
}, error = function(e) {
  cat("Random Forest tuning failed:", e$message, "\n")
  # Use default model if tuning fails
  rf_tune <- randomForest(price ~ ., data = train_data, ntree = 100)
})

# Tune XGBoost
cat("Tuning XGBoost...\n")
set.seed(123)
tryCatch({
  xgb_tune <- train(
    price ~ .,
    data = train_data,
    method = "xgbTree",
    trControl = train_control,
    tuneGrid = expand.grid(
      nrounds = 50,
      max_depth = c(3, 6),
      eta = c(0.01, 0.1),
      gamma = 0,
      colsample_bytree = 1,
      min_child_weight = 1,
      subsample = 1
    ),
    verbose = FALSE
  )
  
  cat("XGBoost tuning completed\n")
  print(xgb_tune$results)
}, error = function(e) {
  cat("XGBoost tuning failed:", e$message, "\n")
  # Use default model if tuning fails
  xgb_train <- as.matrix(train_processed %>% select(-price))
  xgb_tune <- xgboost(
    data = xgb_train,
    label = train_processed$price,
    nrounds = 50,
    max_depth = 6,
    eta = 0.1,
    objective = "reg:squarederror",
    verbose = 0
  )
})

# Get tuned predictions
cat("Generating tuned predictions...\n")

if (exists("rf_tune") && "train" %in% class(rf_tune)) {
  rf_tuned_predictions <- predict(rf_tune, newdata = test_data)
} else {
  rf_tuned_predictions <- predict(rf_tune, newdata = test_data)
}

if (exists("xgb_tune") && "train" %in% class(xgb_tune)) {
  xgb_tuned_predictions <- predict(xgb_tune, newdata = test_data)
} else {
  xgb_test <- as.matrix(test_processed %>% select(-price))
  xgb_tuned_predictions <- predict(xgb_tune, xgb_test)
}

# Manual metric calculation function
calculate_metrics <- function(actual, predicted) {
  residuals <- actual - predicted
  rmse <- sqrt(mean(residuals^2))
  mae <- mean(abs(residuals))
  rsq <- cor(actual, predicted)^2
  return(list(rmse = rmse, mae = mae, rsq = rsq))
}

# Load base model metrics for comparison
base_metrics <- read_csv("data/models/model_comparison.csv", show_col_types = FALSE)

# Calculate tuned metrics
rf_tuned_metrics <- calculate_metrics(test_data$price, rf_tuned_predictions)
xgb_tuned_metrics <- calculate_metrics(test_data$price, xgb_tuned_predictions)

# Create results tibbles
rf_tuned_results <- tibble(
  model = "Random Forest (Tuned)",
  rmse = rf_tuned_metrics$rmse,
  mae = rf_tuned_metrics$mae,
  rsq = rf_tuned_metrics$rsq
)

xgb_tuned_results <- tibble(
  model = "XGBoost (Tuned)", 
  rmse = xgb_tuned_metrics$rmse,
  mae = xgb_tuned_metrics$mae,
  rsq = xgb_tuned_metrics$rsq
)

# Final comparison
final_metrics <- bind_rows(
  base_metrics,
  rf_tuned_results, 
  xgb_tuned_results
)

print("Final Model Performance Comparison:")
print(final_metrics)

# Best model selection
best_model <- final_metrics %>%
  filter(rmse == min(rmse))

cat("Best model:", best_model$model, "with RMSE:", round(best_model$rmse, 2), "\n")

# Save final comparison
write_csv(final_metrics, "data/models/final_model_comparison.csv")

# Save tuned models
if (exists("rf_tune")) {
  saveRDS(rf_tune, "data/models/rf_tuned_model.rds")
}
if (exists("xgb_tune")) {
  saveRDS(xgb_tune, "data/models/xgb_tuned_model.rds")
}

cat("Hyperparameter tuning completed successfully!\n")
cat("Best model:", best_model$model, "\n")
cat("Results saved to: data/models/final_model_comparison.csv\n")