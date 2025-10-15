# Model Training and Evaluation
cat("Training set size:", nrow(train_data), "\n")
cat("Testing set size:", nrow(test_data), "\n")

# Create processed versions for models that need it
train_processed <- bake(prep_recipe, new_data = train_data)
test_processed <- bake(prep_recipe, new_data = test_data)

# Define evaluation metrics function manually (to avoid yardstick issues)
calculate_metrics <- function(actual, predicted) {
  residuals <- actual - predicted
  rmse <- sqrt(mean(residuals^2))
  mae <- mean(abs(residuals))
  rsq <- cor(actual, predicted)^2
  
  return(list(rmse = rmse, mae = mae, rsq = rsq))
}

# Model 1: Linear Regression
set.seed(123)
cat("Training Linear Regression...\n")
lm_model <- lm(price ~ ., data = train_data)
lm_predictions <- predict(lm_model, newdata = test_data)

lm_metrics <- calculate_metrics(test_data$price, lm_predictions)
lm_results <- tibble(
  model = "Linear Regression",
  rmse = lm_metrics$rmse,
  mae = lm_metrics$mae,
  rsq = lm_metrics$rsq
)

# Model 2: Random Forest
set.seed(123)
cat("Training Random Forest...\n")
rf_model <- randomForest(
  price ~ ., 
  data = train_data,
  ntree = 200,  # Reduced for faster training
  importance = TRUE,
  na.action = na.omit
)

rf_predictions <- predict(rf_model, newdata = test_data)
rf_metrics <- calculate_metrics(test_data$price, rf_predictions)
rf_results <- tibble(
  model = "Random Forest",
  rmse = rf_metrics$rmse,
  mae = rf_metrics$mae,
  rsq = rf_metrics$rsq
)

# Model 3: XGBoost
set.seed(123)
cat("Training XGBoost...\n")

# Prepare data for XGBoost
xgb_train <- as.matrix(train_processed %>% select(-price))
xgb_test <- as.matrix(test_processed %>% select(-price))

xgb_model <- xgboost(
  data = xgb_train,
  label = train_processed$price,
  nrounds = 50,  # Reduced for faster training
  max_depth = 6,
  eta = 0.1,
  objective = "reg:squarederror",
  verbose = 0
)

xgb_predictions <- predict(xgb_model, xgb_test)
xgb_metrics <- calculate_metrics(test_data$price, xgb_predictions)
xgb_results <- tibble(
  model = "XGBoost",
  rmse = xgb_metrics$rmse,
  mae = xgb_metrics$mae,
  rsq = xgb_metrics$rsq
)

# Combine all metrics
all_metrics <- bind_rows(lm_results, rf_results, xgb_results)

print("Model Performance Comparison:")
print(all_metrics)

# Visualization of model performance
performance_plot <- all_metrics %>%
  pivot_longer(cols = c(rmse, mae, rsq), names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = model, y = value, fill = model)) +
  geom_col(alpha = 0.8) +
  facet_wrap(~ metric, scales = "free") +
  labs(title = "Model Performance Comparison", y = "Value", x = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(performance_plot)

# Save the comparison results
write_csv(all_metrics, "data/models/model_comparison.csv")
cat("Model comparison saved to data/models/model_comparison.csv\n")

cat("Model training completed successfully!\n")