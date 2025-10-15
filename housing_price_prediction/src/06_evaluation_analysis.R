# Linear Regression coefficients
lm_importance <- tidy(lm_model) %>%
  filter(term != "(Intercept)") %>%
  arrange(desc(abs(estimate))) %>%
  head(10)

p_lm <- ggplot(lm_importance, aes(x = reorder(term, estimate), y = estimate)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  coord_flip() +
  labs(title = "Linear Regression: Top 10 Feature Importance", 
       x = "Feature", y = "Coefficient") +
  theme_minimal()

# Random Forest importance
rf_importance <- importance(rf_model) %>%
  as.data.frame() %>%
  rownames_to_column("Feature") %>%
  arrange(desc(`%IncMSE`)) %>%
  head(10)

p_rf <- ggplot(rf_importance, aes(x = reorder(Feature, `%IncMSE`), y = `%IncMSE`)) +
  geom_col(fill = "forestgreen", alpha = 0.8) +
  coord_flip() +
  labs(title = "Random Forest: Top 10 Feature Importance", 
       x = "Feature", y = "% Increase in MSE") +
  theme_minimal()

# XGBoost importance
xgb_importance <- xgb.importance(
  feature_names = colnames(train_processed %>% select(-price)),
  model = xgb_model
) %>%
  as_tibble() %>%
  head(10)

p_xgb <- ggplot(xgb_importance, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_col(fill = "darkorange", alpha = 0.8) +
  coord_flip() +
  labs(title = "XGBoost: Top 10 Feature Importance", 
       x = "Feature", y = "Gain") +
  theme_minimal()

# Display importance plots
print(p_lm)
print(p_rf)
print(p_xgb)
# Use best model for final predictions
final_predictions <- if(best_model$model == "Linear Regression") {
  lm_predictions
} else if(best_model$model == "Random Forest (Tuned)") {
  rf_tuned_predictions
} else if(best_model$model == "XGBoost (Tuned)") {
  xgb_tuned_predictions
} else if(best_model$model == "Random Forest") {
  rf_predictions
} else {
  xgb_predictions
}

# Residual analysis
residuals <- test_data$price - final_predictions

residual_analysis <- tibble(
  predictions = final_predictions,
  residuals = residuals,
  actual = test_data$price
)

# Residual plots
p_residuals <- ggplot(residual_analysis, aes(x = predictions, y = residuals)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Predicted", 
       x = "Predicted Price", y = "Residuals") +
  theme_minimal()

p_qq <- ggplot(residual_analysis, aes(sample = residuals)) +
  stat_qq(color = "blue", alpha = 0.6) +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot of Residuals") +
  theme_minimal()

p_actual_vs_predicted <- ggplot(residual_analysis, aes(x = actual, y = predictions)) +
  geom_point(alpha = 0.6, color = "purple") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted Prices", 
       x = "Actual Price", y = "Predicted Price") +
  theme_minimal()

# Display residual plots
print(p_residuals)
print(p_qq)
print(p_actual_vs_predicted)

# Final performance metrics
final_rmse <- sqrt(mean(residuals^2))
final_mae <- mean(abs(residuals))
final_rsq <- cor(test_data$price, final_predictions)^2

cat("\nFinal Model Performance:\n")
cat("RMSE:", round(final_rmse, 2), "\n")
cat("MAE:", round(final_mae, 2), "\n")
cat("R²:", round(final_rsq, 4), "\n")
# Session info for reproducibility
sessionInfo()

# Save the final model and preprocessing recipe
saveRDS(prep_recipe, "housing_recipe.rds")
if(best_model$model == "Linear Regression") {
  saveRDS(lm_model, "final_model.rds")
} else if(best_model$model == "Random Forest (Tuned)") {
  saveRDS(rf_tune, "final_model.rds")
} else if(best_model$model == "XGBoost (Tuned)") {
  saveRDS(xgb_tune, "final_model.rds")
}

cat("\nModel and recipe saved successfully!\n")