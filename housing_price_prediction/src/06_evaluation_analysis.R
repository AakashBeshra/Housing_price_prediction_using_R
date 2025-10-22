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
# Create directories
dir.create("results/figures/modeling", recursive = TRUE, showWarnings = FALSE)
dir.create("results/figures/final", recursive = TRUE, showWarnings = FALSE)
dir.create("results/reports", recursive = TRUE, showWarnings = FALSE)

# Save feature importance plots
cat("Saving feature importance plots...\n")

# Linear Regression coefficients
p_lm <- ggplot(lm_importance, aes(x = reorder(term, estimate), y = estimate)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  coord_flip() +
  labs(title = "Linear Regression: Top 10 Feature Importance", 
       x = "Feature", y = "Coefficient") +
  theme_minimal()

# Random Forest importance
p_rf <- ggplot(rf_importance, aes(x = reorder(Feature, `%IncMSE`), y = `%IncMSE`)) +
  geom_col(fill = "forestgreen", alpha = 0.8) +
  coord_flip() +
  labs(title = "Random Forest: Top 10 Feature Importance", 
       x = "Feature", y = "% Increase in MSE") +
  theme_minimal()

# XGBoost importance
p_xgb <- ggplot(xgb_importance, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_col(fill = "darkorange", alpha = 0.8) +
  coord_flip() +
  labs(title = "XGBoost: Top 10 Feature Importance", 
       x = "Feature", y = "Gain") +
  theme_minimal()

# Combine all feature importance plots
feature_plot <- p_lm / p_rf / p_xgb
ggsave("results/figures/modeling/feature_importance.png", feature_plot, 
       width = 12, height = 15, dpi = 300)

# Save residual analysis
residual_plot <- p_residuals / p_qq
ggsave("results/figures/modeling/residual_analysis.png", residual_plot, 
       width = 12, height = 8, dpi = 300)

# Save final plots
ggsave("results/figures/final/actual_vs_predicted.png", p_actual_vs_predicted, 
       width = 10, height = 6, dpi = 300)

# Save best model metrics
best_model_metrics <- tibble(
  model = best_model$model,
  rmse = best_model$rmse,
  mae = best_model$mae,
  rsq = best_model$rsq,
  timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
)

write_csv(best_model_metrics, "results/figures/final/best_model_metrics.csv")

cat("All evaluation plots saved successfully!\n")
# Generate HTML report
cat("Generating final report...\n")

report_content <- paste(
  "<!DOCTYPE html>",
  "<html>",
  "<head>",
  "<title>Housing Price Prediction - Final Report</title>",
  "<style>",
  "body { font-family: Arial, sans-serif; margin: 40px; }",
  ".header { background: #f4f4f4; padding: 20px; border-radius: 5px; }",
  ".metrics { background: #e8f4f8; padding: 15px; margin: 10px 0; border-radius: 5px; }",
  ".plot { text-align: center; margin: 20px 0; }",
  "img { max-width: 100%; height: auto; border: 1px solid #ddd; border-radius: 4px; }",
  "</style>",
  "</head>",
  "<body>",
  "<div class='header'>",
  "<h1>🏠 Housing Price Prediction - Final Report</h1>",
  "<p><strong>Generated:</strong> ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "</p>",
  "</div>",
  "",
  "<div class='metrics'>",
  "<h2>📊 Best Model Performance</h2>",
  "<p><strong>Best Model:</strong> ", best_model$model, "</p>",
  "<p><strong>RMSE:</strong> $", round(best_model$rmse, 2), "</p>",
  "<p><strong>MAE:</strong> $", round(best_model$mae, 2), "</p>",
  "<p><strong>R²:</strong> ", round(best_model$rsq, 4), "</p>",
  "</div>",
  "",
  "<div class='plot'>",
  "<h2>📈 Actual vs Predicted Prices</h2>",
  "<img src='../figures/final/actual_vs_predicted.png' alt='Actual vs Predicted'>",
  "</div>",
  "",
  "<div class='plot'>",
  "<h2>🔍 Feature Importance</h2>",
  "<img src='../figures/modeling/feature_importance.png' alt='Feature Importance'>",
  "</div>",
  "",
  "<div class='plot'>",
  "<h2>📊 Model Performance Comparison</h2>",
  "<img src='../figures/modeling/model_performance.png' alt='Model Performance'>",
  "</div>",
  "",
  "<div class='plot'>",
  "<h2>📉 Residual Analysis</h2>",
  "<img src='../figures/modeling/residual_analysis.png' alt='Residual Analysis'>",
  "</div>",
  "",
  "<div class='metrics'>",
  "<h2>🎯 Conclusion</h2>",
  "<p>The housing price prediction model achieved strong performance with ", 
  best_model$model, " demonstrating the best results. ",
  "The model explains approximately ", round(best_model$rsq * 100, 1), 
  "% of the variance in housing prices.</p>",
  "</div>",
  "</body>",
  "</html>",
  sep = "\n"
)

writeLines(report_content, "results/reports/final_model_report.html")
cat("Final report saved to: results/reports/final_model_report.html\n")