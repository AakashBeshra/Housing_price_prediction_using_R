# Basic structure and summary
str(housing_data)
summary(housing_data)

# Missing values analysis
missing_summary <- housing_data %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Missing_Count") %>%
  mutate(Missing_Pct = round(Missing_Count / nrow(housing_data) * 100, 2))

print("Missing Values Summary:")
print(missing_summary)

# Target variable distribution
p1 <- ggplot(housing_data, aes(x = price)) +
  geom_histogram(fill = "steelblue", alpha = 0.8, bins = 30) +
  labs(title = "Distribution of Housing Prices", 
       x = "Price ($)", y = "Count") +
  theme_minimal()

p2 <- ggplot(housing_data, aes(y = price)) +
  geom_boxplot(fill = "lightcoral", alpha = 0.7) +
  labs(title = "Boxplot of Housing Prices", y = "Price ($)") +
  theme_minimal()

# Combine plots
p1 + p2

# Numeric variables distributions
numeric_vars <- housing_data %>%
  select(where(is.numeric), -price) %>%
  names()

# Create histograms for all numeric variables
housing_data %>%
  select(all_of(numeric_vars)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Value)) +
  geom_histogram(fill = "cadetblue", alpha = 0.8, bins = 30) +
  facet_wrap(~ Variable, scales = "free") +
  labs(title = "Distributions of Numeric Predictors") +
  theme_minimal()

# Categorical variables analysis
categorical_vars <- housing_data %>%
  select(where(is.factor), -price) %>%
  names()

# Bar plots for categorical variables
walk(categorical_vars, function(var) {
  p <- housing_data %>%
    ggplot(aes(x = !!sym(var))) +
    geom_bar(fill = "darkolivegreen", alpha = 0.8) +
    labs(title = paste("Distribution of", var), x = var) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p)
})

# Correlation analysis for numeric variables
cor_matrix <- housing_data %>%
  select(where(is.numeric)) %>%
  cor(use = "complete.obs")

corrplot::corrplot(cor_matrix, method = "color", type = "upper", 
                   order = "hclust", tl.cex = 0.8, tl.col = "black")

# Price by categorical variables
walk(categorical_vars, function(var) {
  p <- housing_data %>%
    ggplot(aes(x = !!sym(var), y = price)) +
    geom_boxplot(fill = "goldenrod", alpha = 0.7) +
    labs(title = paste("Price by", var), x = var, y = "Price ($)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p)
})

# Scatter plots: Price vs numeric predictors (handle missing values)
walk(numeric_vars, function(var) {
  # Create a temporary dataset without NAs for this variable
  temp_data <- housing_data %>% filter(!is.na(!!sym(var)))
  
  p <- ggplot(temp_data, aes(x = !!sym(var), y = price)) +
    geom_point(alpha = 0.6, color = "purple") +
    geom_smooth(method = "lm", color = "red", se = FALSE, na.rm = TRUE) +
    labs(title = paste("Price vs", var), x = var, y = "Price ($)") +
    theme_minimal()
  print(p)
})