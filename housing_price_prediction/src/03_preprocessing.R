# Data Preprocessing

# First, let's make sure we have the housing_data object
if (!exists("housing_data")) {
  stop("housing_data not found. Please run data generation first.")
}

cat("Original data dimensions:", dim(housing_data), "\n")
cat("Missing values before preprocessing:", sum(is.na(housing_data)), "\n")

# Handle missing values using median/mode imputation
housing_clean <- housing_data %>%
  mutate(
    bathrooms = ifelse(is.na(bathrooms), median(bathrooms, na.rm = TRUE), bathrooms),
    year_built = ifelse(is.na(year_built), median(year_built, na.rm = TRUE), year_built),
    lot_size = ifelse(is.na(lot_size), median(lot_size, na.rm = TRUE), lot_size)
  )

cat("Missing values after imputation:", sum(is.na(housing_clean)), "\n")

# Outlier detection using IQR method
handle_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  x[x < lower_bound] <- lower_bound
  x[x > upper_bound] <- upper_bound
  return(x)
}

# Identify numeric variables (excluding target)
numeric_vars <- housing_clean %>%
  select(where(is.numeric), -price) %>%
  names()

cat("Numeric variables for outlier treatment:", paste(numeric_vars, collapse = ", "), "\n")

# Apply outlier treatment to numeric variables (excluding target)
housing_clean <- housing_clean %>%
  mutate(across(all_of(numeric_vars), handle_outliers))

# Remove any remaining NA values just to be safe
housing_clean <- housing_clean %>%
  filter(!is.na(price)) %>%  # Ensure target has no NAs
  drop_na()  # Remove any rows with NA values

cat("Data dimensions after removing NAs:", dim(housing_clean), "\n")

# Create preprocessing recipe
recipe_spec <- recipe(price ~ ., data = housing_clean) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors()) %>%
  step_zv(all_predictors())

# Prepare the recipe
prep_recipe <- prep(recipe_spec, training = housing_clean)
housing_preprocessed <- bake(prep_recipe, new_data = housing_clean)

cat("Preprocessed data dimensions:", dim(housing_preprocessed), "\n")

# Data Splitting
set.seed(123)
split <- initial_split(housing_clean, prop = 0.8, strata = "price")
train_data <- training(split)
test_data <- testing(split)

cat("Training set size:", nrow(train_data), "\n")
cat("Testing set size:", nrow(test_data), "\n")

# Create processed versions for models that need it
train_processed <- bake(prep_recipe, new_data = train_data)
test_processed <- bake(prep_recipe, new_data = test_data)

# Save the processed data
write_csv(housing_clean, "data/processed/housing_clean.csv")
write_rds(list(train = train_data, test = test_data), "data/processed/housing_train_test.rds")

# Save the preprocessing recipe
saveRDS(prep_recipe, "data/models/housing_recipe.rds")

cat("Preprocessing completed successfully!\n")
cat("Saved files:\n")
cat("- Cleaned data: data/processed/housing_clean.csv\n")
cat("- Train/test split: data/processed/housing_train_test.rds\n")
cat("- Preprocessing recipe: data/models/housing_recipe.rds\n")