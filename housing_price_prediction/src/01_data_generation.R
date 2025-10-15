set.seed(123)  # For reproducibility

# Generate synthetic housing dataset
n <- 1500  # Number of observations

housing_data <- tibble(
  # Numeric features
  square_footage = round(rnorm(n, 2000, 800)),
  bedrooms = sample(1:6, n, replace = TRUE, prob = c(0.05, 0.2, 0.4, 0.2, 0.1, 0.05)),
  bathrooms = round(runif(n, 1, 4.5) * 2) / 2,  # Half increments
  year_built = sample(1920:2020, n, replace = TRUE),
  lot_size = round(rnorm(n, 10000, 5000)),
  distance_to_city = round(rgamma(n, shape = 2, scale = 5), 1),
  
  # Categorical features
  location_type = sample(c("Urban", "Suburban", "Rural"), n, replace = TRUE, 
                         prob = c(0.3, 0.6, 0.1)),
  condition = sample(c("Poor", "Fair", "Good", "Excellent"), n, replace = TRUE,
                     prob = c(0.1, 0.3, 0.4, 0.2)),
  garage = sample(c(0, 1), n, replace = TRUE, prob = c(0.3, 0.7)),
  pool = sample(c(0, 1), n, replace = TRUE, prob = c(0.8, 0.2)),
  
  # Add some realistic correlations
  price_base = square_footage * 150 + 
               bedrooms * 10000 + 
               bathrooms * 15000 +
               (2024 - year_built) * (-100) +  # Older houses cheaper
               lot_size * 0.5 +
               distance_to_city * (-2000) +    # Further from city = cheaper
               garage * 15000 +
               pool * 25000
)

# Add location premium and condition adjustments
housing_data <- housing_data %>%
  mutate(
    location_premium = case_when(
      location_type == "Urban" ~ 1.3,
      location_type == "Suburban" ~ 1.1,
      location_type == "Rural" ~ 0.9
    ),
    condition_multiplier = case_when(
      condition == "Poor" ~ 0.7,
      condition == "Fair" ~ 0.9,
      condition == "Good" ~ 1.0,
      condition == "Excellent" ~ 1.2
    ),
    price = round(price_base * location_premium * condition_multiplier + 
                   rnorm(n, 0, 50000))  # Add noise
  ) %>%
  select(-price_base, -location_premium, -condition_multiplier)

# Introduce missing values (5% random)
set.seed(123)
missing_indices <- sample(1:n, n * 0.05)
housing_data$bathrooms[missing_indices[1:25]] <- NA
housing_data$year_built[missing_indices[26:50]] <- NA
housing_data$lot_size[missing_indices[51:75]] <- NA

# Convert categoricals to factors
housing_data <- housing_data %>%
  mutate(
    location_type = factor(location_type, levels = c("Urban", "Suburban", "Rural")),
    condition = factor(condition, levels = c("Poor", "Fair", "Good", "Excellent")),
    garage = factor(garage, levels = c(0, 1), labels = c("No", "Yes")),
    pool = factor(pool, levels = c(0, 1), labels = c("No", "Yes"))
  )

cat("Dataset dimensions:", dim(housing_data), "\n")
head(housing_data)
# Save the generated data
write_csv(housing_data, "data/raw/synthetic_housing_data.csv")
cat("Raw data saved to: data/raw/synthetic_housing_data.csv\n")

# Make sure the data is available for the next steps
cat("Dataset created successfully with dimensions:", dim(housing_data), "\n")
cat("First few rows:\n")
print(head(housing_data))

# Explicitly assign to global environment for the pipeline
housing_data <<- housing_data