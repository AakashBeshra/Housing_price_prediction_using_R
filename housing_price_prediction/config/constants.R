# Project Constants and Configuration

# Analysis parameters
SEED <- 123
SAMPLE_SIZE <- 1500
TRAIN_SPLIT <- 0.8

# File paths
PROJECT_DIR <- getwd()
DATA_DIR <- file.path(PROJECT_DIR, "data")
RAW_DATA_PATH <- file.path(DATA_DIR, "raw/synthetic_housing_data.csv")
PROCESSED_DATA_PATH <- file.path(DATA_DIR, "processed/housing_clean.csv")
MODEL_DIR <- file.path(DATA_DIR, "models")
RESULTS_DIR <- file.path(PROJECT_DIR, "results")

# Model parameters
CV_FOLDS <- 5
RF_NTREES <- 500
XGB_ROUNDS <- 100

# Create directories if they don't exist
directories <- c(
  file.path(DATA_DIR, "raw"),
  file.path(DATA_DIR, "processed"), 
  file.path(DATA_DIR, "models"),
  file.path(RESULTS_DIR, "figures/eda"),
  file.path(RESULTS_DIR, "figures/modeling"),
  file.path(RESULTS_DIR, "figures/final")
)

invisible(lapply(directories, function(dir) {
  if(!dir.exists(dir)) dir.create(dir, recursive = TRUE)
}))

cat("Project constants loaded successfully!\n")