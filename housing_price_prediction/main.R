# Housing Price Prediction - Main Execution Script
# Run this file to execute the entire pipeline from project root

# Set working directory to project root (works for both RStudio and command line)
if (Sys.getenv("RSTUDIO") == "1") {
  # Running in RStudio
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
} else {
  # Running via Rscript - ensure we're in project root
  current_dir <- getwd()
  if (basename(current_dir) == "src") {
    setwd("..")  # Move up to project root if in src/ folder
  }
}

cat("Working directory:", getwd(), "\n")
cat("Running from command line...\n")

# Check if required files exist
required_files <- c("requirements.R", "config/constants.R")
for (file in required_files) {
  if (!file.exists(file)) {
    stop("Missing required file: ", file)
  } else {
    cat("Found:", file, "\n")
  }
}

# Load configuration and requirements
cat("Loading requirements...\n")
source("requirements.R")

cat("Loading constants...\n")
source("config/constants.R")

cat("=== Starting Housing Price Prediction Pipeline ===\n")
cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# Execute pipeline steps with better error handling
steps <- list(
  list(name = "Generating synthetic housing data", file = "src/01_data_generation.R"),
  list(name = "Performing exploratory data analysis", file = "src/02_eda_analysis.R"),
  list(name = "Preprocessing and cleaning data", file = "src/03_preprocessing.R"),
  list(name = "Training initial models", file = "src/04_model_training.R"),
  list(name = "Hyperparameter tuning", file = "src/05_hyperparameter_tuning.R"),
  list(name = "Final evaluation and analysis", file = "src/06_evaluation_analysis.R")
)

for (step in steps) {
  cat("\n--- ", step$name, "---\n")
  tryCatch({
    source(step$file)
    cat("✅", step$name, "completed successfully\n")
  }, error = function(e) {
    cat("❌ Error in", step$name, ":", e$message, "\n")
    stop("Pipeline failed at: ", step$name)
  })
}

cat("\n🎉 === Pipeline Complete ===\n")
cat("All steps completed successfully!\n")
cat("Results saved to: results/\n")
cat("Models saved to: data/models/\n")
cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")