# Package Requirements for Housing Price Prediction Project

# Set CRAN mirror first
options(repos = c(CRAN = "https://cloud.r-project.org"))

required_packages <- c(
  # Core tidyverse
  "dplyr", "tidyr", "purrr", "ggplot2", "readr", "stringr", "forcats", "tibble",
  # Modeling
  "caret", "randomForest", "xgboost", 
  # Visualization
  "corrplot", "GGally", "patchwork",
  # Tidymodels ecosystem
  "recipes", "parsnip", "yardstick", "workflows", "tune", "dials", "rsample",
  # Utilities
  "skimr", "broom"
)

# Install missing packages
install_missing <- required_packages[!required_packages %in% installed.packages()[,"Package"]]

if(length(install_missing) > 0) {
  cat("Installing", length(install_missing), "missing packages...\n")
  cat("Packages to install:", paste(install_missing, collapse = ", "), "\n")
  
  # Install packages one by one with error handling
  for (pkg in install_missing) {
    cat("Installing", pkg, "...\n")
    tryCatch({
      install.packages(pkg, quiet = TRUE)
      cat("✅", pkg, "installed successfully\n")
    }, error = function(e) {
      cat("❌ Failed to install", pkg, ":", e$message, "\n")
    })
  }
} else {
  cat("All packages are already installed.\n")
}

# Load all packages
cat("\nLoading packages...\n")
loaded_packages <- c()
failed_packages <- c()

for (pkg in required_packages) {
  tryCatch({
    suppressPackageStartupMessages(library(pkg, character.only = TRUE, quietly = TRUE))
    loaded_packages <- c(loaded_packages, pkg)
    cat("✅ Loaded:", pkg, "\n")
  }, error = function(e) {
    failed_packages <- c(failed_packages, pkg)
    cat("❌ Failed to load", pkg, ":", e$message, "\n")
  })
}

cat("\n=== Package Loading Summary ===\n")
cat("Successfully loaded:", length(loaded_packages), "packages\n")
if (length(failed_packages) > 0) {
  cat("Failed to load:", paste(failed_packages, collapse = ", "), "\n")
} else {
  cat("✅ All packages loaded successfully!\n")
}