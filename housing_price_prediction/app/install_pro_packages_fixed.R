# Install professional packages with CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

pro_packages <- c("shiny", "shinythemes", "shinyWidgets", "ggplot2", "dplyr", 
                  "plotly", "DT", "bslib")

cat("Installing professional packages...\n\n")

for (pkg in pro_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("📦 Installing:", pkg, "...")
    tryCatch({
      install.packages(pkg, quiet = TRUE)
      cat(" ✅\n")
    }, error = function(e) {
      cat(" ❌ Failed:", e$message, "\n")
    })
  } else {
    cat("✅ Already installed:", pkg, "\n")
  }
}

cat("\n🎉 Package installation completed!\n")

# Check which packages loaded successfully
cat("\n📊 Installation Summary:\n")
for (pkg in pro_packages) {
  if (require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("✅", pkg, "loaded successfully\n")
  } else {
    cat("❌", pkg, "failed to load\n")
  }
}