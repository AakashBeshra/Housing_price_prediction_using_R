# Install professional packages
pro_packages <- c("shiny", "shinythemes", "shinyWidgets", "ggplot2", "dplyr", 
                  "plotly", "DT", "bslib", "fontawesome")

cat("Installing professional packages...\n")
for (pkg in pro_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg)
    cat("✅ Installed:", pkg, "\n")
  } else {
    cat("📦 Already installed:", pkg, "\n")
  }
}

cat("\n🎉 All professional packages installed!\n")
cat("Your app will now have:\n")
cat("• Premium UI design\n• Interactive charts\n• Professional styling\n• Advanced features\n")