@echo off
title Housing Price Predictor
color 0A
echo ========================================
echo    🏠 HOUSING PRICE PREDICTOR
echo ========================================
echo.
echo Starting the application...
echo This will open in your web browser...
echo.
echo If you see any errors, please ensure:
echo 1. R is installed on your computer
echo 2. You have internet connection
echo.
echo Press Ctrl+C to stop the application
echo.

:: Change to the script directory
cd /d "F:\ACE Documents\housing_price_prediction"

:: Run the Shiny app
Rscript -e "shiny::runApp('app/app.R', port = 8888, launch.browser = TRUE)"

pause