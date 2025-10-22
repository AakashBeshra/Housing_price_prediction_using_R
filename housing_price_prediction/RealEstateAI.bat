@echo off
chcp 65001 >nul
title RealEstateAI - Professional Property Valuation
color 0F

echo.
echo =======================================================
echo                🏠 REALESTATEAI PRO
echo =======================================================
echo.
echo    Professional Property Valuation Platform
echo.
echo =======================================================
echo.
echo Starting AI valuation platform...
echo.

cd /d "F:\ACE Documents\housing_price_prediction"

:: Run the professional app
echo Starting RealEstateAI application...
timeout /t 2 /nobreak >nul
Rscript -e "shiny::runApp('app/app_pro.R', port = 8888, launch.browser = TRUE)"

pause