@echo off
cd /d "F:\ACE Documents\housing_price_prediction"
Rscript -e "shiny::runApp('app/app.R', port = 8888, launch.browser = TRUE)"
pause