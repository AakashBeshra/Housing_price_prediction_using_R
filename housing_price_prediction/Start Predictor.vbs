Set WshShell = CreateObject("WScript.Shell")
WshShell.CurrentDirectory = "F:\ACE Documents\housing_price_prediction"
WshShell.Run "cmd /c Rscript -e ""shiny::runApp('app/app.R', port = 8888, launch.browser = TRUE)""", 0, False
MsgBox "Housing Price Predictor is starting... Check your browser!", vbInformation, "App Launcher"