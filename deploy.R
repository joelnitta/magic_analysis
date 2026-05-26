# Deploy Shiny app to shinyapps.io
#
# Usage:
# 1. Set up your shinyapps.io account token (see below).
# 2. Run this script in the R console after running targets::tar_make().
#
# This will deploy app.R and the required data files to shinyapps.io.

# ---- Setup (run once per machine) ----
# Uncomment and fill in your shinyapps.io account info the first time you deploy.
# DO NOT COMMIT CREDENTIALS TO GIT
# library(rsconnect)
# rsconnect::setAccountInfo(
#   name = "<your-account-name>",
#   token = "<your-token>",
#   secret = "<your-secret>"
# )

# ---- Deploy app ----
library(rsconnect)

# List of files required for the app to run
app_files <- c(
  "app.R",
  "data/app/wins_losses.csv",
  "data/app/deck_color_counts.csv",
  "data/app/lands_set_averages.csv"
)

# Deploy the app (change appName as desired)
rsconnect::deployApp(
  appDir = ".",
  appFiles = app_files,
  appName = "magic-dashboard",
  account = Sys.getenv("SHINYAPPS_ACCOUNT", unset = "joelnitta"),
  server = Sys.getenv("SHINYAPPS_SERVER", unset = "shinyapps.io")
)
