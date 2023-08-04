### Main App ### #Joash

## Initiate Libraries ##
source("usePackages.R")
pkgnames <- c("shiny","shinyjs","DBI","jsonlite","DT", "plotly", "shinyBS")
loadPkgs(pkgnames)

## Start App ##
shinyAppDir("app")