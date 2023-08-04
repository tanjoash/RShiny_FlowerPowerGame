### Main App ###

## Initiate Libraries ##
source("usePackages.R")
pkgnames <- c("shiny","shinyjs","DBI","jsonlite","DT", "plotly", "shinyBS")
loadPkgs(pkgnames)

shinyAppDir("app")