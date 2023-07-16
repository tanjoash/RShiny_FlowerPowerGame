### Main App ###

## Initiate Libraries ##
source("usePackages.R")
pkgnames <- c("shiny","shinyjs","DBI","jsonlite")
loadPkgs(pkgnames)

shinyAppDir("app")