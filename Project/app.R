### Main App ###

## Initiate Libraries ##
<<<<<<< Updated upstream
source("usePackages.R")
pkgnames <- c("shiny","shinyjs","DBI","jsonlite")
loadPkgs(pkgnames)

=======
library(shiny)
library(shinyjs)
library(DT)
>>>>>>> Stashed changes
shinyAppDir("app")