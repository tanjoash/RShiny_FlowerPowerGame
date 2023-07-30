### Main App ###

## Initiate Libraries ##
source("usePackages.R")
pkgnames <- c("shiny","shinyjs","DBI","jsonlite")
loadPkgs(pkgnames)


library(shiny)
library(shinyjs)
library(DT)

shinyAppDir("app")