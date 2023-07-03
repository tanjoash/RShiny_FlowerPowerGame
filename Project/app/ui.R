## UI for the app ##
ui <- shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  h1("hello"),
  htmlOutput("testing"),
  actionButton("plus", "Plus")
))