## UI for the app ##
ui <- shinyUI(fillPage(
  tags$head(
    tags$link(rel = "stylesheet", type="text/css", href="styling/panels.css")
  ),
  useShinyjs(),
  tabsetPanel(
    id = "flowerPages",
    type = "hidden",
    tabPanelBody(
      "StartingPage",
      img(id="startScreen", src="assets/startScreen.png"),
      img(id="gameLogo", src="assets/gameLogo.png"),
      actionButton("instructions", "Instructions"),
      actionButton("playButton", "Play"),
      textInput("playerName", "Name:"),
      textInput("seedNumber", "Seed:"),
      tags$p("Name:", class = "name-text-class"),
      tags$p("Seed:", class = "seed-text-class")
    ),
    tabPanelBody(
      "SecondPage",
      img(id="mainGame", src="assets/mainGame.png"),
      img(id="scoreButton", src="assets/scoreButton.png"),
      img(id="orderButton", src="assets/orderButton.png"),
      img(id="inventoryButton", src="assets/inventoryButton.png"),
      img(id="dayButton", src="assets/dayButton.png"),
      img(id="resetGame", src="assets/button.jpg")
      )
  )
))