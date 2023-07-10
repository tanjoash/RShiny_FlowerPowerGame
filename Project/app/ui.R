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
      img(id="menu", src="assets/menu.jpg"),
      img(id="startGame", src="assets/button.jpg"),
      actionButton("instructions", "Instructions")
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