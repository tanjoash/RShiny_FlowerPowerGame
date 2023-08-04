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
      textOutput("username"),
      uiOutput("loginPlaceholder"),
      #actionButton("login", "Login"),
      actionButton("register", "Register"),
      actionButton("instructions", "Instructions"),
      uiOutput("playButtonPlaceholder"),
      #textInput("playerName", ""),
      textInput("seedNumber", "", placeholder="Optional"),
      #tags$p("Name:", class = "name-text-class"),
      tags$p("Seed:", class = "seed-text-class")
      ),
    tabPanelBody(
      "SecondPage",
      img(id="mainGame", src="assets/mainGame.png"),
      img(id="cashbal_btn", src="assets/Score Button.png"),
      img(id="orderButton", src="assets/Order button.png"),
      img(id="inventoryButton", src="assets/Inventory Button.png"),
      textOutput("cashBal"),
      textOutput("dateofmay"),
      img(id="dayButton", src="assets/DayButton 2.png"),
      img(id="leaderboard_btn", src="assets/Leaderboard button.png"),
      img(id="endDay", src="assets/End of day button.png")
      )
  )
))