## Server logic ##
server <- function(input, output, session){
  
  ## First page logic ##
  source("logic/instructionsManager.R")
    
  # Observe button that starts game
  shinyjs::onclick("startGame", updateTabsetPanel(session, "flowerPages", "SecondPage"))
  shinyjs::onclick("resetGame", updateTabsetPanel(session, "flowerPages", "StartingPage"))
  
  # Observe button that opens instructions modal
  observeEvent(input$instructions, {
    showModal(instructionsModal())
  })
  
  observeEvent(input$resetGame, {
    updateTabsetPanel(session, "flowerPages", "StartingPage")
  })
}