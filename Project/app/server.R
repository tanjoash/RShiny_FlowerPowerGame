## Server logic ##
server <- function(input, output, session){
  
  ## First page logic ##
  source("logic/instructionsManager.R")
    
  # Observe button that starts game
  shinyjs::onclick("resetGame", updateTabsetPanel(session, "flowerPages", "StartingPage"))
  
  # Observe button that opens instructions modal
  observeEvent(input$instructions, {
    showModal(instructionsModal())
  })
  
  observeEvent(input$playButton, {
    # if (!is.null(input$playername) && input$playername != "") {
      updateTabsetPanel(session, "flowerPages", "SecondPage")
    # } else {
    #   showModal(nameError())
    # }
  })
  
  shinyjs::onclick("dayButton", showModal(calendarModal()))
}