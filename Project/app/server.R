## Server logic ##
server <- function(input, output, session){
  
  ## First page logic ##
  source("logic/instructionsManager.R")
  source("logic/demandGeneration.R")
    
  # Observe button that starts game
  shinyjs::onclick("resetGame", updateTabsetPanel(session, "flowerPages", "StartingPage"))

  
  # Observe button that opens instructions modal
  observeEvent(input$instructions, {
    showModal(instructionsModal())
  })
  
  # Get Seed
  observeEvent(input$playButton, {
    #if (!is.null(input$playername) && input$playername != "") {
      regexNumber <- "^[0-9]+$"
      if (grepl(regexNumber, input$seedNumber)) { 
        setSeed(as.integer(input$seedNumber))
        updateTabsetPanel(session, "flowerPages", "SecondPage")
      } else if (!is.null(input$seedNumber)) {
        setSeed()
        updateTabsetPanel(session, "flowerPages", "SecondPage")
      } else {
        # DO SOMETHING IF NOT MET
        print("Not a Positive Integer")
      }
    #} else {
    # showModal(nameError())
    #}
  })
  
  shinyjs::onclick("dayButton", showModal(calendarModal()))
}