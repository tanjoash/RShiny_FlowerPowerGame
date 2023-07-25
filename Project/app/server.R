## Server logic ##
server <- function(input, output, session){
  
  ## First page logic ##
  source("logic/instructionsManager.R")
  source("logic/demandGeneration.R")
    
  # Observe button that starts game
  shinyjs::onclick("resetGame", updateTabsetPanel(session, "flowerPages", "StartingPage"))

  
  # Observe button that opens instructions modal
  observeEvent(input$instructions, {
    demand <- getDemandEachDay()
    print(demand)
  })
  
  # Get Seed
  observeEvent(input$playButton, {
    
    #if (!is.null(input$playername) && input$playername != "") {
      regexNumber <- "^[0-9]+$"
      if (grepl(regexNumber, input$seedNumber)) { 
        setSeed(input$playerName, as.integer(input$seedNumber))
        updateTabsetPanel(session, "flowerPages", "SecondPage")
      } else if (!is.null(input$seedNumber)) {
        setSeed(input$playerName)
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
  shinyjs::onclick("inventoryButton", showModal(inventoryModal()))
  # output$BLeftTable <- renderDataTable({
  #   # Generate sample data for the table
  #   Binv_data <- data.frame(
  #     BouquetsLeft = c(0, 2, 3, 4, 5, 1)
  #   )
  #   Binv_data_t<- t(Binv_data)
  #   Binv_df <- data.frame(Binv_data_t)
  #   
  #   # Return the data table
  #   datatable(Binv_df, 
  #             options = list(
  #               ordering = FALSE,  # Disable sorting
  #               searching = FALSE,  # Disable searching
  #               paging = FALSE,  # Disable pagination
  #               info = FALSE,  # Hide "Showing 1 to 2 of 2 entries" text
  #               rowCallback = JS('function(row, data) {
  #                          $(row).css("pointer-events", "none");  // Disable row interactions
  #                        }')
  #             )
  #           )
  # })
}