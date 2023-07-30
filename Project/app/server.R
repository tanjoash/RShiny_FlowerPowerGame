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
    # regexNumber <- "^[0-9]+$"
    # if (grepl(regexNumber, input$seedNumber)) { 
    #   setSeed(input$playerName, as.integer(input$seedNumber))
      updateTabsetPanel(session, "flowerPages", "SecondPage")
    # } else if (!is.null(input$seedNumber)) {
    #   setSeed(input$playerName)
    #   updateTabsetPanel(session, "flowerPages", "SecondPage")
    # } else {
    #   # DO SOMETHING IF NOT MET
    #   print("Not a Positive Integer")
    # }
    #} else {
    # showModal(nameError())
    #}
    showModal(startgameModal())
    
    ### Number Output for Start Game Modal ###
    #Inventory for Start Game Modal 
    output$B1Left_start <- renderText({bouquet_left[1]})
    output$B2Left_start <- renderText({bouquet_left[2]})
    output$B3Left_start <- renderText({bouquet_left[3]})
    output$B4Left_start <- renderText({bouquet_left[4]})
    output$B5Left_start <- renderText({bouquet_left[5]})
    output$B6Left_start <- renderText({bouquet_left[6]})
    output$B1Exp_start <- renderText({bouquet_exp[1]})
    output$B2Exp_start <- renderText({bouquet_exp[2]})
    output$B3Exp_start <- renderText({bouquet_exp[3]})
    output$B4Exp_start <- renderText({bouquet_exp[4]})
    output$B5Exp_start <- renderText({bouquet_exp[5]})
    output$B6Exp_start <- renderText({bouquet_exp[6]})
    output$roseLeft_start <- renderText({flowers_left[1]})
    output$babyLeft_start <- renderText({flowers_left[2]})
    output$carnLeft_start <- renderText({flowers_left[3]})
    output$roseExp_start <- renderText({flowers_exp[1]})
    output$babyExp_start <- renderText({flowers_exp[2]})
    output$carnExp_start <- renderText({flowers_exp[3]})
    
    #Demand Forecast for Start Game Modal 
    output$B1demandforecast_start <- renderText({demand_forecast[1]})
    output$B2demandforecast_start <- renderText({demand_forecast[2]})
    output$B3demandforecast_start <- renderText({demand_forecast[3]})
    output$B4demandforecast_start <- renderText({demand_forecast[4]})
    output$B5demandforecast_start <- renderText({demand_forecast[5]})
    output$B6demandforecast_start <- renderText({demand_forecast[6]})
    
        # updateTabsetPanel(session, "flowerPages", "SecondPage")
      
  })
  
  shinyjs::onclick("dayButton", showModal(calendarModal()))
  shinyjs::onclick("inventoryButton", showModal(inventoryModal()))
  shinyjs::onclick("endDay", showModal(enddayModal()))
  
  #placeholder list
  bouquet_left <- c(0, 10, 4, 7, 8, 9)
  flowers_left <- c(34, 45, 27, "")
  bouquet_exp <- c(01, 04, 11, 12, 09, 03)
  flowers_exp <- c(7, 15, 2, "")
  demand_forecast <- c(1,2,3,4,5,6)
  total_orders <- 15
  orders_fulfilled <- 10
  daily_revenue <- 1000
  daily_costs <- 500
  daily_profit <- daily_revenue - daily_costs
  
  #button for calendar modal
  # shinyjs::onclick("May_1", showModal())
  
  ### Number output for Inventory Modal ###
  #output text for the diff values
  output$B1Left <- renderText({
    bouquet_left[1]
  })
  output$B2Left <- renderText({
    bouquet_left[2]
  })
  output$B3Left <- renderText({
    bouquet_left[3]
  })
  output$B4Left <- renderText({
    bouquet_left[4]
  })
  output$B5Left <- renderText({
    bouquet_left[5]
  })
  output$B6Left <- renderText({
    bouquet_left[6]
  })
  output$B1Exp <- renderText({
    bouquet_exp[1]
  })
  output$B2Exp <- renderText({
    bouquet_exp[2]
  })
  output$B3Exp <- renderText({
    bouquet_exp[3]
  })
  output$B4Exp <- renderText({
    bouquet_exp[4]
  })
  output$B5Exp <- renderText({
    bouquet_exp[5]
  })
  output$B6Exp <- renderText({
    bouquet_exp[6]
  })
  output$roseLeft <- renderText({
    flowers_left[1]
  })
  output$babyLeft <- renderText({
    flowers_left[2]
  })
  output$carnLeft <- renderText({
    flowers_left[3]
  })
  output$roseExp <- renderText({
    flowers_exp[1]
  })
  output$babyExp <- renderText({
    flowers_exp[2]
  })
  output$carnExp <- renderText({
    flowers_exp[3]
  })
  
  ### Table output for End Game Modal ###
  output$fulfilledOutput <- renderText({
    paste(orders_fulfilled, "/", total_orders)
  })
  
  bouquet_prefix <- c("B1", "B2", "B3", "B4", "B5", "B6")
  flower_prefix <- c("R", "B", "C", "")
  
  matrix_bleft <- matrix(
    paste(bouquet_prefix, ":", c(bouquet_left)),
    nrow = 2, byrow = TRUE
  )
  matrix_bexp <- matrix(
    paste(bouquet_prefix, ":", c(bouquet_exp)),
    nrow = 2, byrow = TRUE
  )
  matrix_fexp <- matrix(
    paste(flower_prefix, ":", c(flowers_exp)),
    nrow = 2, byrow = TRUE
  )
  matrix_fleft <- matrix(
    paste(flower_prefix, ":", c(flowers_left)), 
    nrow = 2, byrow = TRUE
  )
  # Render the table output
  output$BouqLeftOutput <- renderTable({
    matrix_bleft
  }, include.rownames = FALSE, include.colnames = FALSE)
  output$BouqExpOutput <- renderTable({
    matrix_bexp
  }, include.rownames = FALSE, include.colnames = FALSE)
  output$flowExpOutput <- renderTable({
    matrix_fexp
  }, include.rownames = FALSE, include.colnames = FALSE)
  output$flowLeftOutput <- renderTable({
    matrix_fleft
  }, include.rownames = FALSE, include.colnames = FALSE)
  
  output$dailyRevenue <- renderText({
    paste("$", daily_revenue)
  })
  output$dailyCost <- renderText({
    paste("$", daily_costs)
  })
  output$dailyProfit <- renderText({
    paste("$", daily_profit)
  })
}