## Server logic ##
server <- function(input, output, session){
  
  ## First page logic ##
  source("logic/instructionsManager.R")
  source("logic/demandGeneration.R")
  source("logic/createInventory.R")
  source("logic/endDayCalculations.R")
  
  # reactiveValues objects for storing items like the user password
  vals <- reactiveValues(playerid = NULL,
                         day = 0,
                         cost = 0,
                         revenue = 0,
                         cashbal = 500,
                         startday = FALSE,
                         orders_fulfilled = 0,
                         total_orders = 0,
                         bouquet_left = c(0, 0, 0, 0, 0, 0),
                         bouquet_exp = c(0, 0, 0, 0, 0, 0), # need to linkt o database
                         flowers_left = c(10, 10, 20, ""),
                         flowers_exp = c(10, 10, 20, ""),
                         demand_forecast = c(0, 0, 0, 0, 0, 0),
                         calculator_vals = c(0, 0, 0),
                         actual_demand = c(6, 5, 4, 3, 2, 1),
                         daily_revenue = 0,
                         daily_cost = 0,
                         daily_profit = 0)
  
  #placeholder list
  nextdaydemand <- c(1,2,3,4,5,6)
  actual_demand <- (c(6,5,4,3,2,1))
  bouquets_made <- c(8,7,6,5,4,3)
  
  
  # Observe button that starts game
  shinyjs::onclick("resetGame", updateTabsetPanel(session, "flowerPages", "StartingPage"))

  #instruction Modal stuff
  instructPage <- reactiveVal(1)
  # Observe button that opens instructions modal
  observeEvent(input$instructions, {
    showModal(instructionsModal())
    instructPage(1)
  })
  
  observeEvent(input$btn_previous, {
    instructPage(max(1, instructPage() - 1))
    updatePageContent()
  })
  
  observeEvent(input$btn_next, {
    instructPage(min(7, instructPage() + 1))
    updatePageContent()
  })
  observe({
    # Disable the "Next" button on the last page
    if (instructPage() == 6) {
      shinyjs::disable("btn_next")
    } else {
      shinyjs::enable("btn_next")
    }
    
    # Disable the "Previous" button on the first page
    if (instructPage() == 1) {
      shinyjs::disable("btn_previous")
    } else {
      shinyjs::enable("btn_previous")
    }
  })
  observeEvent(input$btn_close, {
    removeModal()
  })
  
  updatePageContent <- function() {
    output$page_content <- renderUI({
      switch(instructPage(),
             "1" = img(id="instruct_1", src="assets/Instructions pg1.png", width = "570px"),
             "2" = img(id="instruct_2", src="assets/Instructions pg2.png", width = "570px"),
             "3" = img(id="instruct_3", src="assets/Instructions pg3.png", width = "570px"),
             # "4" = img(id="instruct_4", src=""),
             "4" = p("hi"),
             "5" = img(id="instruct_5", src="assets/Instructions pg5.png", width = "570px"),
             "6" = img(id="instruct_6", src="assets/Instructions pg6.png", width = "570px"),
      )
    })
  }
  
  # Get Seed
  observeEvent(input$playButton, {
    vals$playerid <- 1 # need to change
    vals$total_orders <- total_demand_calc(vals$day, vals$playerid)
    vals$demand_forecast <- c(forecastB1[vals$day+1], forecastB2[vals$day+1], forecastB3[vals$day+1], forecastB4[vals$day+1], forecastB5[vals$day+1], forecastB6[vals$day+1])
    createInventory()
    #if (!is.null(input$playername) && input$playername != "") {
    # regexNumber <- "^[0-9]+$"
    # if (grepl(regexNumber, input$seedNumber)) { 
    #   setSeed(input$playerName, as.integer(input$seedNumber))
    updateTabsetPanel(session, "flowerPages", "SecondPage")
    if (vals$startday == FALSE || vals$day == 0){
      showModal(startgameModal())
    }
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
    #showModal(startgameModal())
    
    ### Number Output for Start Game Modal ###
    #Inventory for Start Game Modal 
    output$B1Exp_start <- renderText({vals$bouquet_exp[[1]]})
    output$B2Exp_start <- renderText({vals$bouquet_exp[[2]]})
    output$B3Exp_start <- renderText({vals$bouquet_exp[[3]]})
    output$B4Exp_start <- renderText({vals$bouquet_exp[[4]]})
    output$B5Exp_start <- renderText({vals$bouquet_exp[[5]]})
    output$B6Exp_start <- renderText({vals$bouquet_exp[[6]]})
    output$roseLeft_start <- renderText({vals$flowers_left[[1]]})
    output$babyLeft_start <- renderText({vals$flowers_left[[2]]})
    output$carnLeft_start <- renderText({vals$flowers_left[[3]]})
    output$roseExp_start <- renderText({vals$flowers_exp[[1]]})
    output$babyExp_start <- renderText({vals$flowers_exp[[2]]})
    output$carnExp_start <- renderText({vals$flowers_exp[[3]]})
    
    #Demand Forecast for Start Game Modal 
    output$B1demandforecast_start <- renderText({vals$demand_forecast[[1]]})
    output$B2demandforecast_start <- renderText({vals$demand_forecast[[2]]})
    output$B3demandforecast_start <- renderText({vals$demand_forecast[[3]]})
    output$B4demandforecast_start <- renderText({vals$demand_forecast[[4]]})
    output$B5demandforecast_start <- renderText({vals$demand_forecast[[5]]})
    output$B6demandforecast_start <- renderText({vals$demand_forecast[[6]]})
    
    output$R <- renderText({vals$calculator_vals[1]})
    output$C <- renderText({vals$calculator_vals[2]})
    output$B <- renderText({vals$calculator_vals[3]})
    
        # updateTabsetPanel(session, "flowerPages", "SecondPage")
      
  })
  
  shinyjs::onclick("dayButton", showModal(cal_menuModal()))
  shinyjs::onclick("inventoryButton", showModal(inventoryModal()))
  shinyjs::onclick("endDay", showModal(enddayModal()))
  shinyjs::onclick("orderButton", showModal(order_fulfilmentModal()))
  shinyjs::onclick("scoreButton", showModal(score_leaderboardModal()))
  
  observeEvent(input$calc, {
    calculator_R <- 3*as.numeric(input$B1calc) + 3*as.numeric(input$B2calc) + 5*as.numeric(input$B4calc)
    calculator_C <- 3*as.numeric(input$B1calc) + 3*as.numeric(input$B3calc) + 5*as.numeric(input$B5calc)
    calculator_B <- 5*as.numeric(input$B2calc) + 5*as.numeric(input$B3calc) + 10*as.numeric(input$B6calc)
    vals$calculator_vals <- c(calculator_R, calculator_C, calculator_B)
  })
  
  observeEvent(input$startday_btn, {
    # Get the values from the textInputs
    B1_make <- as.integer(input$B1choice)
    B2_make <- as.integer(input$B2choice)
    B3_make <- as.integer(input$B3choice)
    B4_make <- as.integer(input$B4choice)
    B5_make <- as.integer(input$B5choice)
    B6_make <- as.integer(input$B6choice) # NEED TO CHECK IF CAN MAKE THE BOUQUETS
    if(is.numeric(B1_make) || is.numeric(B2_make) || is.numeric(B3_make) || is.numeric(B4_make) || is.numeric(B5_make) || is.numeric(B6_make)){
      print("success") # run some function to calculate B1 to B6e
      updateBouquetsMade(vals$day, bouquetsInventory, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
      updateOrdersFulfilled(vals$day, vals$actual_demand, bouquetsInventory, ordersFulfilled)
      vals$revenue <- calculateRevenue(vals$day, ordersFulfilled)
      calculateCashBal(vals$cost, vals$revenue, vals$cashbal)
    } else {
      print("Error")
    }
    #values$makeB1 <- ifelse(input$B1choice == "", 0, input$B1choice)
    #values$makeB2 <- ifelse(input$B2choice == "", 0, input$B2choice)
    #values$makeB3 <- ifelse(input$B3choice == "", 0, input$B3choice)
    #values$makeB4 <- ifelse(input$B4choice == "", 0, input$B4choice)
    #values$makeB5 <- ifelse(input$B5choice == "", 0, input$B5choice)
    #values$makeB6 <- ifelse(input$B6choice == "", 0, input$B6choice)
    
    # Update demand forecast for next day
    vals$demand_forecast <- c(forecastB1[vals$day+1], forecastB2[vals$day+1], forecastB3[vals$day+1], forecastB4[vals$day+1], forecastB5[vals$day+1], forecastB6[vals$day+1])
    
    # Close the modal after saving
    removeModal()
  })
  
  #calendar menu prompt
  shinyjs::onclick("month_fc", showModal(month_fcModal()))
  shinyjs::onclick("next_fc", showModal(next_fcModal()))
  shinyjs::onclick("cal_button", showModal(calendarModal()))
  
  output$nextday_B1 <- renderText({
    paste("Bouquet 1:", nextdaydemand[1])
  })
  output$nextday_B2 <- renderText({
    paste("Bouquet 2:", nextdaydemand[2])
  })
  output$nextday_B3 <- renderText({
    paste("Bouquet 3:", nextdaydemand[3])
  })
  output$nextday_B4 <- renderText({
    paste("Bouquet 4:", nextdaydemand[4])
  })
  output$nextday_B5 <- renderText({
    paste("Bouquet 5:", nextdaydemand[5])
  })
  output$nextday_B6 <- renderText({
    paste("Bouquet 6:", nextdaydemand[6])
  })
  output$nextday_modal_title <- renderText({
    paste("Tomorrow's Demand")
  })
  ### Number output for Inventory Modal ###
  #output text for the diff values
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
    paste(vals$orders_fulfilled, "/", vals$total_orders)
  })
  
  bouquet_prefix <- c("B1", "B2", "B3", "B4", "B5", "B6")
  flower_prefix <- c("R", "B", "C", "")
  
  
  matrix_bexp <- reactive({
    paste_vals <- paste(bouquet_prefix, ":", vals$bouquet_exp)
    matrix(paste_vals, nrow = 2, byrow = TRUE)
  })
  
  matrix_fexp <- reactive({
    paste_vals <- paste(flower_prefix, ":", vals$flowers_exp)
    matrix(paste_vals, nrow = 2, byrow = TRUE)
  })
  
  matrix_fleft <- reactive({
    paste_vals <- paste(flower_prefix, ":", vals$flowers_left)
    matrix(paste_vals, nrow = 2, byrow = TRUE)
  })
  
  observeEvent(input$endday_btn, {
    r_order <- as.integer(input$r_order)
    c_order <- as.integer(input$c_order)
    b_order <- as.integer(input$b_order)
    staff_hire <- as.integer(input$staff_hire)
    staff_fire <- as.integer(input$staff_fire)
    manpower <- staff_hire-staff_fire
    if(is.numeric(r_order) || is.numeric(c_order) || is.numeric(b_order) || is.numeric(staff_fire) || is.numeric(staff_hire)){
      if(min(staff_fire, staff_hire) == 0){
        updateEodOrdered(vals$day, eodOrdered, r_order, c_order, b_order, manpower)
      } else {
        print("You cannot fire and hire at the same time") #need render?
      }
    } else {
      print("Please input integers only") #need render?
    }
    updateFlowersInvStart(vals$day, flowersInventory, eodOrdered)
    updateBouquetsInvStart()
    vals$cost <- calculateCost(vals$day, eodOrdered)
    vals$day <- vals$day+1
    vals$startday <- TRUE
    # Close the modal after saving
    removeModal()
    showModal(startgameModal())
  })
  
  # Render the table output
  output$BouqExpOutput <- renderTable({
    matrix_bexp()
  }, include.rownames = FALSE, include.colnames = FALSE)
  output$flowExpOutput <- renderTable({
    matrix_fexp()
  }, include.rownames = FALSE, include.colnames = FALSE)
  output$flowLeftOutput <- renderTable({
    matrix_fleft()
  }, include.rownames = FALSE, include.colnames = FALSE)
  
  output$dailyRevenue <- renderText({
    paste("$", vals$daily_revenue)
  })
  output$dailyCost <- renderText({
    paste("$", vals$daily_cost)
  })
  output$dailyProfit <- renderText({
    paste("$", vals$daily_profit)
  })
  
  ### Number Output for Order Fulfilment Modal ###      
  output$order_fulfilmentTitle <- renderText({paste("Order Fulfilment")})   
  output$bouquet1fulfilled <- renderText({
    paste("Bouquet 1:",bouquets_made[1], "/", actual_demand[1])
    })   
  output$bouquet2fulfilled <- renderText({
    paste("Bouquet 2:",bouquets_made[2], "/", actual_demand[2])
    })   
  output$bouquet3fulfilled <- renderText({
    paste("Bouquet 3:",bouquets_made[3], "/", actual_demand[3])
    })   
  output$bouquet4fulfilled <- renderText({
    paste("Bouquet 4:",bouquets_made[4], "/", actual_demand[4])
    })   
  output$bouquet5fulfilled <- renderText({
    paste("Bouquet 5:",bouquets_made[5], "/", actual_demand[5])
    })   
  output$bouquet6fulfilled <- renderText({
    paste("Bouquet 6:",bouquets_made[6], "/", actual_demand[6])
    })
  
  ## Table Output for Leaderboard    
  player_name <- c("Penis0", "Penis1", "Penis2", "Penis3", "Penis4", "Penis5")   
  player_score <- c("0", "1", "2", "3", "4", "5")      
  # Create matrix for leaderboard    
  matrix_leaderboard <- matrix(     
    paste(player_name, ":", c(player_score)),     
    nrow = 2, byrow = TRUE)      
  # Render Leaderboard Table   
  output$scoreLeaderboard <- renderTable({
    matrix_leaderboard}, include.rownames = FALSE, include.colnames = FALSE)
  
  output$cashBal <- renderText({
    paste(vals$cashbal)
  })
}