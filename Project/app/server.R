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
                         startday=FALSE,
                         orders_fulfilled = 0,
                         total_orders = 0,
                         bouquet_left = c(0, 0, 0, 0, 0, 0),
                         bouquet_exp = c(0, 0, 0, 0, 0, 0),
                         flowers_left = c(10, 10, 20, ""),
                         og1 = 0,
                         flowers_exp = c(10, 10, 20, ""),
                         demand_forecast = c(0, 0, 0, 0, 0, 0),
                         daily_revenue = 0,
                         daily_cost = 0,
                         daily_profit = 0)
  
  r_order_reactive <- reactive({
    # Access the value of the "r_order" text input and return it
    input$r_order
  })
  
  #placeholder list
  nextdaydemand <- c(1,2,3,4,5,6)
  actual_demand <- (c(6,5,4,3,2,1))
  bouquets_made <- c(8,7,6,5,4,3)
  
  
  # Observe button that starts game
  shinyjs::onclick("resetGame", updateTabsetPanel(session, "flowerPages", "StartingPage"))

  
  # Observe button that opens instructions modal
  observeEvent(input$instructions, {
    showModal(instructionsModal())
  })
  
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
    output$B1Left_start <- renderText({vals$bouquet_left[[1]]})
    output$B2Left_start <- renderText({vals$bouquet_left[[2]]})
    output$B3Left_start <- renderText({vals$bouquet_left[[3]]})
    output$B4Left_start <- renderText({vals$bouquet_left[[4]]})
    output$B5Left_start <- renderText({vals$bouquet_left[[5]]})
    output$B6Left_start <- renderText({vals$bouquet_left[[6]]})
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
    
        # updateTabsetPanel(session, "flowerPages", "SecondPage")
      
  })
  
  shinyjs::onclick("dayButton", showModal(cal_menuModal()))
  shinyjs::onclick("inventoryButton", showModal(inventoryModal()))
  shinyjs::onclick("endDay", showModal(enddayModal()))
  shinyjs::onclick("orderButton", showModal(order_fulfilmentModal()))
  shinyjs::onclick("scoreButton", showModal(score_leaderboardModal()))
  
  observeEvent(input$B1choice, {
    new_value <- as.integer(input$B1choice)
    og1 <- as.integer(vals$og1)
    if(new_value > og1){
      vals$flowers_left[1] <- as.numeric(vals$flowers_left[1]) - (new_value-og1)*3
      vals$flowers_left[2] <- as.numeric(vals$flowers_left[2]) - (new_value-og1)*3
      vals$og1 <- new_value
    } else if (new_value < og1){
      vals$flowers_left[1] <- as.numeric(vals$flowers_left[1]) + (og1-new_value)*3
      vals$flowers_left[2] <- as.numeric(vals$flowers_left[2]) + (og1-new_value)*3
      vals$og1 <- new_value
    }
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
      print("success")
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
    paste(vals$orders_fulfilled, "/", vals$total_orders)
  })
  
  bouquet_prefix <- c("B1", "B2", "B3", "B4", "B5", "B6")
  flower_prefix <- c("R", "B", "C", "")
  
  
  matrix_bleft <- reactive({
    paste_vals <- paste(bouquet_prefix, ":", vals$bouquet_left)
    matrix(paste_vals, nrow = 2, byrow = TRUE)
  })
  
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
    vals$day <- vals$day+1
    vals$startday <- TRUE
    # Close the modal after saving
    removeModal()
    showModal(startgameModal())
  })
  
  # Render the table output
  output$BouqLeftOutput <- renderTable({
    matrix_bleft()
  }, include.rownames = FALSE, include.colnames = FALSE)
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
}