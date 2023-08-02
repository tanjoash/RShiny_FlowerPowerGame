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
                         manpower = 0,
                         cashbal = 500,
                         startday = FALSE,
                         orders_fulfilled = c(0, 0, 0, 0, 0, 0),
                         total_orders = 0,
                         bouquet_left = c(0, 0, 0, 0, 0, 0),
<<<<<<< Updated upstream
=======
                         bouquet_exp = c(0, 0, 0, 0, 0, 0), # need to linkt o database
                         flowers_left = c(0, 0, 0),
                         flowers_exp = c(10, 10, 20),
>>>>>>> Stashed changes
                         bouquet_exp = c(0, 0, 0, 0, 0, 0), # to remove
                         flowers_left = c(0, 0, 0, ""),
                         flowers_exp = c(1, 2, 3, ""),
                         demand_forecast = c(0, 0, 0, 0, 0, 0),
                         calculator_vals = c(0, 0, 0),
                         actual_demand = c(0, 0, 0, 0, 0, 0),
                         daily_revenue = 0,
                         daily_cost = 0,
                         daily_profit = 0)
  
  #placeholder list
  nextdaydemand <- c(1,2,3,4,5,6)
  actual_demand <- (c(6,5,4,3,2,1))
  bouquets_made <- c(8,7,6,5,4,3)
  player_name <- c("Penis0", "Penis1", "Penis2", "Penis3", "Penis4", "Penis5")   
  player_score <- c("0", "1", "2", "3", "4", "5") 
  
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
  shinyjs::onclick("back_btn", {
    showModal(cal_menuModal())
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
             "6" = img(id="instruct_6", src="assets/Instructions pg6.png", width = "570px")
      )
    })
  }
  
  # Get Seed
  observeEvent(input$playButton, {
    vals$playerid <- playerID() # need to change and get latest playerid
    print(vals$playerid)
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
<<<<<<< Updated upstream
=======
    output$numberofstaff <- renderText({69}) ### actual is reference manpower value below 
>>>>>>> Stashed changes

    #output$B1Exp_start <- renderText({vals$bouquet_exp[[1]]})
    #output$B2Exp_start <- renderText({vals$bouquet_exp[[2]]})
    #output$B3Exp_start <- renderText({vals$bouquet_exp[[3]]})
    #output$B4Exp_start <- renderText({vals$bouquet_exp[[4]]})
    #output$B5Exp_start <- renderText({vals$bouquet_exp[[5]]})
    #output$B6Exp_start <- renderText({vals$bouquet_exp[[6]]})

    output$B1Left_start <- renderText({vals$bouquet_left[[1]]})
    output$B2Left_start <- renderText({vals$bouquet_left[[2]]})
    output$B3Left_start <- renderText({vals$bouquet_left[[3]]})
    output$B4Left_start <- renderText({vals$bouquet_left[[4]]})
    output$B5Left_start <- renderText({vals$bouquet_left[[5]]})
    output$B6Left_start <- renderText({vals$bouquet_left[[6]]})
    #output$B1Exp_start <- renderText({vals$bouquet_exp[[1]]})
    #output$B2Exp_start <- renderText({vals$bouquet_exp[[2]]})
    #output$B3Exp_start <- renderText({vals$bouquet_exp[[3]]})
    #output$B4Exp_start <- renderText({vals$bouquet_exp[[4]]})
    #output$B5Exp_start <- renderText({vals$bouquet_exp[[5]]})
    #output$B6Exp_start <- renderText({vals$bouquet_exp[[6]]})
<<<<<<< Updated upstream

=======
>>>>>>> Stashed changes
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
    
    output$R <- renderText({paste("Roses Required:", vals$calculator_vals[1])})
    output$C <- renderText({paste("Carnations Required:",vals$calculator_vals[2])})
    output$B <- renderText({paste("Baby Breaths Required:", vals$calculator_vals[3])})
      
  })
  
  shinyjs::onclick("dayButton", showModal(cal_menuModal()))
  shinyjs::onclick("inventoryButton", showModal(inventoryModal()))
  shinyjs::onclick("endDay", showModal(enddayModal()))
  shinyjs::onclick("orderButton", showModal(order_fulfilmentModal()))
  shinyjs::onclick("scoreButton", showModal(score_leaderboardModal()))
  
  ## Calculator Reactive Value 
  flower_requirements <- reactiveValues(
    R = 0,
    B = 0,
    C = 0
  )
  
  ## Calculator Observe Event 
  observeEvent({input$B1calc
    input$B2calc
    input$B3calc
    input$B4calc
    input$B5calc
    input$B6calc}, {
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
    B6_make <- as.integer(input$B6choice)
    if(is.numeric(B1_make) || is.numeric(B2_make) || is.numeric(B3_make) || is.numeric(B4_make) || is.numeric(B5_make) || is.numeric(B6_make)){
      total_f1 <- as.numeric(vals$flowers_left[1]) + as.numeric(vals$flowers_exp[1])
      total_f2 <- as.numeric(vals$flowers_left[2]) + as.numeric(vals$flowers_exp[2])
      total_f3 <- as.numeric(vals$flowers_left[3]) + as.numeric(vals$flowers_exp[3])
      
      used_f1 <- 3*as.numeric(B1_make) + 3*as.numeric(B2_make) + 5*as.numeric(B4_make)
      used_f2 <- 3*as.numeric(B1_make) + 3*as.numeric(B3_make) + 5*as.numeric(B5_make)
      used_f3 <- 5*as.numeric(B2_make) + 5*as.numeric(B3_make) + 10*as.numeric(B6_make)
      
      if(used_f1 <= total_f1 || used_f2 <= total_f2 || used_f3 <= total_f3){
        if(used_f1 <= as.numeric(vals$flowers_exp[1])){ # if used flowers is less than the expiring ones to throw today
          vals$flowers_exp[1] <- as.numeric(vals$flowers_exp[1]) - used_f1 #to throw away today
        } else { 
          vals$flowers_left[1] <- as.numeric(vals$flowers_left[1]) - (used_f1 - as.numeric(vals$flowers_exp[1]))
          vals$flowers_exp[1] <- 0
        }
        
        if(used_f2 <= as.numeric(vals$flowers_exp[2])){ # if used flowers is less than the expiring ones to throw today
          vals$flowers_exp[2] <- as.numeric(vals$flowers_exp[2]) - used_f2 #to throw away today
        } else { 
          vals$flowers_left[2] <- as.numeric(vals$flowers_left[2]) - (used_f2 - as.numeric(vals$flowers_exp[2]))
          vals$flowers_exp[2] <- 0
        }
        
        if(used_f3 <= as.numeric(vals$flowers_exp[3])){ # if used flowers is less than the expiring ones to throw today
          vals$flowers_exp[3] <- as.numeric(vals$flowers_exp[3]) - used_f3 #to throw away today
        } else { 
          vals$flowers_left[3] <- as.numeric(vals$flowers_left[3]) - (used_f3 - as.numeric(vals$flowers_exp[3]))
          vals$flowers_exp[3] <- 0
        }
        
        print(paste0("Flowers Expired today", vals$flowers_exp))
        print(paste0("Flowers Left today", vals$flowers_left))

        vals$calculator_vals <- c(0, 0, 0)
        bouquetsInventory <- updateBouquetsMade(vals$day, bouquetsInventory, B1_make, B2_make, B3_make, B4_make, B5_make, B6_make)
        #print(bouquetsInventory)
        flowersInventory <- updateFlowersUsed(vals$day, flowersInventory, as.numeric(vals$flowers_left[1]),
                                              as.numeric(vals$flowers_left[2]),
                                              as.numeric(vals$flowers_left[3]),
                                              as.numeric(vals$flowers_exp[1]),
                                              as.numeric(vals$flowers_exp[2]),
                                              as.numeric(vals$flowers_exp[3]))
        #print(flowersInventory)
        
        # Calculate orders fulfilled
        vals$orders_fulfilled <- c(min(B1_make, as.numeric(vals$actual_demand[1])),
                                   min(B2_make, as.numeric(vals$actual_demand[2])),
                                   min(B3_make, as.numeric(vals$actual_demand[3])),
                                   min(B4_make, as.numeric(vals$actual_demand[4])),
                                   min(B5_make, as.numeric(vals$actual_demand[5])),
                                   min(B6_make, as.numeric(vals$actual_demand[6])))
        
        ordersFulfilled <- updateOrdersFulfilled(vals$day, vals$actual_demand, as.numeric(vals$orders_fulfilled[1]),
                                                 as.numeric(vals$orders_fulfilled[2]),
                                                 as.numeric(vals$orders_fulfilled[3]),
                                                 as.numeric(vals$orders_fulfilled[4]),
                                                 as.numeric(vals$orders_fulfilled[5]),
                                                 as.numeric(vals$orders_fulfilled[6]),
                                                 ordersFulfilled)
        #print(ordersFulfilled)
        
        vals$revenue <- calculateRevenue(vals$day, ordersFulfilled)
        vals$cashbal <- calculateCashBal(vals$cost, vals$revenue, vals$cashbal)
        uploadValues(vals$day, vals$cashbal, vals$cost, vals$revenue, vals$playerid)
        
        # Update demand forecast for next day
        vals$demand_forecast <- c(forecastB1[vals$day+1], forecastB2[vals$day+1], forecastB3[vals$day+1], forecastB4[vals$day+1], forecastB5[vals$day+1], forecastB6[vals$day+1])
        
        # Close the modal after saving
        removeModal()
      } else {
        print("Not enough flowers")
      }
    } else {
      print("Not a integer value")
    }
    #values$makeB1 <- ifelse(input$B1choice == "", 0, input$B1choice)
    #values$makeB2 <- ifelse(input$B2choice == "", 0, input$B2choice)
    #values$makeB3 <- ifelse(input$B3choice == "", 0, input$B3choice)
    #values$makeB4 <- ifelse(input$B4choice == "", 0, input$B4choice)
    #values$makeB5 <- ifelse(input$B5choice == "", 0, input$B5choice)
    #values$makeB6 <- ifelse(input$B6choice == "", 0, input$B6choice)
    
   

  })
  
  #calendar menu prompt
  shinyjs::onclick("month_fc", showModal(month_fcModal()))
  shinyjs::onclick("next_fc", showModal(next_fcModal()))
  shinyjs::onclick("cal_button", showModal(calendarModal()))
  
  output$nextday_B1 <- renderText({
    paste("Bouquet 1:", vals$demand_forecast[1])
  })
  output$nextday_B2 <- renderText({
    paste("Bouquet 2:", vals$demand_forecast[2])
  })
  output$nextday_B3 <- renderText({
    paste("Bouquet 3:", vals$demand_forecast[3])
  })
  output$nextday_B4 <- renderText({
    paste("Bouquet 4:", vals$demand_forecast[4])
  })
  output$nextday_B5 <- renderText({
    paste("Bouquet 5:", vals$demand_forecast[5])
  })
  output$nextday_B6 <- renderText({
    paste("Bouquet 6:", vals$demand_forecast[6])
  })
  output$nextday_modal_title <- renderText({
    paste("Tomorrow's Demand")
  })
  ### Number output for Inventory Modal ###
  #output text for the diff values
  output$B1Exp <- renderText({
    vals$bouquet_exp[1]
  })
  output$B2Exp <- renderText({
    vals$bouquet_exp[2]
  })
  output$B3Exp <- renderText({
    vals$bouquet_exp[3]
  })
  output$B4Exp <- renderText({
    vals$bouquet_exp[4]
  })
  output$B5Exp <- renderText({
    vals$bouquet_exp[5]
  })
  output$B6Exp <- renderText({
    vals$bouquet_exp[6]
  })
  output$roseLeft <- renderText({
    vals$flowers_left[1]
  })
  output$babyLeft <- renderText({
    vals$flowers_left[2]
  })
  output$carnLeft <- renderText({
    vals$flowers_left[3]
  })
  output$roseExp <- renderText({
    vals$flowers_exp[1]
  })
  output$babyExp <- renderText({
    vals$flowers_exp[2]
  })
  output$carnExp <- renderText({
    vals$flowers_exp[3]
  })
  
  ### Table output for End Game Modal ###
  output$fulfilledOutput <- renderText({
    paste(vals$orders_fulfilled, "/", vals$total_orders)
  })
  
  flower_prefix <- c("R", "B", "C")
  
  matrix_fexp <- reactive({
    paste_vals <- paste(flower_prefix, ":", vals$flowers_exp)
    matrix(paste_vals, nrow = 1, byrow = TRUE)
  })
  
  matrix_fleft <- reactive({
    paste_vals <- paste(flower_prefix, ":", vals$flowers_left)
    matrix(paste_vals, nrow = 1, byrow = TRUE)
  })
  
  observeEvent(input$endday_btn, {
    r_order <- as.integer(input$r_order)
    c_order <- as.integer(input$c_order)
    b_order <- as.integer(input$b_order)
    staff_hire <- as.integer(input$staff_hire)
    staff_fire <- as.integer(input$staff_fire)
    
    if(is.numeric(r_order) || is.numeric(c_order) || is.numeric(b_order) || is.numeric(staff_fire) || is.numeric(staff_hire)){ #need fix
      if(min(staff_fire, staff_hire) == 0){
        staff_total <- as.numeric(vals$manpower) + staff_hire - staff_fire
        enddaycost <- r_order*1 + c_order*0.5 + b_order*0.1 + staff_total*10
        if(enddaycost > as.numeric(vals$cashbal)){
          print("Not enough money to do purchase.")
        } else {
          vals$manpower <- as.numeric(vals$manpower) + staff_hire-staff_fire
          eodOrdered <- updateEodOrdered(vals$day, eodOrdered, r_order, c_order, b_order, vals$manpower)
          flowersInventory <- updateFlowersInvStart(vals$day, flowersInventory, eodOrdered)
          vals$cost <- calculateCost(vals$day, eodOrdered)
          vals$day <- vals$day+1
          actdemand <- getDemandEachDay(vals$day, vals$playerid)
          vals$actual_demand <- c(actdemand[[1]], actdemand[[2]], actdemand[[3]], actdemand[[4]], actdemand[[5]], actdemand[[6]])
          vals$flowers_left <- c(eodOrdered[vals$day, "F1"], eodOrdered[vals$day, "F2"], eodOrdered[vals$day, "F3"], "")
          vals$flowers_exp <- c(flowersInventory[vals$day+1, "F1e"], flowersInventory[vals$day+1, "F2e"], flowersInventory[vals$day+1, "F3e"], "")
          print(eodOrdered)
          print(flowersInventory)
          # Close the modal after saving
          removeModal()
          showModal(startgameModal())
        }
      } else {
        print("You cannot fire and hire at the same time") #need render?
      }
    } else {
      print("Please input integers only") #need render?
    }
    
  })
  
  # Render the table output
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
  output$b1_ec_input <- renderText({
    paste("B1:")
  })
  output$b2_ec_input <- renderText({
    "B2:"
  })
  output$b3_ec_input <- renderText({
    "B3:"
  })
  output$b4_ec_input <- renderText({
    "B4:"
  })
  output$b5_ec_input <- renderText({
    "B5:"
  })
  output$b6_ec_input <- renderText({
    "B6:"
  })
  
  ### Number Output for Order Fulfilment Modal ###      
  output$order_fulfilmentTitle <- renderText({paste("Order Fulfilment")})   
  output$bouquet1fulfilled <- renderText({
    paste("Bouquet 1:", vals$orders_fulfilled[1], "/", vals$actual_demand[1])
    })   
  output$bouquet2fulfilled <- renderText({
    paste("Bouquet 2:", vals$orders_fulfilled[2], "/", vals$actual_demand[2])
    })   
  output$bouquet3fulfilled <- renderText({
    paste("Bouquet 3:", vals$orders_fulfilled[3], "/", vals$actual_demand[3])
    })   
  output$bouquet4fulfilled <- renderText({
    paste("Bouquet 4:", vals$orders_fulfilled[4], "/", vals$actual_demand[4])
    })   
  output$bouquet5fulfilled <- renderText({
    paste("Bouquet 5:", vals$orders_fulfilled[5], "/", vals$actual_demand[5])
    })   
  output$bouquet6fulfilled <- renderText({
    paste("Bouquet 6:", vals$orders_fulfilled[6], "/", vals$actual_demand[6])
    })
  
  # Create matrix for leaderboard    
  matrix_leaderboard <- matrix(     
    paste(player_name, ":", c(player_score)),     
    nrow = length(player_name), byrow = TRUE)      
  # Render Leaderboard Table   
  output$score_leaderboardTitle <- renderText({
    paste("LeaderBoard")
  })
  output$scoreLeaderboard <- renderTable({
    matrix_leaderboard}, include.rownames = FALSE, include.colnames = FALSE)
  
  output$cashBal <- renderText({
    paste(vals$cashbal)
  })
  
}