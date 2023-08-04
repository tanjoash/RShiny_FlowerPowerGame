## Server logic ##
server <- function(input, output, session){
  
  ## First page logic ## #Joash
  source("logic/instructionsManager.R")
  source("logic/demandGeneration.R")
  source("logic/createInventory.R")
  source("logic/endDayCalculations.R")
  
  # Reactive Values objects for storing objects and values #Joash
  vals <- reactiveValues(userid = NULL,
                         username = NULL,
                         playerid = NULL,
                         day = 0,
                         cost = 0,
                         profit = 0,
                         revenue = 0,
                         manpower = 0,
                         capacity = 5,
                         cashbal = 500,
                         startday = FALSE,
                         orders_fulfilled = c(0, 0, 0, 0, 0, 0),
                         total_orders = 0,
                         bouquet_left = c(0, 0, 0, 0, 0, 0),
                         bouquet_exp = c(0, 0, 0, 0, 0, 0), # need to linkt o database
                         flowers_left = c(0, 0, 0),
                         flowers_exp = c(0, 0, 0),
                         demand_forecast = c(0, 0, 0, 0, 0, 0),
                         calculator_vals = c(0, 0, 0),
                         actual_demand = c(0, 0, 0, 0, 0, 0),
                         eodOrdered = NULL,
                         bouquetsInventory = NULL,
                         flowersInventory = NULL,
                         ordersFulfilled = NULL,
                         leaderboardName = c("", "", "", "", ""),
                         leaderboardScore = c(0, 0, 0, 0, 0),
                         df_revenue = NULL,
                         df_cost = NULL,
                         df_cashbal = NULL,
                         order_price = 0
                         )
  
  ## Login logic ## #Joash
  output$username <- renderText({
    if(is.null(vals$username)){
      paste("Please Login or Register")
    } else {
      paste("Welcome", vals$username)
    }
  })
  
  #Joash
  observeEvent(input$gotoregister, {
    showModal(registerModal())
  })
  
  #Joash
  observeEvent(input$login, {
    showModal(loginModal())
  })
  
  # Login Modal #Joash
  observeEvent(input$loginok, {
    if(is.na(input$usernameInput) || input$usernameInput == '' || is.na(input$password3) || input$password3 == ''){
      showModal(loginModal(empty = TRUE, failed = FALSE))
    } else {
      userid <- getuserid(input$usernameInput, input$password3)
      if(userid>0){
        vals$userid <- userid
        vals$username <- input$usernameInput
        showModal(successModal("Login Successful!"))
      } else {
        showModal(loginModal(empty = FALSE, failed = TRUE))
      }
    }
    
  })
  
  ## Register Logic ## #Joash
  observeEvent(input$register, {
    showModal(registerModal())
  })
  
  # Register Modal #Joash
  observeEvent(input$registerok, {
    users <- uniqueUsers()
    if (is.na(input$usernameInput) || input$usernameInput == ''){
      showModal(registerModal(empty = TRUE, repetition = FALSE, validatepw = FALSE))
    } else {
      users <- uniqueUsers()
      if(input$usernameInput %in% users$username){
        showModal(registerModal(empty = FALSE, repetition = TRUE, validatepw = FALSE))
      } else {
        if(is.na(input$password1) || is.na(input$password2) || input$password1 == '' || input$password2 == '' || (input$password1 != input$password2)){
          showModal(registerModal(empty = FALSE, repetition = FALSE, validatepw = TRUE))
        } else {
          registerPlayer(input$usernameInput, input$password1)
          showModal(successModal("Successfully registered! You may proceed to Login."))
        }
      }
    }
  })
  
  # Render Logout if logged in #Joash
  output$loginPlaceholder <- renderUI({
    if(is.null(vals$userid)){
      actionButton("login", "Login")
    } else {
      actionButton("logout", "Logout")
    }
  })
  
  # logout #Joash
  observeEvent(input$logout, {
    vals$userid <- NULL
    vals$username <- NULL
  })
  
  # Play button appears if logged in #Joash
  output$playButtonPlaceholder <- renderUI({
    if(vals$userid > 0){
      actionButton("playButton", "Play")
    }
  })
  
  # Click image in second page logics #Bing Zhe and Marc
  shinyjs::onclick("cashbal_btn", {
    result <- RevCostCash(as.numeric(vals$playerid))
    vals$df_revenue <- result[1,2:33]
    vals$df_cost <- result[1,35:66]
    vals$df_cashbal <- result[1,68:99]
    showModal(cash_balmenuModal())
    })
  shinyjs::onclick("cash_bal", showModal(cash_balModal()))
  shinyjs::onclick("cost", showModal(costModal()))
  shinyjs::onclick("revenue", showModal(revenueModal()))
  shinyjs::onclick("back_btn_cb", {showModal(cash_balmenuModal())})
  
  # Render plots #Jeanelle
  output$cashbal_plot <- renderPlotly({
    plot_ly(x = seq(0,vals$day,1), y = unlist(vals$df_cashbal[,1:(vals$day+1)]), type = "scatter", mode = "lines") %>%
      layout(xaxis = list(title = "Day", tickvals = seq(0,vals$day,1), tickmode="array", ticktext=as.character(seq(0,vals$day,1))), yaxis = list(title = "Cash Balance"))
  })
  output$revenue_plot <- renderPlotly({
    plot_ly(x = seq(0,vals$day,1), y = unlist(vals$df_revenue[,1:(vals$day+1)]), type = "scatter", mode = "lines") %>%
      layout(xaxis = list(title = "Day", tickvals = seq(0,vals$day,1), tickmode="array", ticktext=as.character(seq(0,vals$day,1))), yaxis = list(title = "Revenue"))
  })
  output$cost_plot <- renderPlotly({
    plot_ly(x = seq(0,vals$day,1), y = unlist(vals$df_cost[,1:(vals$day+1)]), type = "scatter", mode = "lines") %>%
      layout(xaxis = list(title = "Day", tickvals = seq(0,vals$day,1), tickmode="array", ticktext=as.character(seq(0,vals$day,1))), yaxis = list(title = "Cost"))
  })
  
  # Date output #Bing Zhe
  output$dateofmay <- renderText({
    vals$day
  })

  ## Instructions ##
  # Instruction Modal #Bing Zhe
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
  
  # Render instructions images #Bing Zhe
  updatePageContent <- function() {
    output$page_content <- renderUI({
      switch(instructPage(),
             "1" = img(id="instruct_1", src="assets/instructions 1.png", width = "570px"),
             "2" = img(id="instruct_2", src="assets/instructions 2.png", width = "570px"),
             "3" = img(id="instruct_3", src="assets/instructions 3.png", width = "570px"),
             "4" = img(id="instruct_4", src="assets/instructions 4.png", width = "570px"),
             "5" = img(id="instruct_5", src="assets/instructions 5.png", width = "570px"),
             "6" = img(id="instruct_6", src="assets/instructions 6.png", width = "570px")
      )
    })
  }
  
  # Get Seed #Joash and Jeanelle
  observeEvent(input$playButton, {
    vals$playerid <- playerID() # need to change and get latest playerid
    vals$demand_forecast <- c(forecastB1[vals$day+1], forecastB2[vals$day+1], forecastB3[vals$day+1], forecastB4[vals$day+1], forecastB5[vals$day+1], forecastB6[vals$day+1])
    vals$eodOrdered <- createEodOrdered()
    vals$bouquetsInventory <- createBouquetsInventory()
    vals$flowersInventory <- createFlowersInventory()
    vals$ordersFulfilled <- createOrdersFulfilled()
    regexNumber <- "^[0-9]+$"
    if (grepl(regexNumber, input$seedNumber)) { 
      setSeed(vals$username, as.integer(vals$userid), as.integer(input$seedNumber))
      updateTabsetPanel(session, "flowerPages", "SecondPage")
      showModal(enddayModal())
    } else if (is.na(input$seedNumber) || input$seedNumber == '') {
      setSeed(vals$username, as.integer(vals$userid))
      updateTabsetPanel(session, "flowerPages", "SecondPage")
      showModal(enddayModal())
    } else {
      showModal(errorModal("Not a Positive Integer."))
    }

    ### Number Output for Start Game Modal ###
    # Render Inventory for Start Game Modal #Marc
    output$numberofstaff <- renderText({vals$capacity})
    
    # Render Bouquet left #Marc
    output$B1Left_start <- renderText({vals$bouquet_left[[1]]})
    output$B2Left_start <- renderText({vals$bouquet_left[[2]]})
    output$B3Left_start <- renderText({vals$bouquet_left[[3]]})
    output$B4Left_start <- renderText({vals$bouquet_left[[4]]})
    output$B5Left_start <- renderText({vals$bouquet_left[[5]]})
    output$B6Left_start <- renderText({vals$bouquet_left[[6]]})

    # Render Flowers left #Marc
    output$roseLeft_start <- renderText({vals$flowers_left[[1]]})
    output$carnLeft_start <- renderText({vals$flowers_left[[2]]})
    output$babyLeft_start <- renderText({vals$flowers_left[[3]]})

    # Render Flowers expiring #Marc
    output$roseExp_start <- renderText({vals$flowers_exp[[1]]})
    output$carnExp_start <- renderText({vals$flowers_exp[[2]]})
    output$babyExp_start <- renderText({vals$flowers_exp[[3]]})

    
    # Render Demand Forecast for Start Game Modal #Marc
    output$B1demandforecast_start <- renderText({vals$demand_forecast[[1]]})
    output$B2demandforecast_start <- renderText({vals$demand_forecast[[2]]})
    output$B3demandforecast_start <- renderText({vals$demand_forecast[[3]]})
    output$B4demandforecast_start <- renderText({vals$demand_forecast[[4]]})
    output$B5demandforecast_start <- renderText({vals$demand_forecast[[5]]})
    output$B6demandforecast_start <- renderText({vals$demand_forecast[[6]]})
    
    # Render Calculator #Bing Zhe
    output$R <- renderText({paste("Roses Required:", vals$calculator_vals[1])})
    output$C <- renderText({paste("Carnations Required:",vals$calculator_vals[2])})
    output$B <- renderText({paste("Baby Breaths Required:", vals$calculator_vals[3])})
      
  })
  
  # 2nd set of Buttons logic #Bing Zhe
  shinyjs::onclick("dayButton", showModal(cal_menuModal()))
  shinyjs::onclick("inventoryButton", showModal(inventoryModal()))
  shinyjs::onclick("endDay", {
    if(vals$cashbal < 0.50 && vals$flowers_left[1] < 3 && vals$flowers_left[2] < 3 && vals$flowers_left[3] < 5){
      result <- displayLeaderboard()
      vals$leaderboardName <- c(result[,1])
      vals$leaderboardScore <- c(result[,2])
      showModal(gameoverModal())
    } else {
      if(vals$day != 31){
        showModal(enddayModal())
      } else {
        result <- displayLeaderboard()
        vals$leaderboardName <- c(result[,1])
        vals$leaderboardScore <- c(result[,2])
        showModal(gamefinishModal())
      }
    }
  })
  shinyjs::onclick("orderButton", showModal(order_fulfilmentModal()))
  shinyjs::onclick("leaderboard_btn", {
    result <- displayLeaderboard()
    vals$leaderboardName <- c(result[,1])
    vals$leaderboardScore <- c(result[,2])
    showModal(score_leaderboardModal())
  })
  
  # Calculator Observe Event #Bing Zhe
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
  
  # Start day button logic and math #Joash
  observeEvent(input$startday_btn, {
    # Get the values from the textInputs
    B1_make <- as.integer(input$B1choice)
    B2_make <- as.integer(input$B2choice)
    B3_make <- as.integer(input$B3choice)
    B4_make <- as.integer(input$B4choice)
    B5_make <- as.integer(input$B5choice)
    B6_make <- as.integer(input$B6choice)
    
    # check if numeric #Joash
    if(is.numeric(B1_make) && is.numeric(B2_make) && is.numeric(B3_make) && is.numeric(B4_make) && is.numeric(B5_make) && is.numeric(B6_make)){
      
      # Calculate total flowers available #Joash
      total_f1 <- as.numeric(vals$flowers_left[1]) + as.numeric(vals$flowers_exp[1])
      total_f2 <- as.numeric(vals$flowers_left[2]) + as.numeric(vals$flowers_exp[2])
      total_f3 <- as.numeric(vals$flowers_left[3]) + as.numeric(vals$flowers_exp[3])
      
      # Calculate required flowers to make bouquets #Joash
      used_f1 <- 3*as.numeric(B1_make) + 3*as.numeric(B2_make) + 5*as.numeric(B4_make)
      used_f2 <- 3*as.numeric(B1_make) + 3*as.numeric(B3_make) + 5*as.numeric(B5_make)
      used_f3 <- 5*as.numeric(B2_make) + 5*as.numeric(B3_make) + 10*as.numeric(B6_make)
      
      # Calculate total bouquets made #Joash
      total_bouquets_made <- B1_make + B2_make + B3_make + B4_make + B5_make + B6_make
      
      # Ensure that total bouquest is less than the capacity to maek the bouquets #Joash
      if(total_bouquets_made <= as.numeric(vals$capacity)){
        # Ensure that the flowers used are less than or equal to the total flower available #Joash
        if(used_f1 <= total_f1 && used_f2 <= total_f2 && used_f3 <= total_f3){
          # if used flowers is less than the expiring ones to throw today #Joash
          if(used_f1 <= as.numeric(vals$flowers_exp[1])){ 
            vals$flowers_exp[1] <- as.numeric(vals$flowers_exp[1]) - used_f1 #to throw away today
          } else { 
            vals$flowers_left[1] <- as.numeric(vals$flowers_left[1]) - (used_f1 - as.numeric(vals$flowers_exp[1]))
            vals$flowers_exp[1] <- 0
          }
          
          # if used flowers is less than the expiring ones to throw today
          if(used_f2 <= as.numeric(vals$flowers_exp[2])){ 
            vals$flowers_exp[2] <- as.numeric(vals$flowers_exp[2]) - used_f2 #to throw away today
          } else { 
            vals$flowers_left[2] <- as.numeric(vals$flowers_left[2]) - (used_f2 - as.numeric(vals$flowers_exp[2]))
            vals$flowers_exp[2] <- 0
          }
          
          # if used flowers is less than the expiring ones to throw today
          if(used_f3 <= as.numeric(vals$flowers_exp[3])){ 
            vals$flowers_exp[3] <- as.numeric(vals$flowers_exp[3]) - used_f3 #to throw away today
          } else { 
            vals$flowers_left[3] <- as.numeric(vals$flowers_left[3]) - (used_f3 - as.numeric(vals$flowers_exp[3]))
            vals$flowers_exp[3] <- 0
          }
          
          # Reset calculator values #Joash
          vals$calculator_vals <- c(0, 0, 0)
          # Update local dataframe #Jeanelle
          vals$bouquetsInventory <- updateBouquetsMade(vals$day, vals$bouquetsInventory, B1_make, B2_make, B3_make, B4_make, B5_make, B6_make)
          vals$flowersInventory <- updateFlowersUsed(vals$day, vals$flowersInventory, as.numeric(vals$flowers_left[1]),
                                                     as.numeric(vals$flowers_left[2]),
                                                     as.numeric(vals$flowers_left[3]),
                                                     as.numeric(vals$flowers_exp[1]),
                                                     as.numeric(vals$flowers_exp[2]),
                                                     as.numeric(vals$flowers_exp[3]))
          
          # Calculate orders fulfilled #Jeanelle
          vals$orders_fulfilled <- c(min(B1_make, as.numeric(vals$actual_demand[1])),
                                     min(B2_make, as.numeric(vals$actual_demand[2])),
                                     min(B3_make, as.numeric(vals$actual_demand[3])),
                                     min(B4_make, as.numeric(vals$actual_demand[4])),
                                     min(B5_make, as.numeric(vals$actual_demand[5])),
                                     min(B6_make, as.numeric(vals$actual_demand[6])))
          
          # Bouquets that were thrown away #Jeanelle
          vals$bouquet_exp <- c((B1_make - as.numeric(vals$orders_fulfilled[1])),
                                (B2_make - as.numeric(vals$orders_fulfilled[2])),
                                (B3_make - as.numeric(vals$orders_fulfilled[3])),
                                (B4_make - as.numeric(vals$orders_fulfilled[4])),
                                (B5_make - as.numeric(vals$orders_fulfilled[5])),
                                (B6_make - as.numeric(vals$orders_fulfilled[6])))
          
          # Update local data #Jeanelle
          vals$ordersFulfilled <- updateOrdersFulfilled(vals$day, vals$actual_demand, as.numeric(vals$orders_fulfilled[1]),
                                                        as.numeric(vals$orders_fulfilled[2]),
                                                        as.numeric(vals$orders_fulfilled[3]),
                                                        as.numeric(vals$orders_fulfilled[4]),
                                                        as.numeric(vals$orders_fulfilled[5]),
                                                        as.numeric(vals$orders_fulfilled[6]),
                                                        vals$ordersFulfilled)
          # Update reactive values #Joash
          vals$revenue <- calculateRevenue(vals$day, vals$ordersFulfilled)
          vals$cashbal <- calculateCashBal(vals$cost, vals$revenue, vals$cashbal)
          uploadValues(vals$day, vals$cashbal, vals$cost, vals$revenue, vals$playerid)
          vals$profit <- vals$revenue - vals$cost
          
          
          # Update demand forecast for next day #Joash
          vals$demand_forecast <- c(forecastB1[vals$day+1], forecastB2[vals$day+1], forecastB3[vals$day+1], forecastB4[vals$day+1], forecastB5[vals$day+1], forecastB6[vals$day+1])
            
          # Close the modal after saving
          removeModal()
        } else {
          showModal(errorModal("Not enough flowers to make bouquets.", startday = TRUE))
        }
      } else {
        showModal(errorModal("Not enough manpower to make bouquets.", startday = TRUE))
      }
    } else {
      showModal(errorModal("Input was not an integer value.", startday = TRUE))
    }
  })
  
  # Open Startday modal again to prevent soft locking of the game #Joash
  observeEvent(input$startdayagain, {
    removeModal()
    showModal(startgameModal())
  })
  
  # Render Calendar menu and Logic prompt #Marc and Bing Zhe
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
  
  # Render the forecasted demand #Jeanelle
  output$line_chart <- renderPlotly({
    plot_ly(x = forecastday, y = forecastB1, type = "scatter", mode = "lines") %>%
      layout(xaxis = list(title = "Forecast Day"), yaxis = list(title = "Forecast B1"))
  })
  observeEvent(input$fc_B1, {
    output$line_chart <- renderPlotly({
      plot_ly(x = forecastday, y = forecastB1, type = "scatter", mode = "lines") %>%
        layout(xaxis = list(title = "Forecast Day"), yaxis = list(title = "Forecast B1"))
    })
  })
  observeEvent(input$fc_B2, {
    output$line_chart <- renderPlotly({
      plot_ly(x = forecastday, y = forecastB2, type = "scatter", mode = "lines") %>%
        layout(xaxis = list(title = "Forecast Day"), yaxis = list(title = "Forecast B2"))
    })
  })
  observeEvent(input$fc_B3, {
    output$line_chart <- renderPlotly({
      plot_ly(x = forecastday, y = forecastB3, type = "scatter", mode = "lines") %>%
        layout(xaxis = list(title = "Forecast Day"), yaxis = list(title = "Forecast B3"))
    })
  })
  observeEvent(input$fc_B4, {
    output$line_chart <- renderPlotly({
      plot_ly(x = forecastday, y = forecastB4, type = "scatter", mode = "lines") %>%
        layout(xaxis = list(title = "Forecast Day"), yaxis = list(title = "Forecast B4"))
    })
  })
  observeEvent(input$fc_B5, {
    output$line_chart <- renderPlotly({
      plot_ly(x = forecastday, y = forecastB5, type = "scatter", mode = "lines") %>%
        layout(xaxis = list(title = "Forecast Day"), yaxis = list(title = "Forecast B5"))
    })
  })
  observeEvent(input$fc_B6, {
    output$line_chart <- renderPlotly({
      plot_ly(x = forecastday, y = forecastB6, type = "scatter", mode = "lines") %>%
        layout(xaxis = list(title = "Forecast Day"), yaxis = list(title = "Forecast B6"))
    })
  })
  
  # Render text for Inventory Modal #Marc
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
  output$carnLeft <- renderText({
    vals$flowers_left[2]
  })
  output$babyLeft <- renderText({
    vals$flowers_left[3]
  })

  output$roseExp <- renderText({
    vals$flowers_exp[1]
  })
  output$carnExp <- renderText({
    vals$flowers_exp[2]
  })
  output$babyExp <- renderText({
    vals$flowers_exp[3]
  })

  
  # Table output for End Game Modal #Bing Zhe
  output$fulfilledOutput <- renderText({
    paste(sum(vals$orders_fulfilled), "/", vals$total_orders)
  })
  
  flower_prefix <- c("R", "C", "B")
  
  matrix_fexp <- reactive({
    paste_vals <- paste(flower_prefix, ":", vals$flowers_exp)
    matrix(paste_vals, nrow = 1, byrow = TRUE)
  })
  
  matrix_fleft <- reactive({
    paste_vals <- paste(flower_prefix, ":", vals$flowers_left)
    matrix(paste_vals, nrow = 1, byrow = TRUE)
  })
  
  # Logic of end day button #Joash
  observeEvent(input$endday_btn, {
    # Get input values #Joash
    r_order <- as.integer(input$r_order)
    c_order <- as.integer(input$c_order)
    b_order <- as.integer(input$b_order)
    staff_hire <- as.integer(input$staff_hire)
    staff_fire <- as.integer(input$staff_fire)
      # Check if numeric #Joash
      if(is.numeric(r_order) && is.numeric(c_order) && is.numeric(b_order) && is.numeric(staff_fire) && is.numeric(staff_hire)){
        # Check if fire and hire at same time #Josah
        if(min(staff_fire, staff_hire) == 0){
          # Ensure that manpower does not go negative #Joash
          if((as.numeric(vals$manpower + staff_hire - staff_fire) >= 0)){
            # Update vals #Joash
            staff_total <- as.numeric(vals$manpower) + staff_hire - staff_fire
            enddaycost <- r_order*1 + c_order*0.5 + b_order*0.1 + staff_total*10
            
            # Fixed some bug of not being able to purchase and ensures that you cannot buy when
            # you dont have enough money #Joash
            if(enddaycost > (vals$cashbal+0.01)){
              showModal(errorModal("Not enough money to do purchase."))
            } else {
              # Updating of values #Jeanelle
              vals$manpower <- as.numeric(vals$manpower) + staff_hire - staff_fire
              vals$capacity <- as.numeric(vals$manpower)*5 + 5
              vals$eodOrdered <- updateEodOrdered(vals$day, vals$eodOrdered, r_order, c_order, b_order, vals$manpower)
              vals$flowersInventory <- updateFlowersInvStart(vals$day, vals$flowersInventory, vals$eodOrdered)
              vals$cost <- calculateCost(vals$day, vals$eodOrdered)
              vals$day <- vals$day+1
              actdemand <- getDemandEachDay(vals$day, vals$playerid)
              vals$actual_demand <- c(actdemand[[1]], actdemand[[2]], actdemand[[3]], actdemand[[4]], actdemand[[5]], actdemand[[6]])
              vals$total_orders <- sum(as.numeric(vals$actual_demand))
              vals$flowers_left <- c(vals$eodOrdered[vals$day, "F1"], vals$eodOrdered[vals$day, "F2"], vals$eodOrdered[vals$day, "F3"])
              vals$flowers_exp <- c(vals$flowersInventory[vals$day+1, "F1e"], vals$flowersInventory[vals$day+1, "F2e"], vals$flowersInventory[vals$day+1, "F3e"])
              #print("eodOrdered")
              #print(vals$eodOrdered)
              #print("flowers Inventory")
              #print(vals$flowersInventory)
            # Close the modal after saving
            removeModal()
            showModal(startgameModal())
          }
        } else {
          showModal(errorModal("You cannot fire more than the number of people you have"))
        }
      } else {
        showModal(errorModal("You cannot fire and hire at the same time"))
      }
    } else {
      showModal(errorModal("Please input integers only"))
        }
  })
  
  # Render the table output #Bing Zhe
  output$flowExpOutput <- renderTable({
    matrix_fexp()
  }, include.rownames = FALSE, include.colnames = FALSE)
  output$flowLeftOutput <- renderTable({
    matrix_fleft()
  }, include.rownames = FALSE, include.colnames = FALSE)
  
  output$dailyRevenue <- renderText({
    paste("$", vals$revenue)
  })
  output$dailyCost <- renderText({
    paste("$", vals$cost)
  })
  output$dailyProfit <- renderText({
    paste("$", vals$profit)
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
  output$endday_nfc1 <- renderText({
    paste("B1:", vals$demand_forecast[1])
  })
  output$endday_nfc2 <- renderText({
    paste("B2:", vals$demand_forecast[2])
  })
  output$endday_nfc3 <- renderText({
    paste("B3:", vals$demand_forecast[3])
  })
  output$endday_nfc4 <- renderText({
    paste("B4:", vals$demand_forecast[4])
  })
  output$endday_nfc5 <- renderText({
    paste("B5:", vals$demand_forecast[5])
  })
  output$endday_nfc6 <- renderText({
    paste("B6:", vals$demand_forecast[6])
  })
  output$staffno <- renderText({vals$manpower})
  
  # Reactive price calculator #Bing Zhe
  observeEvent({input$r_order
    input$c_order
    input$b_order
    input$staff_hire
    input$staff_fire}, {
    vals$order_price <- as.numeric(input$r_order)+as.numeric(input$c_order)*0.5+as.numeric(input$b_order)*0.1+as.numeric(vals$manpower)*10+as.numeric(input$staff_hire)*10-as.numeric(input$staff_fire)*10
  })
  
  output$pricecalc <- renderText({paste("Price: $", vals$order_price)})
  
  # Number Output for Order Fulfilment Modal #Bing Zhe     
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
  matrix_fleft <- reactive({
    paste_vals <- paste(flower_prefix, ":", vals$flowers_left)
    matrix(paste_vals, nrow = 1, byrow = TRUE)
  })
  
  # Create matrix for leaderboard #Marc  
  rank_prefix <- c("👑","2️⃣","3️⃣","4️⃣","5️⃣")
  leaderboard_data <- reactive({
    data.frame(
      Rank = rank_prefix,
      Name = vals$leaderboardName[1:5],
      Score = vals$leaderboardScore[1:5]
    ) 
  })  
  # Render Leaderboard Table #Bing Zhe
  output$score_leaderboardTitle <- renderText({
    paste("LeaderBoard 🎉 ") }) 
  output$scoreLeaderboard <- renderTable({   
    leaderboard_data()}, include.rownames = FALSE, include.colnames = TRUE) 
  
  # Render Cash Balance #Bing Zhe
  output$cashBal <- renderText({
    round(vals$cashbal, digits = 2)
  })
  
  #Game over modal #Joash
  output$gameover_title <- renderText({"Game Over!"})
  output$gameover_text <- renderText({"You have not enough money and flowers to continue playing. Good luck next time!"})
  
  #Finish game modal #Bing Zhe
  output$finish_title <- renderText({"Congratulations!"})
  output$finalstat_title <- renderText({"Final day stats:"})
  output$final_cashbal <- renderText({ paste("Cash Balance & Final Score: ", vals$cashbal)})
  output$final_manpower <-renderText({paste("Number of Staff: ", vals$manpower)})
  output$final_orders_fulfilled <- renderText({paste("Orders Fulfilled: ", sum(vals$order_fulfilled), "/", vals$total_orders)})
  output$final_fleft_title <- renderText({"Flowers Left:"})
  output$flowLeftfinish <- renderTable({
    matrix_fleft()
  }, include.rownames = FALSE, include.colnames = FALSE)
  output$final_leaderboard_title <- renderText({"Leaderboard 🎉"})
  output$final_leaderboard <- renderTable({
    leaderboard_data()}, include.rownames = FALSE, include.colnames = TRUE)
  output$thanks <-renderText({"Thank You For Playing!"})
  
  #button to close/refresh the game #Bing Zhe
  observeEvent(input$restartGame, {
    session$reload()
  })
  
  #Close app #Joash
  observeEvent(input$byebye, {
    stopApp()
  })
}