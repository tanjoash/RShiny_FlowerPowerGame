## Modal for instructions ##
instructionsModal <- function() {
  modalDialog(
    title = "Instructions"
  )
}
nameError <- function(){
  modalDialog(
    title = "Error!",
    "Player name is empty!"
  )
}

cal_menuModal <- function() {
  modalDialog(
    size = "l",
    div(
      id = "cal-menu-modal",
      img(src = "assets/Forecast Demand wo button.png"),
      actionButton("month_fc", "Monthly Forecast"),
      actionButton("cal_button", "Calendar"),
      actionButton("next_fc", "Next Day Forecast"),
    )
  )
}

month_fcModal <- function(){
  modalDialog(
    id = "month-fc-modal",
    # img(src = "insert graph here")
  )
}
next_fcModal <- function(){
  modalDialog(
    div(
      id = "next-fc-modal",
      textOutput("nextday_modal_title"),
      textOutput("nextday_B1"),
      textOutput("nextday_B2"),
      textOutput("nextday_B3"),
      textOutput("nextday_B4"),
      textOutput("nextday_B5"),
      textOutput("nextday_B6")
    )
  )
}

calendarModal <- function() {
  modalDialog(
    size = "l",
    div(
      id = "calendar-modal",
      tags$img(src = "assets/calendar prompt font changed.png"),
      # actionButton("May_1", ""),
      # actionButton("May_2", ""),
      # actionButton("May_3", ""),
      # actionButton("May_4", ""),
      # actionButton("May_5", ""),
      # actionButton("May_6", ""),
      # actionButton("May_7", ""),
      # actionButton("May_8", ""),
      # actionButton("May_9", ""),
      # actionButton("May_10", ""),
      # actionButton("May_11", ""),
      # actionButton("May_12", ""),
      # actionButton("May_13", ""),
      # actionButton("mothersday", ""),
      # actionButton("May_15", ""),
      # actionButton("May_16", ""),
      # actionButton("May_17", ""),
      # actionButton("May_18", ""),
      # actionButton("May_19", ""),
      # actionButton("wuerling", ""),
      # actionButton("May_21", ""),
      # actionButton("May_22", ""),
      # actionButton("May_23", ""),
      # actionButton("May_24", ""),
      # actionButton("May_25", ""),
      # actionButton("May_26", ""),
      # actionButton("May_27", ""),
      # actionButton("May_28", ""),
      # actionButton("May_29", ""),
      # actionButton("May_30", ""),
      # actionButton("May_31", ""),
    )
  )
}

inventoryModal <- function(){
  modalDialog(
    size = "l",
    div(
      id = "inventory-modal",
      tags$img(src = "assets/photo_2023-07-19_12-39-09.jpg"),
      
      ##inventory number output
      textOutput("B1Left"),
      textOutput("B2Left"),
      textOutput("B3Left"),
      textOutput("B4Left"),
      textOutput("B5Left"),
      textOutput("B6Left"),
      textOutput("B1Exp"),
      textOutput("B2Exp"),
      textOutput("B3Exp"),
      textOutput("B4Exp"),
      textOutput("B5Exp"),
      textOutput("B6Exp"),
      textOutput("roseLeft"),
      textOutput("babyLeft"),
      textOutput("carnLeft"),
      textOutput("roseExp"),
      textOutput("babyExp"),
      textOutput("carnExp"),
    )
  )
}
startgameModal <- function() {   
  modalDialog(     
    title = "Start Game Modal",     
    div(       
      id = "start-game-modal",       
      img(src = "assets/Start-day prompt.png"),
      
      ## user selection for number of flowers for the day
      textInput("B1choice", "" , 0),       
      textInput("B2choice", "", 0),       
      textInput("B3choice", "", 0),       
      textInput("B4choice", "", 0),       
      textInput("B5choice", "", 0),       
      textInput("B6choice", "", 0),
      
      ##demand forecast number output 
      textOutput("B1demandforecast_start"),
      textOutput("B2demandforecast_start"),
      textOutput("B3demandforecast_start"),
      textOutput("B4demandforecast_start"),
      textOutput("B5demandforecast_start"),
      textOutput("B6demandforecast_start"),
      
      ##inventory number output 
      textOutput("B1Left_start"),
      textOutput("B2Left_start"),
      textOutput("B3Left_start"),
      textOutput("B4Left_start"),
      textOutput("B5Left_start"),
      textOutput("B6Left_start"),
      textOutput("B1Exp_start"),
      textOutput("B2Exp_start"),
      textOutput("B3Exp_start"),
      textOutput("B4Exp_start"),
      textOutput("B5Exp_start"),
      textOutput("B6Exp_start"),
      textOutput("roseLeft_start"),
      textOutput("babyLeft_start"),
      textOutput("carnLeft_start"),
      textOutput("roseExp_start"),
      textOutput("babyExp_start"),
      textOutput("carnExp_start"),
      
      textInput("B1calc", "B1" , 0),
      textInput("B2calc", "B2" , 0), 
      textInput("B3calc", "B3" , 0), 
      textInput("B4calc", "B4" , 0), 
      textInput("B5calc", "B5" , 0), 
      textInput("B6calc", "B6" , 0), 
      actionButton("calc", "Calculator"),
      textOutput("R"),
      textOutput("B"),
      textOutput("C"),
      
      #Button to proceed with the game and assign values to variables
      actionButton("startday_btn", "Start Day !"),
      
      # tags$div(dataTableOutput("forecast_demandTable"), id = "Forecast_D_table")
      ),
    options = list(onshow = HTML('
        $("#modal .modal-header .close").on("click", function(event) {
          event.preventDefault();
        });
      '))
    )
}

#please help i cannot remove the empty space under the endday modal
enddayModal <- function(){
  modalDialog(
    id ="end-day-modal",
    size = "l",
    div(
      img(src = "assets/Summary&End_day prompt.png"),
      textOutput("fulfilledOutput"),
      textOutput("flowersLeft_sum"),
      textOutput("flowersExp_sum"),
      textOutput("dailyRevenue"),
      textOutput("dailyCost"),
      textOutput("dailyProfit"),
      tableOutput("BouqLeftOutput"),
      tableOutput("BouqExpOutput"),
      tableOutput("flowExpOutput"),
      tableOutput("flowLeftOutput"),
      textInput("r_order", "", 0),
      textInput("c_order", "", 0),
      textInput("b_order", "", 0),
      textInput("staff_hire", "", 0),
      textInput("staff_fire", "", 0),
      actionButton("endday_btn", "End Day!")
    )
  )
}

#Order fulfilment
order_fulfilmentModal <- function(){   
  modalDialog(     
    div(       
      id = "order-fulfilment-modal",       
      textOutput("order_fulfilmentTitle"),       
      textOutput("bouquet1fulfilled"),       
      textOutput("bouquet2fulfilled"),       
      textOutput("bouquet3fulfilled"),       
      textOutput("bouquet4fulfilled"),       
      textOutput("bouquet5fulfilled"),       
      textOutput("bouquet6fulfilled")     
    )   
  ) 
}
#scoreleaderboard
score_leaderboardModal <- function(){   
  modalDialog(     
    div(       
      id = "score-leaderboard-modal",       
      textOutput("score_leaderboardTitle"),       
      tableOutput("scoreLeaderboard"),     
    )   
  ) 
}