## Modal for instructions ##
instructionsModal <- function() {
  modalDialog(
    title = "Instructions",
    uiOutput("page_content"),
    footer = tagList(
      actionButton("btn_previous", "Previous"),
      actionButton("btn_next", "Next"),
      actionButton("btn_close", "Dismiss")
    )
  )
}

## Modal for register ##
registerModal <- function(repetition = FALSE, validatepw = FALSE) {
  modalDialog(
    title = "Create a user",
    textInput("username", "Enter a username:"),
    passwordInput("password1", "Enter a new password:"),
    passwordInput("password2", "Confirm new password:"),
    "If successful #do something.",
    if (repetition){
      div(tags$b("Username is taken already", style = "color: red;"))
    },
    if (validatepw){
      div(tags$b("Please double check your password", style = "color: red;"))
    },
    footer = tagList(
      modalButton("Cancel"),
      actionButton("registerok", "Register")
    )
  )
}

loginModal <- function(failed = FALSE) {
  modalDialog(
    title = "Login",
    textInput("playername", "Enter your assigned Player Name", "FrostyFuzzyPickle"),
    passwordInput("password3", "Enter your password:"),
    if (failed)
      div(tags$b("There is no registered player with that name and password. Try again or re-register.", style = "color: red;")),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("loginok", "Login")
    )
  )
}

errorModal <- function(text){
  modalDialog(
    title = "Error!",
    text
  )
}

cal_menuModal <- function() {
  modalDialog(
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
    footer = tagList(
      actionButton("back_btn", "Back"),
      actionButton("btn_close", "Dismiss")
    )
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
      textOutput("nextday_B6"),
      ),
    footer = tagList(
      actionButton("back_btn", "Back"),
      actionButton("btn_close", "Dismiss")
    )
  )
}

calendarModal <- function() {
  modalDialog(
    div(
      id = "calendar-modal",
      tags$img(src = "assets/calendar prompt font changed.png"),
    ),
    footer = tagList(
      actionButton("back_btn", "Back"),
      actionButton("btn_close", "Dismiss")
    )
  )
}

inventoryModal <- function(){
  modalDialog(
    div(
      id = "inventory-modal",
      tags$img(src = "assets/Inventory prompt.png"),
      
      ##inventory number output
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
    div(       
      id = "start-game-modal",       
      img(src = "assets/start.png"),
      
      ## user selection for number of flowers for the day
      textInput("B1choice", NULL , 0),       
      textInput("B2choice", NULL, 0),       
      textInput("B3choice", NULL, 0),       
      textInput("B4choice", NULL, 0),       
      textInput("B5choice", NULL, 0),       
      textInput("B6choice", NULL, 0),
      
      ##demand forecast number output 
      textOutput("B1demandforecast_start"),
      textOutput("B2demandforecast_start"),
      textOutput("B3demandforecast_start"),
      textOutput("B4demandforecast_start"),
      textOutput("B5demandforecast_start"),
      textOutput("B6demandforecast_start"),
      
      ##inventory number output 
      textOutput("numberofstaff"),
      textOutput("roseLeft_start"),
      textOutput("babyLeft_start"),
      textOutput("carnLeft_start"),
      textOutput("roseExp_start"),
      textOutput("babyExp_start"),
      textOutput("carnExp_start"),
      
      #start day calculator
      div(
        id = "startcalc_B1",
        textInput("B1calc", NULL , 0),
      ),
      
      div(
        id = "startcalc_B2",
        textInput("B2calc", NULL , 0),
      ),
      div(
        id = "startcalc_B3",
        textInput("B3calc", NULL , 0),
      ),
      div(
        id = "startcalc_B4",
        textInput("B4calc", NULL , 0),
      ),
      div(
        id = "startcalc_B5",
        textInput("B5calc", NULL , 0),
      ),
      div(
        id = "startcalc_B6",
        textInput("B6calc", NULL , 0),
      ),
      div(
        id = "startcalc_r",
        textOutput("R"),
      ),
      div(
        id = "startcalc_c",
        textOutput("C"),
      ),
      div(
        id = "startcalc_b",
        textOutput("B"),
      ),
      #Button to proceed with the game and assign values to variables
      ),
    footer = tagList(
      actionButton("startday_btn", "Start Day!")
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
    div(
      img(src = "assets/End_day prompt.png"),
      #summary output
      textOutput("fulfilledOutput"),
      textOutput("flowersLeft_sum"),
      textOutput("flowersExp_sum"),
      textOutput("dailyRevenue"),
      textOutput("dailyCost"),
      textOutput("dailyProfit"),
      tableOutput("flowExpOutput"),
      tableOutput("flowLeftOutput"),
      #user input
      textInput("r_order", NULL, 0),
      textInput("c_order", NULL, 0),
      textInput("b_order", NULL, 0),
      textInput("staff_hire", NULL, 0),
      textInput("staff_fire", NULL, 0),
      #calculator
      div(
        id = "endcalc_B1",
        textInput("B1calc", NULL , 0),
        ),
      
      div(
        id = "endcalc_B2",
        textInput("B2calc", NULL , 0),
      ),
      div(
        id = "endcalc_B3",
        textInput("B3calc", NULL , 0),
      ),
      div(
        id = "endcalc_B4",
        textInput("B4calc", NULL , 0),
      ),
      div(
        id = "endcalc_B5",
        textInput("B5calc", NULL , 0),
      ),
      div(
        id = "endcalc_B6",
        textInput("B6calc", NULL , 0),
      ),
      div(
        id = "endcalc_r",
        textOutput("R"),
      ),
      div(
        id = "endcalc_c",
        textOutput("C"),
      ),
      div(
        id = "endcalc_b",
        textOutput("B"),
      ),
      textOutput("b1_ec_input"),
      textOutput("b2_ec_input"),
      textOutput("b3_ec_input"),
      textOutput("b4_ec_input"),
      textOutput("b5_ec_input"),
      textOutput("b6_ec_input"),
      textOutput("endday_nfc1"),
      textOutput("endday_nfc2"),
      textOutput("endday_nfc3"),
      textOutput("endday_nfc4"),
      textOutput("endday_nfc5"),
      textOutput("endday_nfc6"),
    ),
    footer = tagList(
      actionButton("endday_btn", "End Day !"),
      actionButton("btn_close", "Dismiss")
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