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
calendarModal <- function() {
  modalDialog(
    size = "l",
    div(
      id = "calendar-modal",
      tags$img(src = "assets/calendar prompt font changed.png")
    )
  )
}

inventoryModal <- function(){
  modalDialog(
    size = "l",
    div(
      id = "inventory-modal",
      tags$img(src = "assets/photo_2023-07-19_12-39-09.jpg"),
      tags$div(dataTableOutput("BLeftTable"), id = "B_inv_table")
    )
  )
}
