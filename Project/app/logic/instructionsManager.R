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
    title = "Calendar",
    div(
      id = "calendar-modal",
      tags$img(src = "assets/calendar prompt font changed.png")
    )
  )
}

