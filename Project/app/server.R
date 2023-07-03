## Server logic ##
server <- function(input, output, session){
  vals <- reactiveValues(a = 1)
  
  output$testing <- renderUI({
    h3(vals$a)
  })
  
  observeEvent(input$plus, {
    vals$a <- vals$a + 1
  })
}