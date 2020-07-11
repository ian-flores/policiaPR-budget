#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session ) {
  
  police_times <- reactive({
    program_amount <- program_expenses[program_expenses$year == input$year & program_expenses$agency_name == input$program, ]
    
    police_amount <- police_expenses[police_expenses$year == input$year, ]
    
    if (program_amount$amount == 0){
      return(0)
    } else {
      return(round(police_amount$amount / program_amount$amount, 2))
    }
  })
  
  times_direction <- reactive({
    ifelse(police_times() > 1, 'mas grande', 'tan grande como')
  })
  
  output$demo <- epoxy::renderEpoxyHTML(
    times = police_times(),
    times_direction = times_direction()
  )
}