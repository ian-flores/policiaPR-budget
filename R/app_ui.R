#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    fillPage(
      fillRow(
        class = "main-container",
        div(
          class = "main-text",
          epoxy::epoxyHTML(
            .id = "demo",
            "<p>En ",
            epoxy:::epoxyInlineClickChoice(
              "year",
              "Year",
              as.character(2020:2018)
            ),
            " el presupuesto de la Policia de Puerto Rico fue <b>{{times}}</b> veces {{times_direction}} que el presupuesto de el/la",
            epoxy:::epoxyInlineClickChoice(
              "program",
              "City Program",
              {
                programs <- unique(program_expenses$agency_name)
                sample(programs, size = 1000, replace = T)
              }
            ),
            " .</p>"
          ),
          br(),
          HTML("<p>Data: <a href = 'http://www.presupuesto.pr.gov/PRESUPUESTOPROPUESTO2020-2021/Pages/default.aspx'>Oficina de Gerencia y Presupuesto</a>, Codigo: <a href = 'https://github.com/ian-flores/policiaPR-budget'>GitHub</a></p>")
        )
      )
    )
  )
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  golem::add_resource_path(
    "www", app_sys("app/www")
  )
  
  tags$head(
    golem::favicon(),
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "Puerto Rico Police Budget"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}