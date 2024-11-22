# nolint start: box_func_import_count_linter
box::use(
  config[get],
  dplyr[select],
  magrittr[`%>%`],
  shiny,
  shinycssloaders[withSpinner],
)
# nolint end

box::use(
  app/logic/api_utils[get_app_list],
  app/logic/empty_state_utils[generate_empty_state_ui],
  app/logic/general_utils[generate_css_variables],
  app/view/mod_app_table,
  app/view/mod_header,
  app/view/mod_job_list,
  app/view/mod_logs,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$fluidPage(
    class = "dashboard-body",
    shiny$tags$head(
      shiny$uiOutput(ns("dynamic_colors"))
    ),
    mod_header$ui("header"),
    shiny$div(
      class = "dashboard-container",
      shiny$div(
        class = "app-table",
        mod_app_table$ui(ns("app_table"))
      ),
      shiny$div(
        class = "vertical-line"
      ),
      shiny$div(
        class = "job-list",
        shiny$uiOutput(ns("job_list_pane"))
      ),
      shiny$div(
        class = "vertical-line"
      ),
      shiny$div(
        class = "logs",
        shiny$uiOutput(ns("logs_pane"))
      )
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {

    ns <- session$ns

    branding <- get("branding")

    output$dynamic_colors <- shiny$renderUI({
      css_content <- generate_css_variables(branding)
      shiny$tags$style(shiny$HTML(css_content))
    })

    mod_header$server("header")

    state <- shiny$reactiveValues()
    state$selected_app <- shiny$reactive({})
    state$selected_job <- shiny$reactive({})

    app_list <- shiny$reactive({
      get_app_list()
    })

    mod_app_table$server(
      "app_table",
      app_list(),
      state
    )

    shiny$observeEvent(state$selected_app()$guid, {

      if (shiny$isTruthy(state$selected_app()$guid)) {

        output$job_list_pane <- shiny$renderUI({
          mod_job_list$ui(ns("job_list"))
        })

        mod_job_list$server(
          "job_list",
          state
        )

      } else {

        shiny$removeUI(ns("job_list_pane"))

      }
    }, ignoreNULL = FALSE)

    shiny$observeEvent(state$selected_job()$key, {

      if (shiny$isTruthy(state$selected_job()$key)) {

        output$logs_pane <- shiny$renderUI({
          mod_logs$ui(ns("logs"))
        })

        mod_logs$server(
          "logs",
          state
        )
      } else {

        if (!inherits(app_list(), "data.frame")) {
          empty_state <- shiny$renderUI({
            generate_empty_state_ui(
              text = "Oops! Can't read apps from Posit Connect.",
              image_path = "app/static/illustrations/missing_apps.svg",
              color = branding$colors$primary
            )
          })
        } else {
          empty_state <- shiny$renderUI({
            generate_empty_state_ui(
              text = "Select an application and a job to view logs.",
              image_path = "app/static/illustrations/empty_state.svg",
              color = branding$colors$primary
            )
          })
        }

        output$logs_pane <- empty_state
      }

    }, ignoreNULL = FALSE)

  })
}
