#' @file ts_ui.R
#' @title UI Module for Time Series Panel
#'
#' Defines the user interface for the "Trends Over Time" tab of the Chinook 
#' Music Retail Analytics Dashboard. Includes KPI summary cards, a Plotly 
#' time-series visualization, a scrollable data table, and a themed download 
#' button.
#'
#' The module is designed to be theme-aware, mobile-responsive, and compatible
#' with dynamic Bootstrap styling provided by `bslib`.
#'
#' @seealso \code{\link{ts_server}}, \code{\link[bslib]{layout_column_wrap}}, 
#' \code{\link[shiny]{NS}}

#' Time Series Module UI
#'
#' Creates the themed and responsive UI layout for the Time Series tab, 
#' including KPI boxes, an interactive Plotly chart, and a scrollable data 
#' table with a download button.
#'
#' @param id A module namespace string used to isolate UI components.
#'
#' @return A `shiny::tagList` of UI elements for this panel.
#' @export
ts_ui <- function(id) {
  ns <- NS(id)
  div(
    # Enables smooth theming of module components
    class = "theme-transition",
    shiny::tagList(
      br(),
      # KPI Overview (Responsive, row of 3, stacked on small viewports)
      bslib::layout_column_wrap(
        width = 1 / 3,
        min_width = 200,
        gap = "1rem",
        fill = TRUE,
        shiny::uiOutput(ns("revenue_box")),
        shiny::uiOutput(ns("purchase_box")),
        shiny::uiOutput(ns("customer_box"))
      ),
      
      br(),
      
      # Interactive Time Series Visualization (Plot)
      plotly::plotlyOutput(ns("ts_plot")),
      
      br(),
      
      # Scrollable Transaction Table with Download
      br(),
      bslib::card(
        full_screen = FALSE,
        bslib::card_header("Data Table"),
        bslib::card_body(DT::dataTableOutput(ns("ts_table"))),
        div(
          style = "text-align: center;",
          downloadButton(
            ns("download_csv"), 
            "Download CSV", 
            class = "download-btn"
          )
        )
      )
    )
  )
}
