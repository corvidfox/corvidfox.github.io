#' @file retention_ui.R
#' @title UI Module for Customer Retention Panel
#'
#' Defines the user interface for the "Customer Retention" tab of the Chinook 
#' Music Retail Analytics Dashboard. Includes KPI summary cards, Plotly 
#' time-series retention-decay curve and cohort retention heatmap 
#' visualizations, scrollable data tables, and themed download buttons.
#'
#' The module is designed to be theme-aware, mobile-responsive, and compatible
#' with dynamic Bootstrap styling provided by `bslib`.
#'
#' @seealso \code{\link{retention_server}}, 
#' \code{\link[bslib]{layout_column_wrap}}, \code{\link[shiny]{NS}}

#' Customer Retention Module UI
#'
#' Creates the themed and responsive UI layout for the Customer Retention tab, 
#' including KPI boxes, interactive Plotly charts, and scrollable data 
#' tables with download buttons.
#'
#' @param id A module namespace string used to isolate UI components.
#'
#' @return A `shiny::tagList` of UI elements for this panel.
#' @export
retention_ui <- function(id) {
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
        shiny::uiOutput(ns("lifespan_box")),
        shiny::uiOutput(ns("repeat_behavior_box")),
        shiny::uiOutput(ns("tempo_box"))
      ),
      
      br(),
      # Tabset: Customer Decay Curve & Cohort Retention Heatmap
      ## Keeps theming and organization consistent, but consolidates the two
      ## plots/tables into one panel
      bslib::navset_tab(
        id = "nav",
        bslib::nav_panel(
          "Retention Decay Curve", 
          br(),
          # Interactive Retention Curve Visualization (Plot)
          plotly::plotlyOutput(ns("retention_curve_plot")),
          # Scrollable Table with Download (Retention Curve Visualization)
          br(),
          bslib::card(
            full_screen = FALSE,
            bslib::card_header("Data Table"),
            bslib::card_body(DT::dataTableOutput(ns("retention_curve_table"))),
            div(
              style = "text-align: center;",
              downloadButton(
                ns("download_curve_csv"), 
                "Download CSV", 
                class = "download-btn"
              )
            )
          )
        ),
        bslib::nav_panel(
          "Cohort Retention Heatmap",
          # Interactive Cohort Heatmap Visualization (Plot)
          br(),
          plotly::plotlyOutput(ns("retention_heatmap_plot")), 
          br(),
          # Scrollable Table with Download (Cohort Retention Heatmap)
          bslib::card(
            full_screen = FALSE,
            bslib::card_header("Data Table"),
            bslib::card_body(DT::dataTableOutput(ns("retention_heatmap_table"))),
            div(
              style = "text-align: center;",
              downloadButton(
                ns("download_curve_csv"), 
                "Download CSV", 
                class = "download-btn"
              )
            )
          )
        )
      )
    )
  )
}

