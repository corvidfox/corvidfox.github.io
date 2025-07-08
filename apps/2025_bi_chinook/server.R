#' @file server.R
#' @title Server Logic for Chinook BI Dashboard
#'
#' Contains server-side logic for theming, filter handling, dataset summary 
#' rendering, and module initialization. Bridges UI inputs with dynamic 
#' outputs using reactivity.
#'
#' @section Responsibilities:
#' - Compute and render static sidebar summary
#' - Dynamically inject themed CSS via `generate_css()`
#' - Handle theme toggling and color reactivity
#' - Clear user-selected filters
#' - Bundle filters and apply them to modular panels (e.g. `ts_server()`)
#'
#' @seealso \code{\link{generate_styles}}, \code{\link{generate_css}}, 
#' \code{\link{ts_server}}

function(input, output, session) {
  
  ## ---- Static Dataset Summary (Sidebar) ----
  summary_stats <- get_summary_stats(con)
  summary_df <- data.frame(
    Metric = c(
      "Date Range", "Number of Purchases", "Number of Customers",
      "Tracks Sold", "Artists", "Genres", "Countries"
    ),
    Value = c(
      paste0(format(as.Date(summary_stats$date_range$min_date), "%b %Y"), 
             " – ", 
             format(as.Date(summary_stats$date_range$max_date), "%b %Y")),
      summary_stats$purchases,
      summary_stats$customers,
      summary_stats$tracks_sold,
      summary_stats$artists,
      summary_stats$genres,
      summary_stats$countries
    ),
    stringsAsFactors = FALSE
  )
  output$static_summary <- DT::renderDataTable({
    DT::datatable(
      summary_df, 
      options = list(dom = "t", paging = FALSE), 
      rownames = FALSE,
      class = "compact stripe hover"
    )
  })
  
  ## ---- Theming: Light/Dark Toggle ----
  observe({
    if (isTRUE(input$theme_switcher)) {
      session$setCurrentTheme(theme_dark)
    } else {
      session$setCurrentTheme(theme_light)
    }
  })
  
  ## ---- Inject Theme-Aware CSS ----
  output$dynamic_styles <- renderUI({
    styles <- if (input$theme_switcher == "dark") theme_dark else theme_light
    
    tags$style(HTML(generate_css(styles)))
  })
  
  ## ---- Generate Style Parameters for Plots/Cards ----
  styles <- reactive({
    mode <- input$theme_switcher
    generate_styles(mode)
  })
  
  ## ---- Clear Filters ----
  shiny::observeEvent(input$clear_filters, {
    shinyWidgets::updateAirDateInput(
      session,
      inputId = "date_range",
      value = c(min_date, max_date)
    )
    shiny::updateSelectizeInput(
      session, "genre", selected = NULL, server = TRUE
    )
    shiny::updateSelectizeInput(
      session, "artist", selected = NULL, server = TRUE
    )
    shiny::updateSelectizeInput(
      session, "country", selected = NULL, server = TRUE
    )
    shiny::updateSelectInput(session, "metric", selected = "revenue")
  })
  
  ## ---- Reactive Filter Bundle ----
  filter_inputs <- reactive({
    list(
      date_range = input$date_range,
      country = input$country,
      genre = input$genre,
      artist = input$artist,
      metric = input$metric
    )
  })
  
  ## ---- Module Mounting ----
  ts_server("ts", con = con, filters = filter_inputs, styles = styles)
  choro_server("choro", con = con, filters = filter_inputs, styles = styles)
  genre_server("genre", con = con, filters = filter_inputs, styles = styles)
  artist_server("artist", con = con, filters = filter_inputs, styles = styles)
  retention_server(
    "retention", con = con, filters = filter_inputs, styles = styles
    )
  #insights_server("insights", styles = styles)
}
