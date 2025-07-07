#' @file ts_server.R
#' @title Server Module for Time Series Panel
#'
#' Implements the server-side logic for the "Trends Over Time" module of the 
#' Chinook Music Retail Analytics Dashboard. Handles dynamic data querying, KPI 
#' computation, reactive plotting, and data download functionality, all driven 
#' by user-selected filters and consistent with the active UI theme.
#'
#' @seealso \code{\link{ts_ui}}, \code{\link{form_where_clause}}, 
#'   \code{\link{get_ts_data}}, \code{\link{build_ts_kpis}},
#'   \code{\link{render_kpi_card}}, \code{\link{safe_kpi_card}},
#'   \code{\link{style_ggplot2}}, \code{\link{style_plotly}}

#' Time Series Module Server Logic
#'
#' Connects user input filters and global theme settings to the reactive 
#' generation of KPI boxes, a time-series plot, and data table with export 
#' functionality. Designed for use with \code{ts_ui()}.
#'
#' @param id Namespace string for the module instance.
#' @param con A DBI-compliant database connection (e.g., DuckDB).
#' @param filters A reactive expression returning a list of active filters 
#'   (date_range, country, genre, artist, metric).
#' @param styles A reactive expression containing theming/styling elements 
#'   (e.g., text colors, backgrounds).
#'
#' @return None. Called for side-effect within a Shiny server.
#' @export
ts_server <- function(id, con, filters, styles) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Cache data for optimization
    cached_ts <- reactiveVal(NULL)
    
    # Dynamically generate SQL WHERE clause based on active filters
    where_clause_reactive <- reactive({
      form_where_clause(
        date_range = filters()$date_range,
        country = filters()$country,
        genre = filters()$genre,
        artist = filters()$artist,
        .con = con
      )
    })
    
    # Reactive query when the filters change
    shiny::observeEvent(where_clause_reactive(), {
      where_clause <- where_clause_reactive()
      data_raw <- get_ts_data(con, where_clause)
      
      data_monthly <- data_raw |>
        dplyr::group_by(month) |>
        dplyr::summarise(
          revenue = sum(revenue, na.rm = TRUE),
          num_purchases = sum(num_purchases, na.rm = TRUE),
          tracks_sold = sum(tracks_sold, na.rm = TRUE),
          num_customers = dplyr::n_distinct(customer_id),
          first_time_customers = sum(first_time_customer, na.rm = TRUE),
          .groups = "drop"
        )
      
      # KPIs - May be NULL if no data
      kpis <- build_ts_kpis(data_raw, data_monthly)
      
      # Pre-compute and store transformed time-series data + KPI summaries
      cached_ts(list(raw = data_raw, monthly = data_monthly, kpis = kpis))
    })
    
    ts_data <- reactive({
      req(cached_ts())
      cached_ts()
    })
    
    # --- KPI Card Outputs ---
    # 1. Revenue Box
    output$revenue_box <- renderUI({
      safe_kpi_card(
        kpis = ts_data()$kpis, 
        body_fn = function() {
          k <- ts_data()$kpis
          list(
            build_kpi("Total", scales::dollar(k$total_rev), "Total revenue."),
            build_kpi(
              "Avg / Month", 
              scales::dollar(k$avg_rev), 
              "Average revenue per month."
            )
          )
        }, 
        title = "Revenue",
        icon = bsicons::bs_icon("currency-dollar"),
        tooltip = "Gross revenue, in US dollars.",
        styles = styles()
      )
    })
    
    # 2. Purchases Box
    output$purchase_box <- renderUI({
      safe_kpi_card(
        kpis = ts_data()$kpis,
        body_fn = function() {
          k <- ts_data()$kpis
          list(
            build_kpi(
              "Purchases", 
              k$total_purchases, 
              "Total number of unique purchases."
            ),
            build_kpi(
              "Tracks Sold", 
              k$total_tracks, 
              "Total number of tracks sold (unit sales volume)."
            ),
            build_kpi(
              "Avg $ / Purchase", 
              if (!is.na(k$avg_per_purchase)) {
                scales::dollar(k$avg_per_purchase)
              } else {
                NA_integer_
              }, 
              "Average revenue per purchase."
            )
          )
        },
        title = "Purchases",
        icon = bsicons::bs_icon("receipt"),
        tooltip = "Purchase patterns.",
        styles = styles()
      )
    })
    
    # 3. Customers Box
    output$customer_box <- renderUI({
      safe_kpi_card(
        kpis = ts_data()$kpis,
        body_fn = function() {
          k <- ts_data()$kpis
          list(
            build_kpi("Total", k$total_customers, "Total unique customers."),
               build_kpi(
                 "First-Time", k$first_time, "Number of first time customers."
               )
          )
        },
        title = "Customers",
        icon = bsicons::bs_icon("people-fill"),
        tooltip = "Customers that made a purchase.",
        styles = styles()
      )
    })
    
    # --- Plot Output: Interactive Time Series Line Plot ---
    output$ts_plot <- plotly::renderPlotly({
      # Styles
      s <- styles()
      isolate(input$theme_switcher)
      # Data
      df <- ts_data()$monthly
      
      # Parse Metric Options
      y_var <- filters()$metric
      y_label <- dplyr::case_when(
        y_var == "revenue" ~ "Total Revenue (USD$)",
        y_var == "num_customers" ~ "Number of Customers",
        y_var == "first_time_customers" ~ "Number of First-Time Customers",
        y_var == "num_purchases" ~ "Number of Purchases",
        y_var == "tracks_sold" ~ "Tracks Sold"
      )
      
      # Fall back if there are no rows to plot
      if (nrow(df) == 0 || all(is.na(df[[y_var]]))) {
        return(plotly::plotly_empty(type = "scatter", mode = "markers") %>% 
                 plotly::layout(title = "No data available for selected filters"))
      }
      
      # Build Plot
      p <- suppressWarnings(
        ggplot2::ggplot(
          df,
          ggplot2::aes(x = lubridate::ym(month), y = .data[[y_var]])
        ) +
          ggplot2::geom_line(color = s$line_color, size = s$line_size) +
          ggplot2::geom_point(
            ggplot2::aes(
              text = paste0(
                "Month: ", month, "<br>",
                "Revenue: ", scales::dollar(revenue), "<br>",
                "Purchases: ", num_purchases, "<br>",
                "Tracks Sold: ", tracks_sold, "<br>",
                "Unique Customers: ", num_customers, "<br>",
                "First-Time Customers: ", first_time_customers, "<br>",
                "Revenue per Customer: ", 
                scales::dollar(revenue / num_customers)
              )
            ),
            color = s$point_color, size = s$point_size
          ) +
          ggplot2::scale_y_continuous(labels = scales::comma) +
          ggplot2::labs(
            title = paste(y_label, "Over Time"),
            x = "Time (Months)",
            y = y_label
          ))
      
      p <- p |>
        style_ggplot2(styles = s)
      
      plotly::ggplotly(p, tooltip = "text") %>%
        style_plotly(styles = s)
    })
    
    # --- Scrollable Data Table with Metrics by Row ---
    output$ts_table <- DT::renderDataTable({
      DT::datatable(
        ts_data()$raw,
        extensions = "Scroller",
        options = list(
          pageLength = 10,
          scrollY = 300,
          scrollX = TRUE, 
          scrollCollapse = TRUE,
          dom = "tp"
        ),
        rownames = FALSE,
        class = "compact stripe hover"
      )
    })
    
    # --- CSV Download for Raw Transaction Records ---
    output$download_csv <- shiny::downloadHandler(
      filename = function() {
        paste0("chinook_ts_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(ts_data()$raw, file, row.names = FALSE)
      }
    )
  })
}
