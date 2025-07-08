#' @file choro_server.R
#' @title Server Module for Geographic Distribution Panel
#'
#' Implements the server-side logic for the "Geographic Distribution" module 
#' of the Chinook Music Retail Analytics Dashboard. Handles dynamic data 
#' querying, KPI computation, reactive plotting, and data download 
#' functionality, all driven by user-selected filters and consistent with the 
#' active UI theme.
#'
#' @seealso \code{\link{ts_ui}}, \code{\link{form_where_clause}}, 
#'   \code{\link{get_choropleth_data}}, \code{\link{build_choro_kpis}},
#'   \code{\link{render_kpi_card}}, \code{\link{safe_kpi_card}},
#'   \code{\link{style_ggplot2}}, \code{\link{style_plotly}}

#' Choropleth Module Server Logic
#'
#' Connects user input filters and global theme settings to the reactive 
#' generation of KPI boxes, a choropleth plot, and data table with export 
#' functionality. Designed for use with \code{choro_ui()}.
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
choro_server <- function(id, con, filters, styles) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Cache data for optimization
    cached_choro <- reactiveVal(NULL)
    
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
      data_raw <- get_choropleth_data(con, where_clause)
      
      # Add ISO Country Codes
      data_raw <- data_raw |>
        ## Add ISO Country Codes
        dplyr::mutate(
          iso_alpha = countrycode::countrycode(
            country, 
            origin = "country.name", 
            destination = "iso3c"
          )
        ) |>
        dplyr::relocate("iso_alpha", .after = "country")
      
      # Add "All" row, aggregating across all years present
      data_all <- dplyr::bind_rows(
        data_raw |> dplyr::mutate(
          year = as.character(year)
        ),
        data_raw |>
          dplyr::group_by(country, iso_alpha) |>
          dplyr::summarise(
            num_customers = sum(num_customers, na.rm = TRUE),
            num_purchases = sum(num_purchases, na.rm = TRUE),
            tracks_sold = sum(tracks_sold, na.rm = TRUE),
            revenue = sum(revenue, na.rm = TRUE),
            first_time_customers = sum(first_time_customers, na.rm = TRUE),
            .groups = "drop"
          ) |>
          dplyr::mutate(year = "All") |>
          dplyr::distinct()
      )
      
      # Determine active metric
      metric_col <- filters()$metric
      
      # Only use the "All" aggregate year to summarize
      agg_df <- data_all |> dplyr::filter(year == "All")
      
      # KPIs - May be NULL if no data
      kpis <- build_choro_kpis(agg_df, metric_col)
      
      # Pre-compute and store transformed country-based data + KPI summaries
      cached_choro(list(raw = data_raw, data_all = data_all, kpis = kpis))
    })
    
    choro_data <- reactive({
      req(cached_choro())
      cached_choro()
    })
    
    # --- KPI Card Outputs ---
    # 1. Top 5 Countries by Metric
    output$top5_countries_box <- renderUI({
      safe_kpi_card(
        kpis <- choro_data()$kpis,
        body_fn = function() {
          k <- choro_data()$kpis
          
          metric_label <- names(metric_choices)[
            metric_choices == filters()$metric
          ]
          
          # Create numbered HTML list
          list_items <- paste0(
            "<ol style='margin: 0; padding-left: 1rem;'>",
            paste0(
              "<li><strong>", k$top_countries_with_flags, ":</strong> ",
              k$top5_metric,
              "</li>", collapse = "\n"
            ),
            "</ol>"
          )
          
          # Body Items for KPI
          list(
            build_kpi(
              label = metric_label %||% "Top Countries",  
              value = list_items,
              tooltip = "Top countries by the selected metric."
            ),
            build_kpi(
              label = "Total Countries", 
              value = k$num_countries, 
              tooltip = "Number of countries with available data."
            )
          )
        }, 
        title = "Top Countries",
        icon = bsicons::bs_icon("trophy-fill"),
        tooltip = "Top 5 countries, by the chosen metric.",
        styles = styles()
      )
    })
    
    # 2. Revenue Share Box
    output$revenue_share_box <- renderUI({
      safe_kpi_card(
        kpis <- choro_data()$kpis,
        body_fn = function() {
          k <- choro_data()$kpis
          
          # Create numbered HTML list
          list_items <- paste0(
            "<ol style='margin: 0; padding-left: 1rem;'>", 
            paste0(
              "<li><strong>", k$total_revenue, 
              "</strong> (", k$revenue_pct, ")", 
              "</li>", collapse = "\n"
            ),
            "</ol>"
          )
          # Body Items for KPI
          list(
            build_kpi(
              label = "Total Revenue",  
              value = list_items,
              tooltip = "Revenue (% of total revenue)."
            )
          )
        },
        title = "Revenue Share",
        icon = bsicons::bs_icon("pie-chart-fill"),
        tooltip = "Revenue and percentage of total revenue (USD$).",
        styles = styles()
      )
    })
    
    # 3. Customer Box
    output$customer_box <- renderUI({
      safe_kpi_card(
        kpis <- choro_data()$kpis,
        body_fn = function() {
          k <- choro_data()$kpis
          
          # Create numbered HTML list
          list_items <- paste0(
            "<ol style='margin: 0; padding-left: 1rem;'>", 
            paste0(
              "<li><strong>", k$num_customers, 
              "</strong> (", k$avg_rev_per_customer, ")", 
              "</li>", collapse = "\n"
            ),
            "</ol>"
          )
          
          # Body Items for KPI
          list(
            build_kpi(
              label = "Customers",  
              value = list_items,
              tooltip = paste0(
                "Total number of customers ",
                "(average revenue per customer, USD$)."
              )
            )
          )
        },
        title = "Customers (N, Avg$)",
        icon = bsicons::bs_icon("people-fill"),
        tooltip = paste0(
          "Total number of customers and average",
          " revenue per customer (USD$)."
        ),
        styles = styles()
      )
    })
    
    # --- Plot Output: Interactive Choropleth (Year Animated) Plot ---
    output$choro_plot <- plotly::renderPlotly({
      # Styles
      s <- styles()
      isolate(input$theme_switcher)
      # Data - remove values missing an ISO ALPHA
      df <- choro_data()$data_all |>
        dplyr::filter(!is.na(iso_alpha))
      
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
        return(
          plotly::plotly_empty(type = "scatter", mode = "markers") %>% 
            plotly::layout(title = "No data available for selected filters")
        )
      }
      
      # Make 'year' into an ordered factor, with 'All' last in animation
      year_vals <- c(sort(unique(df$year[df$year != "All"])), "All")
      df <- df |>
        dplyr::mutate(year = factor(year, levels = year_vals))
      
      ## Hover Text
      df$hover <- paste0(
        "Country: ", df$country, "<br>",
        "Year: ", df$year, "<br>",
        "Revenue: ", scales::dollar(df$revenue), "<br>",
        "Purchases: ", df$num_purchases, "<br>",
        "Tracks Sold: ", df$tracks_sold, "<br>",
        "Unique Customers: ", df$num_customers, "<br>",
        "First Time Customers: ", df$first_time_customers, "<br>",
        "Revenue per Customer: ", 
        scales::dollar(df$revenue / df$num_customers)
      )
      
      # Choropleth Plot
      plotly::plot_ly(
        data = df,
        type = "choropleth",
        locations = ~iso_alpha,
        z = df[[y_var]],
        # Set minimum and maximum Z values, for consistent scale
        zmin = min(df[[y_var]]),
        zmax = max(df[[y_var]]),
        # Hover Text
        text = df$hover,
        hoverinfo = "text",
        colorscale = s$fill_palette$colorscale,
        reversescale = s$fill_palette$reversescale,
        showscale = TRUE,
        ## Label legend
        colorbar = list(
          title = y_label
        ),
        # Give national boundaries an outline
        marker = list(
          line = list(color = s$geo_border_color, width = s$geo_border_width)
        ),
        frame = ~year
      ) %>%
        style_plotly(styles = s, type = "geo") %>%
        plotly::layout(
          title = list(
            text = paste(y_label, "by Country (Animated by Year)")
          )
        ) %>%
        plotly::animation_opts(frame = 1000, transition = 0, redraw = TRUE)
      
    })
    
    # --- Scrollable Data Table with Metrics by Row ---
    output$choro_table <- DT::renderDataTable({
      DT::datatable(
        choro_data()$raw,
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
        paste0("chinook_geo_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(choro_data()$raw, file, row.names = FALSE)
      }
    )
  })
}
