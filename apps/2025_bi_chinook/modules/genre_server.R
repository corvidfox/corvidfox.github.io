#' @file genre_server.R
#' @title Server Module for Genre Panel
#'
#' Implements the server-side logic for the "by Genre" module of the 
#' Chinook Music Retail Analytics Dashboard. Handles dynamic data querying, KPI 
#' computation, reactive plotting, and data download functionality, all driven 
#' by user-selected filters and consistent with the active UI theme.
#'
#' @seealso \code{\link{genre_ui}}, \code{\link{form_where_clause}}, 
#'   \code{\link{get_genre_data}}, \code{\link{build_genre_kpis}},
#'   \code{\link{render_kpi_card}}, \code{\link{safe_kpi_card}},
#'   \code{\link{style_ggplot2}}, \code{\link{style_plotly}}

#' Genre Module Server Logic
#'
#' Connects user input filters and global theme settings to the reactive 
#' generation of KPI boxes, a by genre bar plot, and data table with export 
#' functionality. Designed for use with \code{genre_ui()}.
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
genre_server <- function(id, con, filters, styles) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Cache data for optimization
    cached_genre <- reactiveVal(NULL)
    
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
      data_raw <- get_genre_data(con, where_clause)
      
      # Add "All" row, aggregating across all years present
      data_all <- dplyr::bind_rows(
        data_raw |> dplyr::mutate(
          year = as.character(year)
        ),
        data_raw |>
          dplyr::group_by(genre) |>
          dplyr::summarise(
            num_customers = sum(num_customers, na.rm = TRUE),
            num_purchases = sum(num_purchases, na.rm = TRUE),
            tracks_sold = sum(tracks_sold, na.rm = TRUE),
            unique_tracks_sold = sum(unique_tracks_sold, na.rm = T),
            first_track_sold = sum(first_track_sold, na.rm = T),
            revenue = sum(revenue, na.rm = TRUE),
            first_time_customers = sum(first_time_customers, na.rm = TRUE),
            ## Should be a constant, so this makes the pull easier
            tracks_in_catalog = mean(tracks_in_catalog, na.rm = T),
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
      kpis <- build_genre_kpis(agg_df, metric_col)
      
      # Pre-compute and store transformed genre-based data + KPI summaries
      cached_genre(list(raw = data_raw, data_all = data_all, kpis = kpis))
    })
    
    genre_data <- reactive({
      req(cached_genre())
      cached_genre()
    })
    
    # --- KPI Card Outputs ---
    # 1. Top 5 Genres by Metric
    output$genres_box <- renderUI({
safe_kpi_card(
        kpis <- genre_data()$kpis,
        body_fn = function() {
          k <- genre_data()$kpis
          top <- k$top_genres
          metric_label <- names(metric_choices)[
            metric_choices == filters()$metric
          ]
          
          # Create numbered HTML list
          list_items <- paste0(
            "<ol style='margin: 0; padding-left: 1rem;'>",
            paste0(
              "<li><strong>", top$genre, ":</strong> ",
              format(top[[filters()$metric]], big.mark = ","),
              "</li>", collapse = "\n"
            ),
            "</ol>"
          )
          
          # Body Items for KPI
          list(
            build_kpi(
              label = metric_label %||% "Top Genres",  
              value = list_items,
              tooltip = "Top genres by the selected metric."
            ),
            build_kpi(
              label = "Total Genres", 
              value = k$num_genres, 
              tooltip = "Number of genres with available data."
            )
          )
        }, 
        title = "Top Genres",
        icon = bsicons::bs_icon("trophy-fill"),
        tooltip = "Top 5 genres, by the chosen metric.",
        styles = styles()
      )
    })
    
    # 2. Revenue Box
    output$revenue_box <- renderUI({
      safe_kpi_card(
        kpis <- genre_data()$kpis,
        body_fn = function() {
          k <- genre_data()$kpis
          top <- k$top_genres
          
          # Create numbered HTML list
          list_items <- paste0(
            "<ol style='margin: 0; padding-left: 1rem;'>", 
            paste0(
              "<li><strong>", scales::dollar(top$revenue), 
              "</strong> (", 
              scales::percent(k$revenue_pct / 1), ")", 
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
    
    # 3. Catalog Box
    output$catalog_box <- renderUI({
      safe_kpi_card(
        kpis <- genre_data()$kpis,
        body_fn = function() {
          k <- genre_data()$kpis
          
          # Create numbered HTML list
          list_items <- paste0(
            "<ol style='margin: 0; padding-left: 1rem;'>", 
            paste0(
              "<li><strong>", format(k$catalog_size, big.mark = ", "), 
              "</strong> (", 
              scales::percent(k$catalog_pct_sold / 100), ")", 
              "</li>", collapse = "\n"
            ),
            "</ol>"
          )
          
          # Body Items for KPI
          list(
            build_kpi(
              label = "Tracks",  
              value = list_items,
              tooltip = paste0(
                "Total tracks in catalog ",
                "(% of catalog sold)."
              )
            )
          )
        },
        title = "Catalog",
        icon = bsicons::bs_icon("disc-fill"),
        tooltip = paste0(
          "Total tracks and percentage of catalog sold."
        ),
        styles = styles()
      )
    })

    # --- Plot Output: Interactive Stacked Bar Plot ---
    output$genre_plot <- plotly::renderPlotly({
      # Styles
      s <- styles()
      isolate(input$theme_switcher)
      # Data
      df <- genre_data()$raw

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
      
      # Format year as factor so it orders correctly (earliest year on bottom)
      df$year <- factor(
        df$year, 
        levels = sort(unique(df$year), decreasing = TRUE)
        )
      
      # Dynamically sort genres by aggregate of selected metric
      df <- df |>
        dplyr::group_by(genre) |>
        dplyr::mutate(total_metric = sum(.data[[y_var]], na.rm = TRUE)) |>
        dplyr::ungroup() |>
        dplyr::mutate(genre = reorder(genre, -total_metric))

      # Add hover text
      df$hover_text <- paste0(
        "Genre: ", df$genre, "<br>",
        "Year: ", df$year, "<br>",
        "Total Revenue (USD$): ", scales::dollar(df$revenue), "<br>",
        "Purchases: ", df$num_purchases, "<br>",
        "Tracks Sold: ", df$tracks_sold, "<br>",
        "Tracks in Catalog: ", df$tracks_in_catalog, "<br>",
        "Revenue per Track: ", 
          scales::dollar(df$revenue / df$tracks_in_catalog), "<br>",
        "Percentage of Catalog Sold: ", 
        scales::percent(
          df$unique_tracks_sold / df$tracks_in_catalog, accuracy = 0.1
        )
      )
      
      # Make Stacked Bar Plot - Suppress "Text" Warning
      p <- suppressWarnings(ggplot2::ggplot(
        df, 
        ggplot2::aes(
          x = genre, y = .data[[y_var]], fill = year, text = hover_text
          )) +
          # Make stacked bar plots for aggregate view
          ggplot2::geom_bar(stat = "identity", position = "stack") +
          # Format y-axis for legibility
          ggplot2::scale_y_continuous(labels = scales::comma) +
          ggplot2::scale_fill_viridis_d(
            option = "D",
            direction = if (s$fill_palette$reversescale) 1 else -1,
            end = 0.9
          ) +
          # Labels
          ggplot2::labs(
            title = paste(y_label, "by Genre"),
            x = "Genre",
            y = y_label,
            fill = "Year"
          ) %>%
          style_ggplot2(styles = s) +
          ggplot2::theme(
            legend.position = "bottom",
            # Angle x-axis labels for legibility
            axis.text.x = ggplot2::element_text(
              angle = 90, vjust = 0.5, hjust = 1
              )
          )
        )
      
      plotly::ggplotly(p, tooltip = "text") %>%
        style_plotly(
          styles = s,
          axes_labs = list(x = "Genre", y = y_label, legend = "Year")
          ) %>%
        # Override hover mode of styler back to default
        plotly::layout(hovermode = 'closest')
      
    })
    
    # --- Scrollable Data Table with Metrics by Row ---
    output$genre_table <- DT::renderDataTable({
      DT::datatable(
        genre_data()$raw,
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
        paste0("chinook_genre_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(genre_data()$raw, file, row.names = FALSE)
      }
    )
  })
}
