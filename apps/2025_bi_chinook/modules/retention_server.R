#' @file retention_server.R
#' @title Server Module for Customer Retention Panel
#'
#' Implements the server-side logic for the "Customer Retention" module of the 
#' Chinook Music Retail Analytics Dashboard. Handles dynamic data querying, KPI 
#' computation, reactive plotting, and data download functionality, all driven 
#' by user-selected filters and consistent with the active UI theme.
#'
#' @seealso \code{\link{retention_ui}}, \code{\link{form_where_clause}}, 
#'   \code{\link{get_retention_decay_data}}, 
#'   \code{\link{get_retention_cohort_data}}, 
#'   \code{\link{get_retention_kpi_events}}, 
#'   \code{\link{build_retention_kpis}},
#'   \code{\link{render_kpi_card}}, \code{\link{safe_kpi_card}},
#'   \code{\link{style_ggplot2}}, \code{\link{style_plotly}}

#' Customer Retention Module Server Logic
#'
#' Connects user input filters and global theme settings to the reactive 
#' generation of KPI boxes, retention-decay plot, cohort retention heatmap
#' plot, and data tables with export functionality. Designed for use with 
#' \code{retention_ui()}.
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
retention_server <- function(id, con, filters, styles) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Cache data for optimization
    cached_retention <- reactiveVal(NULL)
    
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
      decay_df <- get_retention_decay_data(con, where_clause)
      cohort_df <- get_retention_cohort_data(con, where_clause)
      events_df <- get_retention_kpi_events(
        con, 
        form_where_clause(
          date_range = NULL, 
          country = filters()$country, 
          genre = filters()$genre, 
          artist = filters()$artist, .con = con
          )
        )
      
      # KPIs - May be NULL if no data
      kpis <- build_retention_kpis(
        events = events_df, 
        cohort_df = cohort_df, 
        date_range = filters()$date_range, 
        offsets = c(3, 6, 9)
      )
      
      # Pre-compute and store decay data + cohort data + KPI summaries
      cached_retention(list(
        decay = decay_df, cohort = cohort_df, kpis = kpis
        ))
    })
    
    retention_data <- reactive({
      req(cached_retention())
      cached_retention()
    })
    
    # --- KPI Card Outputs ---
    # 1. Customer Lifespan Box
    output$lifespan_box <- renderUI({
      safe_kpi_card(
        kpis = retention_data()$kpis, 
        body_fn = function() {
          k <- retention_data()$kpis
          list(
            build_kpi(
              "Total Customers (% New)", 
              glue::glue("{k$num_customers} ({k$pct_new_customers})"), 
              "Total customers active in subset (% of that are new)."
              ),
            build_kpi(
              "Avg Lifespan (Full, Months)", 
              k$avg_lifespan_months_total, 
              "Average customer lifespan across full history, in months."
            ),
            build_kpi(
              "Avg Lifespan (Subset, Months)", 
              k$avg_lifespan_months_total, 
              "Average time active in the current date range, in months."
            )
          )
        }, 
        title = "Customer Overview",
        icon = bsicons::bs_icon("people-fill"),
        tooltip = paste0(
          "Customer count and average engagement lifespan."
          ),
        styles = styles()
      )
    })
    
    # 2. Repeat Behavior Box
    output$repeat_behavior_box <- renderUI({
      safe_kpi_card(
        kpis = retention_data()$kpis,
        body_fn = function() {
          k <- retention_data()$kpis
          list(
            build_kpi(
              "Repeat Rate (All)", 
              glue::glue("{k$n_repeat} ({k$repeat_rate_total})"), 
              paste0(
                "Number of and % of repeat customers in the full data.")
            ),
            build_kpi(
              "Returning Repeat Rate", 
              glue::glue("{k$n_repeat_ret} ({k$repeat_rate_returning})"), 
              "Number and % of returning repeat customers in the subset."
            ),
            build_kpi(
              "New → Repeat Rate", 
              glue::glue("{k$n_repeat_new} ({k$repeat_rate_new})"),
              paste0(
                "Number and % of new customers in the subset that ",
                "became repeating customers."
                )
            ),
            build_kpi(
              "1st→2nd Gap (Lifetime, Days)",
              k$median_gap_overall,
              paste0(
                "Median days between first and second purchase",
                " in customer lifetime."
                )
            )
          )
        },
        title = "Repeat Customer Behavior",
        icon = bsicons::bs_icon("arrow-repeat"),
        tooltip = "Conversion from first-time to returning customers.",
        styles = styles()
      )
    })
    
    # 3. Engagement Tempo Box
    output$tempo_box <- renderUI({
      safe_kpi_card(
        kpis = retention_data()$kpis,
        body_fn = function() {
          k <- retention_data()$kpis
          list(
            build_kpi(
              "Avg Gap (Subset)", 
              k$avg_gap_window, 
              "Average days between purchases in subset."
              ),
            build_kpi(
              "Avg Gap (Bounded)", 
              k$avg_gap_bounded, 
              paste0(
                "Average days between purchases, ",
                "including the most recent purchase before and/or",
                " after the subset date range."
                )
            ),
            build_kpi(
              "Top Cohort (3mo)", 
              glue::glue(
                "{k$top_cohort_month_3} ({k$top_cohort_retention_3})"
                ),
              tooltip = "Best 3-month retention cohort, and retention rate."
              ),
            build_kpi(
              "Top Cohort (6mo)", 
              glue::glue(
                "{k$top_cohort_month_6} ({k$top_cohort_retention_6})"
              ),
              tooltip = "Best 6-month retention cohort, and retention rate."
            ),
            build_kpi(
              "Top Cohort (9mo)", 
              glue::glue(
                "{k$top_cohort_month_9} ({k$top_cohort_retention_9})"
              ),
              tooltip = "Best 9-month retention cohort, and retention rate."
            )
          )
        },
        title = "Purchase Tempo",
        icon = bsicons::bs_icon("graph-up"),
        tooltip = "Average purchase spacing and cohort rentention highlights.",
        styles = styles()
      )
    })
    
    # --- Plot Output: Interactive Retention Curve (Line) Plot ---
    output$retention_curve_plot <- plotly::renderPlotly({
      # Styles
      s <- styles()
      isolate(input$theme_switcher)
      # Data
      df <- retention_data()$decay |>
        dplyr::mutate(retention_pct = retention_pct / 100)
      
      # Fall back if there are no rows to plot
      if (nrow(df) == 0) {
        return(
          plotly::plotly_empty(type = "scatter", mode = "markers") %>% 
            plotly::layout(title = "No data available for selected filters")
          )
      }
      
        # Add hover text
        df$hover_text <- paste0(
          "Month: ", scales::comma(df$month_offset), "<br>",
          "Retention: ", scales::percent(df$retention_pct)
        ) 
        
        # Create ggplot
        p <- ggplot2::ggplot(
            df, 
            ggplot2::aes(x = month_offset, y = retention_pct)
          ) +
          # Add lines with dots - hover text on dots
          ggplot2::geom_line(color = s$line_color, size = s$line_size) +
          ggplot2::geom_point(
            color = s$point_color, size = s$point_size, 
            ggplot2::aes(text = hover_text)
            ) +
          # Format x axis to have a tick every 3 months
          ggplot2::scale_x_continuous(
            breaks = seq(0, max(df$month_offset, na.rm = TRUE), by = 3)
            ) +
          # Format y-axis as percentages
          ggplot2::scale_y_continuous(labels = scales::percent) +
          # Labels
          ggplot2::labs(
            title = "Customer Retention Decay",
            x = "Months Since First Purchase",
            y = "Retention (%)"
          )
        
        p <- p %>%
          style_ggplot2(styles = s)
        
        plotly::ggplotly(p, tooltip = "text") %>%
          style_plotly(styles = s)
    })
    
    # --- Plot Output: Cohort Retention Heatmap Plot ---
    output$retention_heatmap_plot <- plotly::renderPlotly({
      # Styles
      s <- styles()
      isolate(input$theme_switcher)
      # Data
      df <- retention_data()$cohort |>
        dplyr::mutate(retention_pct = retention_pct / 100)
      
      # Fall back if there are no rows to plot
      if (nrow(df) == 0) {
        return(
          plotly::plotly_empty(type = "scatter", mode = "markers") %>% 
            plotly::layout(title = "No data available for selected filters")
        )
      }
      
      # Ensure all Cohort-Months are represented
      df <- tidyr::complete(
        df,
        cohort_month = seq.Date(
          min(df$cohort_month, na.rm = TRUE),
          max(df$cohort_month, na.rm = TRUE),
          by = "month"
        ),
        month_offset = seq(
          min(df$month_offset, na.rm = TRUE),
          max(df$month_offset, na.rm = TRUE),
          by = 1
        )
      ) |> 
        dplyr::filter(month_offset > 0) |> 
        dplyr::mutate(
          hover_text = dplyr::if_else(
            !is.na(retention_pct),
            paste0(
              "Cohort: ", format(cohort_month, "%b %Y"), "\n",
              "Cohort Size: ", scales::comma(cohort_size), "\n",
              "Month Offset: ", scales::comma(month_offset), "\n",
              "Retention: ", scales::percent(retention_pct, accuracy = 0.1)
            ),
            paste0(
              "Cohort: ", format(cohort_month, "%b %Y"), "\n",
              "Month Offset: ", scales::comma(month_offset), "\n",
              "No data"
            )
          )
        )
      
      # Build plot
      p <- ggplot2::ggplot(
        df,
        ggplot2::aes(
          x = month_offset,
          y = cohort_month,
          fill = retention_pct,
          text = hover_text
        )
      ) +
        geom_tile(color = s$ggplot_bg, size = 0.2) +
        scale_fill_viridis_c(
          na.value = s$plotly_hover_bg,
          option = "D",
          direction = if (s$fill_palette$reversescale) 1 else -1,
          labels = scales::percent
        ) +
        scale_x_continuous(breaks = pretty(df$month_offset)) +
        scale_y_date(date_breaks = "1 month", date_labels = "%b %Y") +
        labs(
          title = "Customer Retention by Cohort",
          x = "Months Since First Purchase",
          y = "Cohort Month (First Purchase)",
          fill = "Retention (%)"
        )
      
      p <- p %>%
        style_ggplot2(styles = s) +
        ggplot2::theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid = element_blank()
        )
      
      plotly::ggplotly(p, tooltip = "text") %>%
        style_plotly(styles = s)
    })    
    
    # --- Scrollable Data Table (Retention Decay Curve) ---
    output$retention_curve_table <- DT::renderDataTable({
      DT::datatable(
        retention_data()$decay,
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
    
    # --- CSV Download for Records (Retention Decay Curve) ---
    output$download_curve_csv <- shiny::downloadHandler(
      filename = function() {
        paste0("chinook_cust_decay_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(retention_data()$decay, file, row.names = FALSE)
      }
    )
    
    # --- Scrollable Data Table (Cohort Retention) ---
    output$retention_heatmap_table <- DT::renderDataTable({
      DT::datatable(
        retention_data()$cohort,
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
    
    # --- CSV Download for Records (Cohort Retention) ---
    output$download_heatmap_csv <- shiny::downloadHandler(
      filename = function() {
        paste0("chinook_cust_cohort_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(retention_data()$cohort, file, row.names = FALSE)
      }
    )
  })
}
