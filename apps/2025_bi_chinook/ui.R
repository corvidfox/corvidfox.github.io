#' @file ui.R
#' @title Shiny UI Layout for Chinook BI Dashboard
#'
#' Defines the fluid-page layout and user interface structure for the Chinook 
#' Music Retail Analytics Dashboard. Includes theme-aware styling, responsive 
#' sidebar filters, modular navigation panels, and accessibility to static 
#' dataset metadata.
#'
#' @section Components:
#' - `theme = theme_light`: default light theme with dark mode support.
#' - `uiOutput("dynamic_styles")`: applies Bootstrap-style CSS overrides from 
#' server.
#' - `input_dark_mode()`: toggle between light and dark themes.
#' - `layout_sidebar()`: contains filter controls and metadata accordion.
#' - `navset_tab()`: renders main content panels for each analytics section.
#'
#' @seealso server.R, global.R

bslib::page_fluid(
  # Default theme
  theme = theme_light,
  # Theme-aware CSS injection (from server-side renderUI)
  uiOutput("dynamic_styles"),
  
  # Sticky dashboard title with dark mode toggle
  shiny::tagList(
    div(
      class = "sticky-header",
      style = paste0(
        "display: flex; ",
        "align-items: left; ", 
        "justify-content: center; ",
        "gap: 1rem; ",
        "padding: 1rem;"
      ),
      bslib::input_dark_mode(id = "theme_switcher"),
      h2("Chinook Music Retail Analytics Dashboard", style = "margin: 0;")
    )
  ),
  
  # Sidebar containing filters and dataset metadata
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Filters",
      width = 300,
      
      # Filters
      shinyWidgets::airDatepickerInput(
        "date_range", "Select Date Range:",
        range = TRUE,
        value = c(min_date, max_date),
        dateFormat = "yyyy-MMM",
        separator = " to ",
        view = "months",
        minView = "months",
        minDate = min_date,
        maxDate = max_date,
        startView = min_date,
        autoClose = TRUE,
        width = "100%",
        update_on = "close"
      ),
      shiny::selectizeInput(
        "country", "Country:", choices = country_choices, multiple = TRUE
      ),
      shiny::selectizeInput(
        "genre", "Genre:", choices = genre_choices, multiple = TRUE
      ),
      shiny::selectizeInput(
        "artist", "Artist:", choices = artist_choices, multiple = TRUE
      ),
      shiny::selectInput("metric", "Metric:", choices = metric_choices),
      shiny::actionButton(
        "clear_filters", "Clear Filters", icon = icon("times-circle")
      ),
      
      # "About this Data"
      bslib::accordion(
        id = "about_data",
        bslib::accordion_panel(
          "About This Data",
          p(
            bsicons::bs_icon("vector-pen"), 
            "Created by", 
            a(
              "Morrigan M.", 
              href = "https://github.com/corvidfox", 
              target = "_blank"
            )
          ),
          p(
            bsicons::bs_icon("link-45deg"), 
            a(
              "Chinook Dataset", 
              href = "https://github.com/lerocha/chinook-database", 
              target = "_blank"
            )
          ),
          p(
            bsicons::bs_icon("file-earmark-fill"), 
            a(
              "Portfolio Post", 
              href = paste0(
                "https://corvidfox.github.io/projects/2025_bi_chinook.html"
              ), 
              target = "_blank")
          ),
          p(
            bsicons::bs_icon("calendar2-check-fill"), 
            "Last updated: ", 
            Sys.Date()
          ),
          
          # Static summary table of full dataset stats
          bslib::card(
            full_screen = FALSE,
            bslib::card_header("Dataset Overview"),
            bslib::card_body(DT::dataTableOutput("static_summary"))
          )
        )
      )
    ),
    
    # Main content organized into modular panels
    bslib::navset_tab(
      id = "nav",
      bslib::nav_panel("Trends Over Time", ts_ui("ts")),
      bslib::nav_panel("Geographic Distribution", choro_ui("choro")),
      bslib::nav_panel("Performance by Genre", genre_ui("genre")),
      bslib::nav_panel("Performance by Artist", artist_ui("artist")),
      bslib::nav_panel("Customer Retention", retention_ui("retention")),
      bslib::nav_panel("Key Insights", "Coming Soon." 
                       #insights_ui("insights")
                       )
    )
  )
)
