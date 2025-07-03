## CHINOOK BI R SHINY DASHBOARD

# Load required packages
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DBI)
library(duckdb)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(lubridate)
library(scales)
library(countrycode)

# Connect to DuckDB and Chinook data
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "../data/chinook.duckdb")

# Set colors and sizes for plots
line_color <- 'cadetblue4'
point_color <- 'cadetblue4'
line_thickness <- 0.75
point_thickness <- 1

# Load choices for filters
## Date Range
date_range_limits <- DBI::dbGetQuery(con, "
  SELECT MIN(InvoiceDate) AS min_date, MAX(InvoiceDate) AS max_date FROM Invoice
")
min_date <- as.Date(date_range_limits$min_date)
max_date <- as.Date(date_range_limits$max_date)

## Genre (Alphabetized)
genre_choices <- sort(DBI::dbGetQuery(
  con, 
  "SELECT DISTINCT Name FROM Genre"
  )$Name)

## Artist (Alphabetized)
artist_choices <- sort(DBI::dbGetQuery(
  con, 
  "SELECT DISTINCT Name FROM Artist ORDER BY Name"
  )$Name)

## Country (Alphabetized)
country_choices <- sort(DBI::dbGetQuery(
  con,
  "SELECT DISTINCT BillingCountry FROM Invoice"
)$BillingCountry)


# Define UI
ui <- shinydashboard::dashboardPage(
## Title
  shinydashboard::dashboardHeader(title = "Test BI Dashboard"),

## Sidebar
  shinydashboard::dashboardSidebar(
### Tabs
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem(
        "Time Series", tabName = "timeseries", icon = shiny::icon("chart-line")
        ),
      shinydashboard::menuItem(
        "Choropleth", tabName = "choropleth", icon = shiny::icon("globe")
        ),
      shinydashboard::menuItem(
        "By Genre", tabName = "genrebar", icon = icon("music")
        ),
      shinydashboard::menuItem(
        "By Artist", tabName = "artistbar", icon = icon("user")
        ),
      shinydashboard::menuItem(
        "Retention Curve", tabName = "retention", icon = icon("user-clock")
        ),
      shinydashboard::menuItem(
        "Cohort Heatmap", tabName = "cohort", icon = icon("th-large")
        ),
      
### Filter Buttons
  #### Date Range
  shinyWidgets::airDatepickerInput(
    inputId = "date_range",
    label = "Select Date Range:",
    range = TRUE,
    dateFormat = "yyyy-MMM",
    separator = " to ",
    view = "months",
    minView = "months",
    minDate = min_date,
    maxDate = max_date,
    startView = min_date,
    autoClose = TRUE,
    value = c(min_date, max_date),
    width = '100%',
    update_on = 'close'
  ),
  #### Country
  shiny::selectizeInput(
    "country",
    "Select Country:",
    choices = country_choices,
    selected = NULL,
    multiple = TRUE,
    options = list(placeholder = "All countries")
  ),
  #### Genre
      shiny::selectizeInput(
        "genre", 
        "Select Genre:", 
        choices = genre_choices, 
        selected = NULL,
        multiple = TRUE,
        options = list(placeholder = "All genres")
        ),
  #### Artist
      shiny::selectizeInput(
        "artist", 
        "Select Artist:", 
        choices = artist_choices, 
        selected = NULL,
        multiple = TRUE,
        options = list(placeholder = "All artists")
        ),
  #### Metric
      shiny::selectInput(
        "metric", 
        "Metric:",
        choices = c(
          "Revenue" = "revenue",
          "Number of Customers" = "num_customers",
          "First-Time Customers" = "first_time_customers",
          "Number of Purchases" = "num_purchases",
          "Tracks Sold" = "tracks_sold"
          ),
        selected = "revenue"
        ),

  #### Clear Filters Button
    shiny::actionButton(
      "clear_filters", 
      "Clear Filters", 
      icon = icon("times-circle")
      )
    )
  ),

## Main Body  
  shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      ### Time-Series Tab
      shinydashboard::tabItem(
        tabName = "timeseries",
        ### KPI Boxes
        #### Revenue, Customers, New Customers
        shiny::fluidRow(
          shinydashboard::infoBoxOutput("kpi_revenue"),
          shinydashboard::infoBoxOutput("kpi_customers"),
          shinydashboard::infoBoxOutput("kpi_firsttime")
        ),
        #### Purchases, Tracks Sold, Avg Rev per Invoice
        shiny::fluidRow(
          shinydashboard::infoBoxOutput("kpi_purchases"),
          shinydashboard::infoBoxOutput("kpi_tracks"),
          shinydashboard::infoBoxOutput("kpi_avg_rev_per_purchase")
        ),
        hr(),
        ### Time series plot
        shiny::fluidRow(
          box(
            title = "Time Series Plot", 
            width = 12, 
            plotly::plotlyOutput("ts_plot", height = "500px")
            )
        )
      ),
    ### Choropleth
    shinydashboard::tabItem(
      tabName = "choropleth",
      fluidRow(
        box(
          title = "KPI by Country (Animated by Year)",
          width = 12,
          plotly::plotlyOutput("choropleth_plot", height = "600px")
        )
      )
    ),
    ### by Genre
    shinydashboard::tabItem(
      tabName = "genrebar",
      fluidRow(
        box(
          title = "by Genre",
          width = 12,
          plotly::plotlyOutput("genre_bar_plot", height = "600px")
        )
      )
    ),
    ### by Artist
    shinydashboard::tabItem(
      tabName = "artistbar",
      fluidRow(
        box(
          title = "by Artist",
          width = 12,
          plotly::plotlyOutput("artist_bar_plot", height = "600px")
        )
      )
    ),
    ### Retention Curve
    shinydashboard::tabItem(
      tabName = "retention",
      fluidRow(
        box(
          title = "Retention Curve",
          width = 12,
          plotly::plotlyOutput("retention_plot", height = "600px")
        )
      )
    ),
    ### Cohort Retention
    shinydashboard::tabItem(
      tabName = "cohort",
      fluidRow(
        box(
          title = "Cohort Retention",
          width = 12,
          plotly::plotlyOutput("cohort_heatmap", height = "500px")
        )
      )
    )
  )
))

# Define Server
server <- function(input, output, session) {
  
  ## Clear Filters Reactivity
  shiny::observeEvent(input$clear_filters, {
    shinyWidgets::updateAirDateInput(
      session,
      inputId = "date_range",
      value = c(min_date, max_date)
    )
    shiny::updateSelectizeInput(session, "genre", selected = NULL, server = T)
    shiny::updateSelectizeInput(session, "artist", selected = NULL, server = T)
    shiny::updateSelectizeInput(session, "country", selected = NULL, server = T)
    shiny::updateSelectInput(session, "metric", selected = "revenue")
  })
  
  ## Reactive query builder (Time Series)
  ts_data <- reactive({
    
    ### Filters
    filters <- c()
    #### Date Range
    if (!is.null(input$date_range)) {
      ##### Whole months (ignore day)
      start_date <- format(
        floor_date(as.Date(input$date_range[1]), "month"), 
        "%Y-%m-%d"
        )
      end_date <- format(
        ceiling_date(as.Date(input$date_range[2]), "month") - days(1), 
        "%Y-%m-%d"
        )
      filters <- c(
        filters, 
        paste0(
          "DATE(i.InvoiceDate) BETWEEN DATE('", 
          start_date, 
          "') AND DATE('", 
          end_date, 
          "')"
          )
        )
    }
    #### Country
    if (!is.null(input$country) && length(input$country) > 0) {
      countries <- paste(paste0("'", input$country, "'"), collapse = ", ")
      filters <- c(filters, paste0("i.BillingCountry IN (", countries, ")"))
    }
    #### Genre
    if (!is.null(input$genre) && length(input$genre) > 0){
      #### Ensure a "NULL" (0 length) selection is treated as "All"
      if (length(input$genre) > 0 ){
        genres <- paste(paste0("'", input$genre, "'"), collapse = ", ")
        filters <- c(filters, paste0("g.Name IN (", genres, ")"))
      }
    }
    #### Artist
    if (!is.null(input$artist) && length(input$artist) > 0){
      #### Ensure a "NULL" (0 length) selection is treated as "All"
      if (length(input$artist) > 0 ){
        artists <- paste(paste0("'", input$artist, "'"), collapse = ", ")
        filters <- c(filters, paste0("ar.Name IN (", artists, ")"))
      }
    }
    
    ### Form "Where" clause to filter SQL query
    where_clause <- if (length(filters) > 0) {
      paste("WHERE", paste(filters, collapse = " AND "))
    } else {
      ""
    }
    
    ### SQL Query
    query <- paste0("
    -- Flag the first purchase for each customer
    WITH first_invoices AS (
      SELECT 
        CustomerId,
        MIN(InvoiceDate) AS first_invoice_date
      FROM Invoice
      GROUP BY CustomerId
    ),
    -- filter data set based on selectors, make KPI some KPI cols
    filtered_data AS (
      SELECT 
        STRFTIME('%Y-%m', i.InvoiceDate) AS month,
        i.CustomerId,
        i.InvoiceDate,
        COUNT(DISTINCT i.InvoiceId) AS num_purchases,
        SUM(il.Quantity) AS tracks_sold,
        SUM(il.UnitPrice * il.Quantity) AS revenue
      FROM Invoice i
      JOIN InvoiceLine il ON i.InvoiceId = il.InvoiceId
      JOIN Track t ON il.TrackId = t.TrackId
      JOIN Genre g ON t.GenreId = g.GenreId
      JOIN Album al ON t.AlbumId = al.AlbumId
      JOIN Artist ar ON al.ArtistId = ar.ArtistId
      -- filter based on selectors
      ",
      if(nchar(where_clause) > 0) paste0(where_clause, " ") else "", 
      "GROUP BY month, i.CustomerId, i.InvoiceDate
    )
    -- Aggregate and format kpi cols
    SELECT
      fd.month,
      fd.CustomerId AS customer_id,
      fd.InvoiceDate AS invoice_date,
      fd.num_purchases,
      fd.tracks_sold,
      fd.revenue,
      CASE 
        WHEN STRFTIME('%Y-%m', fi.first_invoice_date) = fd.month THEN 1
        ELSE 0
      END AS first_time_customer
    FROM filtered_data fd
    JOIN first_invoices fi ON fd.CustomerId = fi.CustomerId
    ORDER BY fd.month;"
    )

    DBI::dbGetQuery(con, query)
  })
  
  ## Aggregate data for KPIs and time series
  aggregated_data <- reactive({
    df <- ts_data()
    
    ### Calculate KPIs on monthly basis
    df_monthly <- df |>
      dplyr::group_by(month) |>
      dplyr::summarise(
        revenue = sum(revenue, na.rm = TRUE),
        num_purchases = sum(num_purchases, na.rm = TRUE),
        tracks_sold = sum(tracks_sold, na.rm = TRUE),
        num_customers = dplyr::n_distinct(customer_id),
        first_time_customers = sum(first_time_customer, na.rm = TRUE),
        .groups = "drop"
      )

    list(
      raw = df,
      monthly = df_monthly
    )
  })
  
  ## Reactive query builder (Choropleth)
  choropleth_data <- reactive({
    ### Filters
    filters <- c()
    #### Date Range
    if (!is.null(input$date_range)) {
      ##### Whole months (ignore day)
      start_date <- format(
        floor_date(as.Date(input$date_range[1]), "month"), 
        "%Y-%m-%d"
      )
      end_date <- format(
        ceiling_date(as.Date(input$date_range[2]), "month") - days(1), 
        "%Y-%m-%d"
      )
      filters <- c(
        filters, 
        paste0(
          "DATE(i.InvoiceDate) BETWEEN DATE('", 
          start_date, 
          "') AND DATE('", 
          end_date, 
          "')"
        )
      )
    }
    #### Genre
    if (!is.null(input$genre) && length(input$genre) > 0){
      #### Ensure a "NULL" (0 length) selection is treated as "All"
      if (length(input$genre) > 0 ){
        genres <- paste(paste0("'", input$genre, "'"), collapse = ", ")
        filters <- c(filters, paste0("g.Name IN (", genres, ")"))
      }
    }
    #### Artist
    if (!is.null(input$artist) && length(input$artist) > 0){
      #### Ensure a "NULL" (0 length) selection is treated as "All"
      if (length(input$artist) > 0 ){
        artists <- paste(paste0("'", input$artist, "'"), collapse = ", ")
        filters <- c(filters, paste0("ar.Name IN (", artists, ")"))
      }
    }
    ### Form "Where" clause to filter SQL query
    where_clause <- if (length(filters) > 0) {
      paste("WHERE", paste(filters, collapse = " AND "))
    } else {
      ""
    }
    
    query <- paste0("
    -- Flag the first purchase for each customer
    WITH first_invoices AS (
      SELECT CustomerId, MIN(InvoiceDate) AS first_invoice_date
      FROM Invoice GROUP BY CustomerId
    ),
    -- filter data set based on selectors, make KPI some KPI cols
    filtered_data AS (
      SELECT 
        STRFTIME('%Y', i.InvoiceDate) AS year,
        i.BillingCountry AS country,
        i.CustomerId,
        COUNT(DISTINCT i.InvoiceId) AS num_purchases,
        SUM(il.Quantity) AS tracks_sold,
        SUM(il.UnitPrice * il.Quantity) AS revenue
      FROM Invoice i
      JOIN InvoiceLine il ON i.InvoiceId = il.InvoiceId
      JOIN Track t ON il.TrackId = t.TrackId
      JOIN Genre g ON t.GenreId = g.GenreId
      JOIN Album al ON t.AlbumId = al.AlbumId
      JOIN Artist ar ON al.ArtistId = ar.ArtistId
      -- filter based on selectors
      ", 
      if(nchar(where_clause) > 0) paste0(where_clause, " ") else "", 
      "
      GROUP BY year, country, i.CustomerId
    )
    -- Aggregate and format kpi cols
    SELECT
      filtered_data.year,
      filtered_data.country,
      COUNT(DISTINCT filtered_data.CustomerId) AS num_customers,
      SUM(num_purchases) AS num_purchases,
      SUM(tracks_sold) AS tracks_sold,
      SUM(revenue) AS revenue,
      SUM(CASE 
        WHEN filtered_data.year = STRFTIME('%Y', fi.first_invoice_date) THEN 1
        ELSE 0 END) AS first_time_customers
    FROM filtered_data
    LEFT JOIN first_invoices fi ON filtered_data.CustomerId = fi.CustomerId
    GROUP BY year, country
    ORDER BY year, country;
  ")
    
    DBI::dbGetQuery(con, query)
  })
  
  ## Reactive query builder (Genre)
genre_data <- reactive ({    
  ### Filters
  filters <- c()
  #### Date Range
  if (!is.null(input$date_range)) {
    ##### Whole months (ignore day)
    start_date <- format(
      floor_date(as.Date(input$date_range[1]), "month"), 
      "%Y-%m-%d"
    )
    end_date <- format(
      ceiling_date(as.Date(input$date_range[2]), "month") - days(1), 
      "%Y-%m-%d"
    )
    filters <- c(
      filters, 
      paste0(
        "DATE(i.InvoiceDate) BETWEEN DATE('", 
        start_date, 
        "') AND DATE('", 
        end_date, 
        "')"
      )
    )
  }
  #### Country
  if (!is.null(input$country) && length(input$country) > 0) {
    countries <- paste(paste0("'", input$country, "'"), collapse = ", ")
    filters <- c(filters, paste0("i.BillingCountry IN (", countries, ")"))
  }
  #### Genre
  if (!is.null(input$genre) && length(input$genre) > 0){
    #### Ensure a "NULL" (0 length) selection is treated as "All"
    if (length(input$genre) > 0 ){
      genres <- paste(paste0("'", input$genre, "'"), collapse = ", ")
      filters <- c(filters, paste0("g.Name IN (", genres, ")"))
    }
  }
  #### Artist
  if (!is.null(input$artist) && length(input$artist) > 0){
    #### Ensure a "NULL" (0 length) selection is treated as "All"
    if (length(input$artist) > 0 ){
      artists <- paste(paste0("'", input$artist, "'"), collapse = ", ")
      filters <- c(filters, paste0("ar.Name IN (", artists, ")"))
    }
  }
  
  ### Form "Where" clause to filter SQL query
  where_clause <- if (length(filters) > 0) {
    paste("WHERE", paste(filters, collapse = " AND "))
  } else {
    ""
  }
  
  ### SQL Query
  query <- paste0("
  -- Get tracks in catalog by genre
  WITH catalog AS (
    SELECT 
        g.Name AS genre,
        COUNT(DISTINCT t.TrackId) AS tracks_in_catalog
    FROM Track t
    JOIN Genre g ON t.GenreId = g.GenreId
    GROUP BY g.Name
  )
  SELECT 
      STRFTIME('%Y', i.InvoiceDate) AS year,
      g.Name AS genre,
      COUNT(DISTINCT i.CustomerId) AS num_customers,
      COUNT(DISTINCT i.InvoiceId) AS num_purchases,
      SUM(il.Quantity) AS tracks_sold,
      SUM(il.UnitPrice * il.Quantity) AS revenue,
      c.tracks_in_catalog
  FROM Invoice i
  JOIN InvoiceLine il ON i.InvoiceId = il.InvoiceId
  JOIN Track t ON il.TrackId = t.TrackId
  JOIN Genre g ON t.GenreId = g.GenreId
  JOIN Album al ON t.AlbumId = al.AlbumId
  JOIN Artist ar ON al.ArtistId = ar.ArtistId
  LEFT JOIN catalog c ON g.Name = c.genre
  ",
  if(nchar(where_clause) > 0) paste0(" ", where_clause, " ") else " ",
  "GROUP BY STRFTIME('%Y', i.InvoiceDate), g.Name, c.tracks_in_catalog
  ORDER BY year, genre;
  ")
  
  DBI::dbGetQuery(con, query)
  })

  ## Reactive query builder (Artist)
artist_data <- reactive ({    
  ### Filters
  filters <- c()
  #### Date Range
  if (!is.null(input$date_range)) {
    ##### Whole months (ignore day)
    start_date <- format(
      floor_date(as.Date(input$date_range[1]), "month"), 
      "%Y-%m-%d"
    )
    end_date <- format(
      ceiling_date(as.Date(input$date_range[2]), "month") - days(1), 
      "%Y-%m-%d"
    )
    filters <- c(
      filters, 
      paste0(
        "DATE(i.InvoiceDate) BETWEEN DATE('", 
        start_date, 
        "') AND DATE('", 
        end_date, 
        "')"
      )
    )
  }
  #### Country
  if (!is.null(input$country) && length(input$country) > 0) {
    countries <- paste(paste0("'", input$country, "'"), collapse = ", ")
    filters <- c(filters, paste0("i.BillingCountry IN (", countries, ")"))
  }
  #### Genre
  if (!is.null(input$genre) && length(input$genre) > 0){
    #### Ensure a "NULL" (0 length) selection is treated as "All"
    if (length(input$genre) > 0 ){
      genres <- paste(paste0("'", input$genre, "'"), collapse = ", ")
      filters <- c(filters, paste0("g.Name IN (", genres, ")"))
    }
  }
  #### Artist
  if (!is.null(input$artist) && length(input$artist) > 0){
    #### Ensure a "NULL" (0 length) selection is treated as "All"
    if (length(input$artist) > 0 ){
      artists <- paste(paste0("'", input$artist, "'"), collapse = ", ")
      filters <- c(filters, paste0("ar.Name IN (", artists, ")"))
    }
  }
  
  ### Form "Where" clause to filter SQL query
  where_clause <- if (length(filters) > 0) {
    paste("WHERE", paste(filters, collapse = " AND "))
  } else {
    ""
  }
  
  ### SQL Query
  query <- paste0("
  -- Get tracks in catalog by artist
  WITH catalog AS (
    SELECT 
        ar.Name AS artist,
        COUNT(DISTINCT t.TrackId) AS tracks_in_catalog
    FROM Track t
    JOIN Album al ON t.AlbumId = al.AlbumId
    JOIN Artist ar ON al.ArtistId = ar.ArtistId
    GROUP BY ar.Name
  )
  SELECT 
      STRFTIME('%Y', i.InvoiceDate) AS year,
      ar.Name AS artist,
      COUNT(DISTINCT i.CustomerId) AS num_customers,
      COUNT(DISTINCT i.InvoiceId) AS num_purchases,
      SUM(il.Quantity) AS tracks_sold,
      SUM(il.UnitPrice * il.Quantity) AS revenue,
      c.tracks_in_catalog
  FROM Invoice i
  JOIN InvoiceLine il ON i.InvoiceId = il.InvoiceId
  JOIN Track t ON il.TrackId = t.TrackId
  JOIN Album al ON t.AlbumId = al.AlbumId
  JOIN Artist ar ON al.ArtistId = ar.ArtistId
  LEFT JOIN catalog c ON ar.Name = c.artist
  ",
   if(nchar(where_clause) > 0) paste0(" ", where_clause, " ") else " ",
                  "GROUP BY STRFTIME('%Y', i.InvoiceDate), ar.Name, c.tracks_in_catalog
  ORDER BY year, artist;
  ")
  
  DBI::dbGetQuery(con, query)
})

  ## Reactive query builder (Retention Decay Curve)
retention_data <- reactive ({    
  ### Filters
  filters <- c()
  #### Date Range
  if (!is.null(input$date_range)) {
    ##### Whole months (ignore day)
    start_date <- format(
      floor_date(as.Date(input$date_range[1]), "month"), 
      "%Y-%m-%d"
    )
    end_date <- format(
      ceiling_date(as.Date(input$date_range[2]), "month") - days(1), 
      "%Y-%m-%d"
    )
    filters <- c(
      filters, 
      paste0(
        "DATE(i.InvoiceDate) BETWEEN DATE('", 
        start_date, 
        "') AND DATE('", 
        end_date, 
        "')"
      )
    )
  }
  #### Country
  if (!is.null(input$country) && length(input$country) > 0) {
    countries <- paste(paste0("'", input$country, "'"), collapse = ", ")
    filters <- c(filters, paste0("i.BillingCountry IN (", countries, ")"))
  }
  #### Artist
  if (!is.null(input$artist) && length(input$artist) > 0){
    #### Ensure a "NULL" (0 length) selection is treated as "All"
    if (length(input$artist) > 0 ){
      artists <- paste(paste0("'", input$artist, "'"), collapse = ", ")
      filters <- c(filters, paste0("ar.Name IN (", artists, ")"))
    }
  }
  #### Artist
  if (!is.null(input$artist) && length(input$artist) > 0){
    #### Ensure a "NULL" (0 length) selection is treated as "All"
    if (length(input$artist) > 0 ){
      artists <- paste(paste0("'", input$artist, "'"), collapse = ", ")
      filters <- c(filters, paste0("ar.Name IN (", artists, ")"))
    }
  }
  
  ### Form "Where" clause to filter SQL query
  where_clause <- if (length(filters) > 0) {
    paste("WHERE", paste(filters, collapse = " AND "))
  } else {
    ""
  }
  
  ### SQL Query
  query <- paste0("
  WITH filtered_data AS (
    SELECT 
      i.CustomerId,
      i.InvoiceDate,
      ar.Name AS ArtistName
    FROM Invoice i
    JOIN InvoiceLine il ON i.InvoiceId = il.InvoiceId
    JOIN Track t ON il.TrackId = t.TrackId
    JOIN Genre g ON t.GenreId = g.GenreId
    JOIN Album al ON t.AlbumId = al.AlbumId
    JOIN Artist ar ON al.ArtistId = ar.ArtistId
  ", 
    if(nchar(where_clause) > 0) paste0(" ", where_clause, " ") else " ",
    "),

  customer_first AS (
    SELECT 
      CustomerId,
      DATE_TRUNC('month', MIN(InvoiceDate)) AS first_month
    FROM filtered_data
    GROUP BY CustomerId
  ),

  customer_activity AS (
    SELECT 
      f.CustomerId,
      DATE_TRUNC('month', f.InvoiceDate) AS invoice_month,
      cf.first_month,
      (YEAR(f.InvoiceDate) - YEAR(cf.first_month)) * 12 +
      (MONTH(f.InvoiceDate) - MONTH(cf.first_month)) AS months_since_first
    FROM filtered_data f
    JOIN customer_first cf ON f.CustomerId = cf.CustomerId
  )

  SELECT 
    months_since_first,
    COUNT(DISTINCT CustomerId) * 100.0 / (
      SELECT COUNT(DISTINCT CustomerId) FROM customer_first
    ) AS retention_pct
  FROM customer_activity
  GROUP BY months_since_first
  ORDER BY months_since_first;
  ")
  
  DBI::dbGetQuery(con, query)
})

  ## Reactive query builder (Cohort Retention)
cohort_data <-  reactive ({    
  ### Filters
  filters <- c()
  #### Date Range
  if (!is.null(input$date_range)) {
    ##### Whole months (ignore day)
    start_date <- format(
      floor_date(as.Date(input$date_range[1]), "month"), 
      "%Y-%m-%d"
    )
    end_date <- format(
      ceiling_date(as.Date(input$date_range[2]), "month") - days(1), 
      "%Y-%m-%d"
    )
    filters <- c(
      filters, 
      paste0(
        "DATE(i.InvoiceDate) BETWEEN DATE('", 
        start_date, 
        "') AND DATE('", 
        end_date, 
        "')"
      )
    )
  }
  #### Country
  if (!is.null(input$country) && length(input$country) > 0) {
    countries <- paste(paste0("'", input$country, "'"), collapse = ", ")
    filters <- c(filters, paste0("i.BillingCountry IN (", countries, ")"))
  }
  #### Artist
  if (!is.null(input$artist) && length(input$artist) > 0){
    #### Ensure a "NULL" (0 length) selection is treated as "All"
    if (length(input$artist) > 0 ){
      artists <- paste(paste0("'", input$artist, "'"), collapse = ", ")
      filters <- c(filters, paste0("ar.Name IN (", artists, ")"))
    }
  }
  #### Artist
  if (!is.null(input$artist) && length(input$artist) > 0){
    #### Ensure a "NULL" (0 length) selection is treated as "All"
    if (length(input$artist) > 0 ){
      artists <- paste(paste0("'", input$artist, "'"), collapse = ", ")
      filters <- c(filters, paste0("ar.Name IN (", artists, ")"))
    }
  }
  
  ### Form "Where" clause to filter SQL query
  where_clause <- if (length(filters) > 0) {
    paste("WHERE", paste(filters, collapse = " AND "))
  } else {
    ""
  }
  
  ### SQL Query
  query <- paste0("
    WITH customer_first AS (
      SELECT 
        i.CustomerId, 
        DATE_TRUNC('month', MIN(i.InvoiceDate)) AS cohort_month
      FROM Invoice i
      JOIN InvoiceLine il ON i.InvoiceId = il.InvoiceId
      JOIN Track t ON il.TrackId = t.TrackId
      JOIN Album al ON t.AlbumId = al.AlbumId
      JOIN Artist ar ON al.ArtistId = ar.ArtistId 
    -- filter based on selectors
", 
                  if(nchar(where_clause) > 0) paste0(where_clause, " ") else "",
                  "GROUP BY i.CustomerId
    ),
    
    customer_activity AS (
      SELECT 
        i.CustomerId,
        DATE_TRUNC('month', i.InvoiceDate) AS invoice_month,
        cf.cohort_month,
        (YEAR(i.InvoiceDate) - YEAR(cf.cohort_month)) * 12 +
    (MONTH(i.InvoiceDate) - MONTH(cf.cohort_month)) AS months_since_first
      FROM Invoice i
      JOIN InvoiceLine il ON i.InvoiceId = il.InvoiceId
      JOIN Track t ON il.TrackId = t.TrackId
      JOIN Album al ON t.AlbumId = al.AlbumId
      JOIN Artist ar ON al.ArtistId = ar.ArtistId
      JOIN customer_first cf ON i.CustomerId = cf.CustomerId
      ", if (nchar(where_clause) > 0) paste0(where_clause, " ") else " ", "
    ),
    
    cohort_sizes AS (
      SELECT 
        cohort_month, 
        COUNT(DISTINCT CustomerId) AS cohort_size
      FROM customer_first
      GROUP BY cohort_month
    )

    SELECT 
      ca.cohort_month AS CohortMonth,
      ca.months_since_first AS MonthsSinceFirst,
      cs.cohort_size AS CohortSize,
      COUNT(DISTINCT ca.CustomerId) * 100.0 / cs.cohort_size AS RetentionPct
    FROM customer_activity ca
    JOIN cohort_sizes cs ON ca.cohort_month = cs.cohort_month
    GROUP BY CohortMonth, MonthsSinceFirst, CohortSize
    ORDER BY CohortMonth, MonthsSinceFirst;
  ")
  
  DBI::dbGetQuery(con, query)
})

  ## KPI Boxes
  ### Total Revenue
  output$kpi_revenue <- shinydashboard::renderInfoBox({
    df <- ts_data()
    val <- sum(df$revenue, na.rm = TRUE)
    
    shinydashboard::infoBox(
      "Total Revenue", 
      scales::dollar(val), 
      icon = shiny::icon("dollar-sign"), 
      color = "green"
      )
  })
  ### Unique Customers
  output$kpi_customers <- renderInfoBox({
    df <- ts_data()
    shinydashboard::infoBox(
      "Unique Customers", 
      dplyr::n_distinct(df$customer_id), 
      icon = shiny::icon("users"), 
      color = "blue"
    )
  })
  ### First-Time Customers
  output$kpi_firsttime <- renderInfoBox({
    df <- ts_data()
    val <- sum(df$first_time_customer, na.rm = TRUE)
    shinydashboard::infoBox(
      "First-Time Customers", 
      val,
      icon = shiny::icon("user-plus"), 
      color = "light-blue"
    )
  })
  ### Number of Purchases (Invoices)
  output$kpi_purchases <- renderInfoBox({
    df <- ts_data()
    shinydashboard::infoBox(
      "Purchases", 
      dplyr::n_distinct(df$invoice_date), 
      icon = shiny::icon("file-invoice"), 
      color = "purple"
    )
  })
  ### Number of Tracks (Units) Sold
  output$kpi_tracks <- shinydashboard::renderInfoBox({
    df <- ts_data()
    shinydashboard::infoBox(
      "Tracks Sold", 
      sum(df$tracks_sold, na.rm = TRUE), 
      icon = shiny::icon("music"), 
      color = "orange"
      )
  })
  ### Average Revenue per Purchase (Invoice)
  output$kpi_avg_rev_per_purchase <- shinydashboard::renderInfoBox({
    df <- ts_data()
    avg <- sum(df$revenue) / dplyr::n_distinct(df$invoice_date)
    shinydashboard::infoBox(
      "Avg Revenue / Purchase", 
      scales::dollar(avg), 
      icon = shiny::icon("money-check-alt"), 
      color = "yellow"
      )
  })

# Plots
  ## Time Series Plot
  output$ts_plot <- plotly::renderPlotly({
    ### Monthly Data
    df <- aggregated_data()$monthly
    req(nrow(df) > 0)
    
    ### Y-Variable Selection
    y_var <- input$metric
    y_label <- dplyr::case_when(
      y_var == "revenue" ~ "Total Revenue (USD$)",
      y_var == "num_customers" ~ "Number of Customers",
      y_var == "first_time_customers" ~ "Number of First-Time Customers",
      y_var == "num_purchases" ~ "Number of Purchases",
      y_var == "tracks_sold" ~ "Tracks Sold"
    )
    
    ### Create the base ggplot - Skip "text" warning
    p <- suppressWarnings(
        ggplot2::ggplot(
          df, 
          ggplot2::aes(x = lubridate::ym(month), y = .data[[y_var]])
          ) +
      ### Line Plot    
      ggplot2::geom_line(color = line_color, size = line_thickness) +
      ### Add points with hover text for select KPIs
      ggplot2::geom_point(
        ggplot2::aes(
          text = paste0(
            "Month: ", month, "<br>",
            "Revenue: ", scales::dollar(revenue), "<br>",
            "Purchases: ", num_purchases, "<br>",
            "Tracks Sold: ", tracks_sold, "<br>",
            "Unique Customers: ", num_customers, "<br>",
            "First-Time Customers: ", first_time_customers, "<br>",
            "Revenue per Customer: ", scales::dollar(revenue/num_customers)
            )
          ), 
        color = point_color, 
        size = point_thickness
        ) +
      ### Ensure Y scale is continuous
      ggplot2::scale_y_continuous(labels = scales::comma) +
      ### Labels
      ggplot2::labs(
        title = paste(y_label, "Over Time"),
        x = "Time (Months)",
        y = y_label) +
      ggplot2::theme_minimal()
    )
    
    # Convert to plotly with custom hover text
    plotly::ggplotly(p, tooltip = "text") %>%
      plotly::layout(
        hovermode = "x unified",
        xaxis = list(
          tickformat = "%b %Y",
          nticks = 12
        )
      )
  })
  
  ## Choropleth Plot
  output$choropleth_plot <- plotly::renderPlotly({
    df <- choropleth_data() |>
      ## Add ISO Country Codes
      dplyr::mutate(
        iso_alpha = countrycode::countrycode(
          country, 
          origin = 'country.name', 
          destination = 'iso3c'
        )
      )
    req(nrow(df) > 0)

    # Add "All" row, aggregating across all years present
    df <- dplyr::bind_rows(
      df |> dplyr::mutate(
        year = as.character(year)
      ),
      df |>
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
    
    # Make 'year' into an ordered factor, with 'All' last in animation
    year_vals <- c(sort(unique(df$year[df$year != "All"])), "All")
    df <- df |>
      dplyr::mutate(year = factor(year, levels = year_vals))
    
    ## Parse "Y-Var" selection
    y_var <- input$metric
    y_label <- dplyr::case_when(
      y_var == "revenue" ~ "Total Revenue (USD$)",
      y_var == "num_customers" ~ "Number of Customers",
      y_var == "first_time_customers" ~ "Number of First-Time Customers",
      y_var == "num_purchases" ~ "Number of Purchases",
      y_var == "tracks_sold" ~ "Tracks Sold"
    )
    
    ## Hover Text
    df$hover <- paste0(
      "Country: ", df$country, "<br>",
      "Year: ", df$year, "<br>",
      "Revenue: ", scales::dollar(df$revenue), "<br>",
      "Purchases: ", df$num_purchases, "<br>",
      "Tracks Sold: ", df$tracks_sold, "<br>",
      "Unique Customers: ", df$num_customers, "<br>",
      "First Time Customers: ", df$first_time_customers, "<br>",
      "Revenue per Customer: ", scales::dollar(df$revenue/df$num_customers)
    )
    
    plotly::plot_ly(
      data = df,
      type = 'choropleth',
      locations = ~iso_alpha,
      z = df[[y_var]],
      # Set minimum and maximum Z values, for consistent scale
      zmin = min(df[[y_var]]),
      zmax = max(df[[y_var]]),
      # Hover Text
      text = df$hover,
      hoverinfo = "text",
      # Colorblind friendly color scheme
      colorscale = "Viridis",
      reversescale = TRUE,
      showscale = TRUE,
      ## Label legend
      colorbar = list(
        title = y_label
      ),
      # Give national boundaries a dark gray outline
      marker = list(line = list(color = "darkgrey", width = 0.5)),
      frame = ~year
    ) %>%
      plotly::layout(
        geo = list(
          projection = list(type = 'natural earth'),
          showframe = FALSE
        ),
        title = paste(y_label, "by Country (Animated by Year)")
      )
  })

# Genre Bar Plot
output$genre_bar_plot <- plotly::renderPlotly({
    df <- genre_data()
    req(nrow(df) > 0) 
    
    # Define allowed metrics
    allowed_metrics <- c(
      "revenue", 
      "num_customers",
      "num_purchases", 
      "tracks_sold"
    )
    
    # Default to "revenue" if unsupported metric
    y_var <- if (input$metric %in% allowed_metrics) {
      input$metric
    } else {
      "revenue"
    }
    y_label <- case_when(
      y_var == "revenue" ~ "Total Revenue (USD$)",
      y_var == "num_customers" ~ "Number of Customers",
      y_var == "num_purchases" ~ "Number of Purchases",
      y_var == "tracks_sold" ~ "Tracks Sold"
    )
    
    # Format year as factor so it orders correctly
    df$year <- factor(df$year, levels = sort(unique(df$year)))
    
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
      "Revenue per Track: ", scales::dollar(df$revenue / df$tracks_in_catalog), "<br>",
      "Percentage of Catalog Sold: ", scales::percent(df$tracks_sold / df$tracks_in_catalog, accuracy = 0.1)
    )
    
    
    # Make ggplot stacked bar chart - suppress "text" warning
    p <- suppressWarnings(ggplot2::ggplot(
      df, 
      ggplot2::aes(x = genre, y = .data[[y_var]], fill = year, text = hover_text)) +
      # Make stacked bar plots for aggregate view
      ggplot2::geom_bar(stat = "identity", position = "stack") +
      # Format y-axis for legibility
      ggplot2::scale_y_continuous(labels = scales::comma) +
      # Colorblind friendly color scheme
      ggplot2::scale_fill_viridis_d() +
      # Labels
      ggplot2::labs(
        title = paste(y_label, "by Genre"),
        x = "Genre",
        y = y_label,
        fill = "Year"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "bottom",
        # Angle x-axis labels for legibility
        axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
      ))
    
    plotly::ggplotly(p, tooltip = "text")
    
    })

  ## Artist Bar Plot
output$artist_bar_plot <- plotly::renderPlotly({
  df <- artist_data()
  req(nrow(df) > 0) 
  
  # Define allowed metrics
  allowed_metrics <- c(
    "revenue", 
    "num_customers",
    "num_purchases", 
    "tracks_sold"
  )
  
  # Default to "revenue" if unsupported metric
  y_var <- if (input$metric %in% allowed_metrics) {
    input$metric
  } else {
    "revenue"
  }
  y_label <- case_when(
    y_var == "revenue" ~ "Total Revenue (USD$)",
    y_var == "num_customers" ~ "Number of Customers",
    y_var == "num_purchases" ~ "Number of Purchases",
    y_var == "tracks_sold" ~ "Tracks Sold"
  )
  
  # Format year as factor so it orders correctly
  df$year <- factor(df$year, levels = sort(unique(df$year)))
  
  # Dynamically sort artists by aggregate of selected metric
  df <- df |>
    dplyr::group_by(artist) |>
    dplyr::mutate(total_metric = sum(.data[[y_var]], na.rm = TRUE)) |>
    dplyr::ungroup() |>
    ## Filter to only top 20 for legibility
    dplyr::filter(artist %in% (
      df |>
        dplyr::group_by(artist) |>
        dplyr::summarise(total_metric = sum(.data[[y_var]], na.rm = TRUE)) |>
        dplyr::arrange(desc(total_metric)) |>
        dplyr::slice_head(n = 20) |>
        dplyr::pull(artist)
    )) |>
    dplyr::mutate(artist = reorder(artist, -total_metric))
  
  # Add hover text
  df$hover_text <- paste0(
    "Artist: ", df$artist, "<br>",
    "Year: ", df$year, "<br>",
    "Total Revenue (USD$): ", scales::dollar(df$revenue), "<br>",
    "Purchases: ", df$num_purchases, "<br>",
    "Tracks Sold: ", df$tracks_sold, "<br>",
    "Tracks in Catalog: ", df$tracks_in_catalog, "<br>",
    "Revenue per Track: ", scales::dollar(df$revenue / df$tracks_in_catalog), "<br>",
    "Percentage of Catalog Sold: ", scales::percent(df$tracks_sold / df$tracks_in_catalog, accuracy = 0.1)
  )
  
  
  # Make ggplot stacked bar chart - suppress "text" warning
  p <- suppressWarnings(ggplot2::ggplot(
    df, 
    ggplot2::aes(x = artist, y = .data[[y_var]], fill = year, text = hover_text)) +
      # Make stacked bar plots for aggregate view
      ggplot2::geom_bar(stat = "identity", position = "stack") +
      # Format y-axis for legibility
      ggplot2::scale_y_continuous(labels = scales::comma) +
      # Colorblind friendly color scheme
      ggplot2::scale_fill_viridis_d() +
      # Labels
      ggplot2::labs(
        title = paste(y_label, "by Artist"),
        x = "Artist",
        y = y_label,
        fill = "Year"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "bottom",
        # Angle x-axis labels for legibility
        axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
      ))
  
  plotly::ggplotly(p, tooltip = "text")
  
})

  ## Retention Decay Curve
output$retention_plot <- plotly::renderPlotly({
  df <- retention_data()
  req(nrow(df) > 0)
  
  # Add hover text
  df$hover_text <- paste0(
    "Month: ", df$months_since_first, "<br>",
    "Retention: ", round(df$retention_pct, 1), "%"
  ) 
  
  # Omit the month of the purchase (not logical)
  df <- df |>
    dplyr::filter(months_since_first > 0)
  
  # Create ggplot
  p <- ggplot2::ggplot(
    df, 
    ggplot2::aes(x = months_since_first, y = retention_pct)) +
    # Add lines with dots - hover text on dots
    ggplot2::geom_line(color = line_color, size = line_thickness) +
    ggplot2::geom_point(color = point_color, size = point_thickness, ggplot2::aes(text = hover_text)) +
    # Format x axis to have a tick every 3 months
    ggplot2::scale_x_continuous(breaks = seq(0, max(df$months_since_first, na.rm = TRUE), by = 3)) +
    # Format y-axis as percentages
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100)) +
    # Labels
    ggplot2::labs(
      title = "Customer Retention Decay",
      x = "Months Since First Purchase",
      y = "Retention (%)"
    ) +
    ggplot2::theme_minimal(base_size = 14)
  
  plotly::ggplotly(p, tooltip = "text")
})

  ## Cohort Retention Heatmap
output$cohort_heatmap <- plotly::renderPlotly({
  req(cohort_data())
  
  res_cohort_df <- cohort_data()
  
  # Ensure all Cohort-Months are represented
  res_cohort_df <- tidyr::complete(
    res_cohort_df,
    CohortMonth = seq.Date(
      min(res_cohort_df$CohortMonth, na.rm = TRUE),
      max(res_cohort_df$CohortMonth, na.rm = TRUE),
      by = "month"
    ),
    MonthsSinceFirst = seq(
      min(res_cohort_df$MonthsSinceFirst, na.rm = TRUE),
      max(res_cohort_df$MonthsSinceFirst, na.rm = TRUE),
      by = 1
    )
  ) |> 
    dplyr::filter(MonthsSinceFirst > 0 & CohortMonth >= as.Date("2009-01-01")) |> 
    dplyr::mutate(
      hover_text = dplyr::if_else(
        !is.na(RetentionPct),
        paste0(
          "Cohort: ", format(CohortMonth, "%b %Y"), "\n",
          "Cohort Size: ", CohortSize, "\n",
          "Month: ", MonthsSinceFirst, "\n",
          "Retention: ", sprintf("%.2f%%", RetentionPct)
        ),
        paste0(
          "Cohort: ", format(CohortMonth, "%b %Y"), "\n",
          "Month: ", MonthsSinceFirst, "\n",
          "No data"
        )
      )
    )
  
  p <- ggplot2::ggplot(
    res_cohort_df,
    ggplot2::aes(
      x = MonthsSinceFirst,
      y = CohortMonth,
      fill = RetentionPct,
      text = hover_text
    )
  ) +
    geom_tile(color = "white", size = 0.2) +
    scale_fill_viridis_c(
      na.value = "gray90",
      option = "D",
      direction = -1
    ) +
    scale_x_continuous(breaks = pretty(res_cohort_df$MonthsSinceFirst)) +
    scale_y_date(date_breaks = "1 month", date_labels = "%b %Y") +
    labs(
      title = "Customer Retention by Cohort",
      x = "Months Since First Purchase",
      y = "Cohort Month (First Purchase)",
      fill = "Retention (%)"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )
  
  plotly::ggplotly(p, tooltip = "text")
})

}

# Run the app
shiny::shinyApp(ui = ui, server = server)
