#' @file sql_query_funs.R
#' @title SQL Query Helpers for Chinook Dashboard
#'
#' Defines helper functions used for querying and filtering the Chinook 
#' dataset via DuckDB. Includes reusable utilities for computing summary 
#' metrics, building dynamic WHERE clauses, and extracting module-specific 
#' data. These functions are primarily consumed by modular Shiny components.
#'
#' @seealso \code{\link{form_where_clause}}, \code{\link{get_summary_stats}}, 
#' \code{\link{get_ts_data}}, \code{\link{get_choropleth_data}},
#' \code{\link{get_genre_data}}
#' @keywords internal

#' Query Global Dataset Summary Stats
#'
#' Computes aggregate totals for purchases, customers, tracks sold, artists, 
#' genres, and countries using non-reactive SQL queries. Used for static 
#' metadata display.
#'
#' @param con A DBI-compliant database connection (e.g., DuckDB).
#'
#' @return A named list of summary statistics.
#' @examples
#' stats <- get_summary_stats(con)
#' stats$purchases
#' @keywords internal
get_summary_stats <- function(con) {
  list(
    purchases     = DBI::dbGetQuery(
      con, "SELECT COUNT(*) AS n FROM Invoice"
    )$n,
    customers     = DBI::dbGetQuery(
      con, "SELECT COUNT(DISTINCT CustomerId) AS n FROM Customer"
    )$n,
    date_range    = DBI::dbGetQuery(
      con, paste0(
        "SELECT MIN(InvoiceDate) AS min_date, MAX(InvoiceDate)",
        " AS max_date FROM Invoice"
      )),
    tracks_sold   = DBI::dbGetQuery(
      con, "SELECT COUNT(*) AS n FROM InvoiceLine"
    )$n,
    artists       = DBI::dbGetQuery(
      con, "SELECT COUNT(DISTINCT ArtistId) AS n FROM Artist"
    )$n,
    genres        = DBI::dbGetQuery(
      con, "SELECT COUNT(DISTINCT GenreId) AS n FROM Genre"
    )$n,
    countries     = DBI::dbGetQuery(
      con, "SELECT COUNT(DISTINCT Country) AS n FROM Customer"
    )$n
  )
}

#' Create Dynamic SQL WHERE Clause for Filters
#'
#' Dynamically builds a SQL WHERE clause based on optional filters including 
#' date range, country, genre, and artist. Each filter is individually 
#' validated and escaped using `glue_sql()` to prevent SQL injection and 
#' ensure database portability.
#'
#' @param date_range A length-2 Date vector specifying the start and end range.
#' @param country Character vector of selected billing countries.
#' @param genre Character vector of selected genres.
#' @param artist Character vector of selected artists.
#' @param .con A DBI connection passed to `glue_sql()`.
#'
#' @return A `DBI::SQL` object containing the WHERE clause, or empty string 
#' if no filters.
#' @seealso \code{\link{get_ts_data}}, \code{\link{get_choropleth_data}}
#' @keywords internal
form_where_clause <- function(
    date_range = NULL, 
    country = NULL, 
    genre = NULL, 
    artist = NULL,
    .con = NULL
) {
  clauses <- list()
  
  # Date range clause
  if (!is.null(date_range)) {
    start_date <- floor_date(as.Date(date_range[1]), "month")
    end_date <- ceiling_date(as.Date(date_range[2]), "month") - days(1)
    
    clauses$date <- glue_sql(
      "DATE(i.InvoiceDate) BETWEEN DATE({start_date}) AND DATE({end_date})",
      .con = .con
    )
  }
  
  # Country clause
  if (!is.null(country) && length(country) > 0) {
    clauses$country <- glue_sql(
      "i.BillingCountry IN ({country*})",
      .con = .con
    )
  }
  
  # Genre clause
  if (!is.null(genre) && length(genre) > 0) {
    clauses$genre <- glue_sql(
      "g.Name IN ({genre*})",
      .con = .con
    )
  }
  
  # Artist clause
  if (!is.null(artist) && length(artist) > 0) {
    clauses$artist <- glue_sql(
      "ar.Name IN ({artist*})",
      .con = .con
    )
  }
  
  # Combine clauses into full "WHERE" clause
  if (length(clauses) > 0) {
    where_clause <- DBI::SQL(
      paste("WHERE", paste(clauses, collapse = " AND "))
    )
  } else {
    where_clause <- DBI::SQL("")
  }
  
  # Return DBI::SQL object
  return(where_clause)
}

#' Get Filtered Time Series Invoice-Level Data
#'
#' Executes a SQL query that returns filtered invoice data grouped by month and customer.
#' KPI columns include revenue, number of purchases, tracks sold, country, and first-time
#' customer flag. Used to power the dashboard's time-series visualizations and KPIs.
#'
#' @param con A DBI connection to the Chinook database.
#' @param where_clause A `DBI::SQL` WHERE clause from `form_where_clause()`.
#'
#' @return A data frame with monthly invoice-level KPI columns.
#' @examples
#' ts <- get_ts_data(con, form_where_clause(date_range = c("2023-01-01", "2023-06-01"), .con = con))
#' head(ts)
#' @seealso \code{\link{form_where_clause}}
#' @keywords internal
get_ts_data <- function(con, where_clause) {
  query <- glue::glue_sql("
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
            i.BillingCountry,
            i.BillingState,
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
          {where_clause}
          GROUP BY month, i.CustomerId, i.InvoiceDate, i.BillingCountry,
          i.BillingState
    )
      -- Aggregate and format kpi cols
      SELECT
        fd.month,
        fd.CustomerId AS customer_id,
        fd.InvoiceDate AS invoice_date,
        fd.num_purchases,
        fd.tracks_sold,
        fd.revenue,
        fd.BillingCountry AS billing_country,
        fd.BillingState AS billing_state,
        CASE 
          WHEN STRFTIME('%Y-%m', fi.first_invoice_date) = fd.month THEN 1
          ELSE 0
        END AS first_time_customer
      FROM filtered_data fd
      JOIN first_invoices fi ON fd.CustomerId = fi.CustomerId
      ORDER BY fd.month;
      ", .con = con)
  
  DBI::dbGetQuery(con, query)
}

#' Get Country-Level Aggregated KPIs for Choropleth
#'
#' Returns year-by-country aggregates of key metrics: revenue, customers, 
#' purchases, tracks sold, and number of first-time customers. Used for the 
#' geographic distribution visualizations on the dashboard.
#'
#' @param con A DBI connection object.
#' @param where_clause A `DBI::SQL` WHERE clause generated from filters.
#'
#' @return A data frame with aggregated KPI columns by year and country.
#' @examples
#' df <- get_choropleth_data(
#'   con, form_where_clause(country = "USA", .con = con)
#'   )
#' @seealso \code{\link{form_where_clause}}
#' @keywords internal
get_choropleth_data <- function(con, where_clause){
  query <- glue::glue_sql("
    -- Flag the first purchase for each customer
    WITH first_invoices AS (
      SELECT CustomerId, MIN(InvoiceDate) AS first_invoice_date
      FROM Invoice GROUP BY CustomerId
    ),
    -- Filter data set based on selectors, make KPI some KPI cols
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
      {where_clause}
      GROUP BY year, country, i.CustomerId
    )
    -- Aggregate and format KPI cols
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
}

#' Get Genre-Level Aggregated KPIs for Genre Module
#'
#' Returns year-by-genre aggregates of key metrics: revenue, customers, 
#' purchases, tracks sold, and number of first-time customers. Used for the 
#' geographic distribution visualizations on the dashboard.
#'
#' @param con A DBI connection object.
#' @param where_clause A `DBI::SQL` WHERE clause generated from filters.
#'
#' @return A data frame with aggregated KPI columns by year and genre.
#' @examples
#' df <- get_genre_data(
#'   con, form_where_clause(country = "USA", .con = con)
#'   )
#' @seealso \code{\link{form_where_clause}}
#' @keywords internal
get_genre_data <- function(con, where_clause) {
  query <- glue::glue_sql("
    -- Get number of tracks in each genre in the catalog
    WITH catalog AS (
      SELECT 
        g.Name AS genre,
        COUNT(DISTINCT t.TrackId) AS tracks_in_catalog
      FROM Track t
      JOIN Genre g ON t.GenreId = g.GenreId
      GROUP BY g.Name
    ),
    -- Get date each track was first sold in the subset
    first_track_sold AS (
      SELECT 
        t.TrackId,
        g.Name AS genre,
        MIN(DATE(i.InvoiceDate)) AS first_sold_date
      FROM Invoice i
      JOIN InvoiceLine il ON i.InvoiceId = il.InvoiceId
      JOIN Track t ON il.TrackId = t.TrackId
      JOIN Genre g ON t.GenreId = g.GenreId
      -- filter based on selectors
      {where_clause}
      GROUP BY t.TrackId, g.Name
    ),
    -- First invoice date for each customer
      first_purchase AS (
        SELECT 
          CustomerId,
          MIN(DATE(InvoiceDate)) AS first_date
        FROM Invoice
        GROUP BY CustomerId
      )
    -- Filter data set based on selectors, make KPI some KPI cols
    SELECT 
      STRFTIME('%Y', i.InvoiceDate) AS year,
      g.Name AS genre_name,
      
      -- Customers & Revenue
      COUNT(DISTINCT i.CustomerId) AS num_customers,
      COUNT(DISTINCT i.InvoiceId) AS num_purchases,
      -- - Total track purchase (including repeats)
      SUM(il.Quantity) AS tracks_sold,
      SUM(il.UnitPrice * il.Quantity) AS revenue,
      COUNT(DISTINCT i.BillingCountry) AS num_countries,
    
      -- First time customers
      COUNT(DISTINCT CASE 
        WHEN STRFTIME('%Y', i.InvoiceDate) = STRFTIME('%Y', fp.first_date)
        THEN i.CustomerId
      END) AS first_time_customers,
    
      -- Unique track logic
      -- - Tracks sold that year
      COUNT(DISTINCT t.TrackId) AS unique_tracks_sold,
      COUNT(DISTINCT CASE 
        WHEN STRFTIME('%Y', i.InvoiceDate) = STRFTIME('%Y', fts.first_sold_date)
        THEN t.TrackId
      -- - Number of tracks sold for the first time that year (in subset)
      END) AS first_track_sold,                     
      -- - Total number of tracks in catalog (for % calculations)
      c.tracks_in_catalog
    FROM Invoice i
    JOIN InvoiceLine il ON i.InvoiceId = il.InvoiceId
    JOIN Track t ON il.TrackId = t.TrackId
    JOIN Genre g ON t.GenreId = g.GenreId
    JOIN Album al ON t.AlbumId = al.AlbumId
    JOIN Artist ar ON al.ArtistId = ar.ArtistId
    LEFT JOIN catalog c ON g.Name = c.genre
    LEFT JOIN first_track_sold fts ON t.TrackId = fts.TrackId AND g.Name = fts.genre
    LEFT JOIN (
      SELECT CustomerId, MIN(DATE(InvoiceDate)) AS first_date
      FROM Invoice
      GROUP BY CustomerId
    ) fp ON i.CustomerId = fp.CustomerId
    -- filter based on selectors
    {where_clause}
    GROUP BY year, genre_name, c.tracks_in_catalog
    ORDER BY year, genre_name;
  ")
  
  DBI::dbGetQuery(con, query) |>
    dplyr::rename_at("genre_name", ~"genre")
}

#' Get Artist-Level Aggregated KPIs for Artist Module
#'
#' Returns year-by-artist aggregates of key metrics: revenue, customers, 
#' purchases, tracks sold, and number of first-time customers. Used for the 
#' geographic distribution visualizations on the dashboard.
#'
#' @param con A DBI connection object.
#' @param where_clause A `DBI::SQL` WHERE clause generated from filters.
#'
#' @return A data frame with aggregated KPI columns by year and artist.
#' @examples
#' df <- get_artist_data(
#'   con, form_where_clause(country = "USA", .con = con)
#'   )
#' @seealso \code{\link{form_where_clause}}
#' @keywords internal
get_artist_data <- function(con, where_clause) {
  query <- glue::glue_sql("
    -- Get number of tracks in each artist's catalog
    WITH catalog AS (
      SELECT 
        ar.Name AS artist,
        COUNT(DISTINCT t.TrackId) AS tracks_in_catalog
      FROM Track t
      JOIN Album al ON t.AlbumId = al.AlbumId
      JOIN Artist ar ON al.ArtistId = ar.ArtistId
      GROUP BY ar.Name
    ),
    -- Get date each track was first sold in the subset
    first_track_sold AS (
      SELECT 
        t.TrackId,
        ar.Name AS artist,
        MIN(DATE(i.InvoiceDate)) AS first_sold_date
      FROM Invoice i
      JOIN InvoiceLine il ON i.InvoiceId = il.InvoiceId
      JOIN Track t ON il.TrackId = t.TrackId
      JOIN Album al ON t.AlbumId = al.AlbumId
      JOIN Artist ar ON al.ArtistId = ar.ArtistId
      {where_clause}
      GROUP BY t.TrackId, ar.Name
    ),
    -- First invoice date for each customer
    first_purchase AS (
      SELECT 
        CustomerId,
        MIN(DATE(InvoiceDate)) AS first_date
      FROM Invoice
      GROUP BY CustomerId
    )
    
    SELECT 
      STRFTIME('%Y', i.InvoiceDate) AS year,
      ar.Name AS artist_name,

      -- Customers & Revenue
      COUNT(DISTINCT i.CustomerId) AS num_customers,
      COUNT(DISTINCT i.InvoiceId) AS num_purchases,
      SUM(il.Quantity) AS tracks_sold,
      SUM(il.UnitPrice * il.Quantity) AS revenue,
      COUNT(DISTINCT i.BillingCountry) AS num_countries,

      -- First time customers
      COUNT(DISTINCT CASE 
        WHEN STRFTIME('%Y', i.InvoiceDate) = STRFTIME('%Y', fp.first_date)
        THEN i.CustomerId
      END) AS first_time_customers,

      -- Unique track logic
      COUNT(DISTINCT t.TrackId) AS unique_tracks_sold,
      COUNT(DISTINCT CASE 
        WHEN STRFTIME('%Y', i.InvoiceDate) = STRFTIME('%Y', fts.first_sold_date)
        THEN t.TrackId
      END) AS first_track_sold,
      c.tracks_in_catalog

    FROM Invoice i
    JOIN InvoiceLine il ON i.InvoiceId = il.InvoiceId
    JOIN Track t ON il.TrackId = t.TrackId
    JOIN Album al ON t.AlbumId = al.AlbumId
    JOIN Artist ar ON al.ArtistId = ar.ArtistId
    LEFT JOIN catalog c ON ar.Name = c.artist
    LEFT JOIN first_track_sold fts ON t.TrackId = fts.TrackId AND ar.Name = fts.artist
    LEFT JOIN first_purchase fp ON i.CustomerId = fp.CustomerId
    {where_clause}
    GROUP BY year, artist_name, c.tracks_in_catalog
    ORDER BY year, artist_name;
  ")
  
  DBI::dbGetQuery(con, query) |>
    dplyr::rename_at("artist_name", ~"artist")
}
