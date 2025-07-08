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
#' \code{\link{get_genre_data}}, \code{\link{get_artist_data}},
#' \code{\link{get_retention_curve_data}}, 
#' \code{\link{get_retention_cohort_data}}, 
#' \code{\link{get_retention_kpi_events}},
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
    date_range    = DBI::dbGetQuery(
      con, paste0(
        "SELECT MIN(InvoiceDate) AS min_date, MAX(InvoiceDate)",
        " AS max_date FROM Invoice"
      )),
    customers     = DBI::dbGetQuery(
      con, "SELECT COUNT(DISTINCT CustomerId) AS n FROM Customer"
    )$n,
    purchases     = DBI::dbGetQuery(
      con, "SELECT COUNT(*) AS n FROM Invoice"
    )$n,
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
#' Executes a SQL query that returns filtered invoice data grouped by month. 
#' KPI and metric columns include revenue, number of purchases, 
#' tracks sold, country, and first-time customer flag. Used to power the
#' dashboard's time-series visualizations and KPIs.
#'
#' @param con A DBI connection to the Chinook database.
#' @param where_clause A `DBI::SQL` WHERE clause from `form_where_clause()`.
#'
#' @return A data frame with monthly invoice-level KPI columns.
#' @examples
#' ts <- get_ts_data(
#'   con, 
#'   form_where_clause(date_range = c("2023-01-01", "2023-06-01"), .con = con)
#'   )
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

#' Get Country-Level Aggregated Metric and KPI data for Choropleth
#'
#' Executes a SQL query that returns filtered invoice data grouped by 
#' country and year. KPI and metric columns include revenue, number of 
#' customers, number of first-time customers, number of purchases, and tracks 
#' sold. Used to power the dashboard's choropleth visualizations and KPIs.
#'
#' @param con A DBI connection to the Chinook database.
#' @param where_clause A `DBI::SQL` WHERE clause from `form_where_clause()`.
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
  ", .con = con)
  
  DBI::dbGetQuery(con, query)
}

#' Get Filtered Genre-Level Invoice Data
#'
#' Executes a SQL query that returns filtered invoice data grouped by year 
#' and genre. KPI and metric columns include revenue, number of purchases, 
#' tracks sold (including first-time sale flag and unique sales), tracks in 
#' catalog, number of countries, number of customers, and number of first-time 
#' customers flag. Used to power the dashboard's "by genre" visualizations and 
#' KPIs.
#'
#' @param con A DBI connection to the Chinook database.
#' @param where_clause A `DBI::SQL` WHERE clause from `form_where_clause()`.
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
  ", .con = con)
  
  DBI::dbGetQuery(con, query) |>
    dplyr::rename_at("genre_name", ~"genre")
}

#' Get Filtered Artist-Level Invoice Data
#'
#' Executes a SQL query that returns filtered invoice data grouped by year 
#' and artist. KPI and metric columns include revenue, number of purchases, 
#' tracks sold (including first-time sale flag and unique sales), tracks in 
#' catalog, number of countries, number of customers, and number of first-time 
#' customers flag. Used to power the dashboard's "by artist" visualizations 
#' and KPIs.
#'
#' @param con A DBI connection to the Chinook database.
#' @param where_clause A `DBI::SQL` WHERE clause from `form_where_clause()`.
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
      JOIN Genre g ON t.GenreId = g.GenreId
      -- filter based on selectors
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
    -- Filter data set based on selectors, make KPI some KPI cols
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
    JOIN Genre g ON t.GenreId = g.GenreId
    LEFT JOIN catalog c ON ar.Name = c.artist
    LEFT JOIN first_track_sold fts ON t.TrackId = fts.TrackId AND ar.Name = fts.artist
    LEFT JOIN first_purchase fp ON i.CustomerId = fp.CustomerId
    -- filter based on selectors
    {where_clause}
    GROUP BY year, artist_name, c.tracks_in_catalog
    ORDER BY year, artist_name;
  ", .con = con)
  
  DBI::dbGetQuery(con, query) |>
    dplyr::rename_at("artist_name", ~"artist")
}

#' Get Retention Decay Curve Data
#'
#' Executes a SQL query that returns filtered invoice data grouped by month
#' offset from month-year of first lifetime purchase. Retention decay curve 
#' specific columns include month offset, number of returning customers, 
#' cohort size, and retention percent. Used to power the dashboard's 
#' "retention decay curve" visualization.
#'
#' @param con A DBI connection to the Chinook database.
#' @param where_clause A `DBI::SQL` WHERE clause from `form_where_clause()`.
#'
#' @return A data frame with month offset, number of returning customers, 
#'   cohort size, and retention percent aggregated by month offset.
#' @examples
#' df <- get_retention_decay_data(
#'   con, form_where_clause(country = "USA", .con = con)
#'   )
#' @seealso \code{\link{form_where_clause}}
#' @keywords internal
get_retention_decay_data <- function(con, where_clause) {
  query <- glue::glue_sql("
    -- First invoice date for each customer
    WITH FirstPurchase AS (
      SELECT
        i.CustomerId,
        MIN(DATE(i.InvoiceDate)) AS first_purchase_date
      FROM Invoice i
      GROUP BY CustomerId
    ),
    
    -- All (filtered) purchase events, tagged with months since first
    Purchases AS (
      SELECT
        i.CustomerId,
        DATE(i.InvoiceDate) AS purchase_date,
        DATE_DIFF('month', fp.first_purchase_date, i.InvoiceDate) AS month_offset
      FROM Invoice i
      JOIN InvoiceLine il ON i.InvoiceId = il.InvoiceId
      JOIN Track t ON il.TrackId = t.TrackId
      JOIN Album al ON t.AlbumId = al.AlbumId
      JOIN Artist ar ON al.ArtistId = ar.ArtistId
      JOIN Genre g ON t.GenreId = g.GenreId
      JOIN FirstPurchase fp ON i.CustomerId = fp.CustomerId
      -- filter based on selectors
      {where_clause}
    )
    
    -- Pull retention-curve specific data
    SELECT
      month_offset,
      COUNT(DISTINCT CASE WHEN month_offset > 0 THEN Purchases.CustomerId END)
        AS returning_customers,
      (SELECT COUNT(*) FROM FirstPurchase) AS cohort_size,
      ROUND(
        100.0
        * COUNT(DISTINCT CASE WHEN month_offset > 0 THEN Purchases.CustomerId END)
        / NULLIF((SELECT COUNT(*) FROM FirstPurchase),0),
        1
      ) AS retention_pct
    FROM Purchases
    WHERE month_offset > 0
    GROUP BY month_offset
    ORDER BY month_offset
  ", .con = con)
  
  DBI::dbGetQuery(con, query)
}

#' Get Cohort Heatmap Data
#'
#' Executes a SQL query that returns filtered invoice data grouped by customer 
#' cohort (month-year of first lifetime purchase). Cohort heatmap specific
#' columns include cohort, month offset, number of active customers, cohort
#' size, and retention percent. Used to power the dashboard's "cohort 
#' retention heatmap" visualization and KPIs.
#'
#' @param con A DBI connection to the Chinook database.
#' @param where_clause A `DBI::SQL` WHERE clause from `form_where_clause()`.
#'
#' @return A data frame with cohorts (month-year of first purchase), month 
#'   offset, number of returning customers, cohort size, and retention 
#'   percent aggregated by cohorts.
#' @examples
#' df <- get_retention_cohort_data(
#'   con, form_where_clause(country = "USA", .con = con)
#'   )
#' @seealso \code{\link{form_where_clause}}
#' @keywords internal
get_retention_cohort_data <- function(con, where_clause) {
  query <- glue::glue_sql("
    -- 1) First purchase date + assign to a cohort (month bucket)
    WITH FirstPurchases AS (
      SELECT
        i.CustomerId,
        MIN(DATE(i.InvoiceDate)) AS first_purchase_date,
        DATE_TRUNC('month', MIN(i.InvoiceDate)) AS cohort_month
      FROM Invoice i
      GROUP BY i.CustomerId
    ),

    -- All purchases with a months‐since‐first tag, Filtered
    AllPurchases AS (
      SELECT
        i.CustomerId,
        DATE(i.InvoiceDate) AS purchase_date,
        DATE_TRUNC('month', i.InvoiceDate) AS purchase_month,
        fp.cohort_month,
        DATE_DIFF('month', fp.first_purchase_date, i.InvoiceDate) AS month_offset
      FROM Invoice i
      JOIN InvoiceLine il ON i.InvoiceId = il.InvoiceId
      JOIN Track t ON il.TrackId = t.TrackId
      JOIN Album al ON t.AlbumId = al.AlbumId
      JOIN Artist ar ON al.ArtistId = ar.ArtistId
      JOIN Genre g ON t.GenreId = g.GenreId
      JOIN FirstPurchases fp ON i.CustomerId = fp.CustomerId
      -- filter based on selectors
      {where_clause}
    ),

    -- Size of each cohort (at month_offset = 0)
    CohortSizes AS (
      SELECT
        cohort_month,
        COUNT(DISTINCT CustomerId) AS cohort_size
      FROM FirstPurchases
      GROUP BY cohort_month
    ),

    -- Unique active customers in each cohort‐month_offset cell
    CustomerActivity AS (
      SELECT DISTINCT
        CustomerId,
        cohort_month,
        month_offset
      FROM AllPurchases
    )

    -- Final cohort retention data grid
    SELECT
      ca.cohort_month,
      ca.month_offset,
      COUNT(DISTINCT ca.CustomerId) AS num_active_customers,
      cs.cohort_size,
      ROUND(
        100.0 * COUNT(DISTINCT ca.CustomerId) / NULLIF(cs.cohort_size, 0),
        1
      ) AS retention_pct
    FROM CustomerActivity ca
    JOIN CohortSizes cs
      ON ca.cohort_month = cs.cohort_month
    GROUP BY ca.cohort_month, ca.month_offset, cs.cohort_size
    ORDER BY ca.cohort_month, ca.month_offset
  ", .con = con)
  
  DBI::dbGetQuery(con, query)
}

#' Get Customer Events for Customer Retention KPIs
#'
#' Executes a SQL query that returns filtered invoice data, reflecting
#' unique invoices (purchases) for each customer with dates. Recommend that
#' a unique WHERE clause is developed that omits "date range".
#' Used to power the dashboard's "customer retention" KPIs.
#'
#' @param con A DBI connection to the Chinook database.
#' @param where_clause A `DBI::SQL` WHERE clause from `form_where_clause()`.
#'
#' @return A data frame with CustomerId, dt (date of purchase), and InvoiceId
#' @examples
#' df <- get_retention_kpi_events(
#'   con, form_where_clause(country = "USA", .con = con)
#'   )
#' @seealso \code{\link{form_where_clause}}
#' @keywords internal
get_retention_kpi_events <- function(con, where_clause) {
  query <- glue::glue_sql(
    "SELECT
       i.CustomerId,
       DATE(i.InvoiceDate) AS dt,
       i.InvoiceId
     FROM Invoice i
     JOIN InvoiceLine il ON i.InvoiceId = il.InvoiceId
     JOIN Track t ON il.TrackId = t.TrackId
     JOIN Album al ON t.AlbumId = al.AlbumId
     JOIN Artist ar ON al.ArtistId = ar.ArtistId
     JOIN Genre g ON t.GenreId = g.GenreId
     -- filter by selectors
     {where_clause}",
    .con = con
  )
  DBI::dbGetQuery(con, query) |>
    dplyr::arrange(CustomerId, dt) |>
    # Reduce to unique invoice (number and date) per customer
    dplyr::distinct(CustomerId, InvoiceId, dt, .keep_all = TRUE)
}