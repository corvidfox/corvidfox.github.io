#' @file global.R
#' @title Global Initialization for Chinook BI Dashboard
#'
#' Loads required packages, helper scripts, and modules. Establishes a 
#' persistent connection to the DuckDB instance of the Chinook dataset. 
#' Initializes global variables and constants used throughout the app 
#' (e.g., filter choices).
#'
#' This script is sourced once at app startup before UI and server 
#' environments are created.
#'
#' @seealso ui.R, server.R

# ---- Load Required Libraries ----
# TO DO: ALphabetize
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(bslib)
library(bsicons)
library(bsplus)

library(DBI)
library(duckdb)

library(dplyr)
library(tidyr)
library(glue)
library(lubridate)
library(scales)
library(countrycode)

library(plotly)
library(ggplot2)

# ---- Load and Validate Helper Functions from /helpers ----
helper_files <- list.files("helpers", full.names = TRUE)

for (f in helper_files) {
  tryCatch({
    source(f, local = TRUE)
    message(sprintf("✅ Loaded: %s", f))
  }, error = function(e) {
    message(sprintf("❌ Error in %s:\n  %s", f, e$message))
  })
}

# ---- Load and Validate UI and Server Modules from /modules ----
module_files <- list.files("modules", full.names = TRUE)

for (f in module_files) {
  tryCatch({
    source(f, local = TRUE)
    message(sprintf("✅ Loaded: %s", f))
  }, error = function(e) {
    message(sprintf("❌ Error in %s:\n  %s", f, e$message))
  })
}

# ---- Connect to Chinook DuckDB Database ----
# Uses embedded DuckDB; requires relative path resolution
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "../../data/chinook.duckdb")

# Close DB connection when app stops
shiny::onStop(function() {
  try({
    DBI::dbDisconnect(con, shutdown = TRUE)
    message("Database connection closed.")
  })
})

# ---- Prepare Global Filter Values ----
## Date Range (min/max invoice dates)
date_range_limits <- DBI::dbGetQuery(con, "
  SELECT MIN(InvoiceDate) AS min_date, MAX(InvoiceDate) AS max_date FROM Invoice
")
min_date <- as.Date(date_range_limits$min_date)
max_date <- as.Date(date_range_limits$max_date)

## Genre Choices (for selectizeInput)
genre_choices <- sort(DBI::dbGetQuery(
  con, 
  "SELECT DISTINCT Name FROM Genre"
)$Name)

## Artist Choices (alphabetized for UX)
artist_choices <- sort(DBI::dbGetQuery(
  con, 
  "SELECT DISTINCT Name FROM Artist ORDER BY Name"
)$Name)

## Country Choices (based on billing location, alphabetized for UX)
country_choices <- sort(DBI::dbGetQuery(
  con,
  "SELECT DISTINCT BillingCountry FROM Invoice"
)$BillingCountry)

## Metric Choices
metric_choices <- c(
  "Revenue" = "revenue",
  "Number of Customers" = "num_customers",
  "First-Time Customers" = "first_time_customers",
  "Number of Purchases" = "num_purchases",
  "Tracks Sold" = "tracks_sold"
)
