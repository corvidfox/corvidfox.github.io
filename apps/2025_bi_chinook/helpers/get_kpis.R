#' Build KPIs from Time Series Data
#'
#' Given raw and monthly-aggregated transaction data, return a list of 
#' time-series KPIs, or NULL if the input data has 0 rows.
#'
#' @param data_raw Raw transactional data from SQL query.
#' @param data_monthly Aggregated monthly time-series data.
#'
#' @return A named list of KPIs or NULL if no data available.
build_ts_kpis <- function(data_raw, data_monthly) {
  if (nrow(data_raw) == 0 || nrow(data_monthly) == 0) return(NULL)
  
  total_purchases <- sum(data_monthly$num_purchases, na.rm = TRUE)
  total_revenue <- sum(data_monthly$revenue, na.rm = TRUE)
  
  list(
    total_rev = total_revenue,
    avg_rev = mean(data_monthly$revenue, na.rm = TRUE),
    total_purchases = total_purchases,
    total_tracks = sum(data_monthly$tracks_sold, na.rm = TRUE),
    avg_per_purchase = if (total_purchases > 0) {
      total_revenue / total_purchases
    } else {
      NA_real_
    },
    total_customers = dplyr::n_distinct(data_raw$customer_id),
    first_time = sum(data_raw$first_time_customer, na.rm = TRUE)
  )
}

#' Build KPIs from Choropleth Data
#'
#' Given raw and monthly-aggregated transaction data, return a list of 
#' choropleth/geographic KPIs, or NULL if the input data has 0 rows.
#'
#' @param agg_df Aggregated country-based data.
#' @param metric_col Name of the metric col from filters()$metric.
#'
#' @return A named list of KPIs or NULL if no data available.
build_choro_kpis <- function(agg_df, metric_col){
  if (nrow(agg_df) == 0 || nrow(agg_df) == 0) return(NULL)
  
  # Top N (default to 5, can be less if needed)
  top_n <- min(nrow(agg_df), 5)
  top_countries <- agg_df |> 
    dplyr::arrange(dplyr::desc(.data[[metric_col]])) |> 
    dplyr::slice_head(n = top_n)
  
  # Add Country Flag emojis
  top_countries$flag_emoji <- iso3_to_flag(top_countries$iso_alpha)
  top_countries$country_with_flag <- trimws(
    paste(iso3_to_flag(top_countries$iso_alpha), top_countries$country)
  )
  
  # Aggregate KPIs
  global_total_revenue <- sum(agg_df$revenue, na.rm = TRUE)
  top_revenue <- sum(top_countries$revenue, na.rm = TRUE)
  top_customers <- sum(top_countries$num_customers, na.rm = TRUE)
  top_revenue_per_customer <- if (top_customers > 0) {
    top_revenue / top_customers
  } else {
    NA
  }
  
  # Package KPIs
  list(
    top_countries = top_countries,
    num_countries = nrow(agg_df),
    top5_metric = top_countries[[metric_col]],
    total_revenue = top_revenue,
    revenue_pct = if (global_total_revenue > 0) {
      round(100 * top_revenue / global_total_revenue, 1)
    } else {
      NA
    },
    num_customers = top_customers,
    avg_rev_per_customer = top_revenue_per_customer
  )
}

#' Build KPIs from Genre Data
#'
#' Given raw and monthly-aggregated transaction data, return a list of 
#' by-Genre KPIs, or NULL if the input data has 0 rows.
#'
#' @param agg_df Aggregated genre-based data.
#' @param metric_col Name of the metric col from filters()$metric.
#'
#' @return A named list of KPIs or NULL if no data available.
build_genre_kpis <- function(agg_df, metric_col){
  if (nrow(agg_df) == 0 || nrow(agg_df) == 0) return(NULL)
  
  # Top N (default to 5, can be less if needed)
  top_n <- min(nrow(agg_df), 5)
  top_genres <- agg_df |> 
    dplyr::arrange(dplyr::desc(.data[[metric_col]])) |> 
    dplyr::slice_head(n = top_n)
  
  
  # Aggregate KPIs
  all_total_revenue <- sum(agg_df$revenue, na.rm = TRUE)
  top_revenue <- sum(top_genres$revenue, na.rm = TRUE)
  
  top_unique_track_sales <- top_genres$first_track_sold
  top_catalog_size <- top_genres$tracks_in_catalog
  
  # Package KPIs
  list(
    top_genres = top_genres,
    num_genres = nrow(agg_df),
    top5_metric = top_genres[[metric_col]],
    total_revenue = top_revenue,
    revenue_pct = if (all_total_revenue > 0) {
      round(100 * top_genres$revenue / all_total_revenue, 1)
    } else {
      NA
    },
    num_tracks_sold = top_genres$tracks_sold,
    catalog_size = top_catalog_size,
    catalog_pct_sold = round(
      100 * top_genres$first_track_sold / top_catalog_size, 1
    )
  )
}

#' Build KPIs from Artist Data
#'
#' Given raw and monthly-aggregated transaction data, return a list of 
#' by-Artist KPIs, or NULL if the input data has 0 rows.
#'
#' @param agg_df Aggregated artist-based data.
#' @param metric_col Name of the metric col from filters()$metric.
#'
#' @return A named list of KPIs or NULL if no data available.
build_artist_kpis <- function(agg_df, metric_col){
  if (nrow(agg_df) == 0 || nrow(agg_df) == 0) return(NULL)
  
  # Top N (default to 5, can be less if needed)
  top_n <- min(nrow(agg_df), 5)
  top_artists <- agg_df |> 
    dplyr::arrange(dplyr::desc(.data[[metric_col]])) |> 
    dplyr::slice_head(n = top_n)
  
  # Aggregate KPIs
  all_total_revenue <- sum(agg_df$revenue, na.rm = TRUE)
  top_revenue <- sum(top_artists$revenue, na.rm = TRUE)
  
  top_unique_track_sales <- top_artists$first_track_sold
  top_catalog_size <- top_artists$tracks_in_catalog
  
  # Package KPIs
  list(
    top_artists = top_artists,
    num_artists = nrow(agg_df),
    top5_metric = top_artists[[metric_col]],
    total_revenue = top_revenue,
    revenue_pct = if (all_total_revenue > 0) {
      round(100 * top_artists$revenue / all_total_revenue, 1)
    } else {
      NA
    },
    num_tracks_sold = top_artists$tracks_sold,
    catalog_size = top_catalog_size,
    catalog_pct_sold = round(
      100 * top_artists$first_track_sold / top_catalog_size, 1
    )
  )
}