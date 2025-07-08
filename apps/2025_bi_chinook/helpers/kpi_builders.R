#' @file kpi_builders.R
#' @title KPI Calculators and Aggregators for Chinook Dashboard
#'
#' Defines helper functions used for calculating and aggregating KPI
#' values in the Chinook dataset from SQL query results. Includes reusable 
#' utilities for dynamically formatting metric-filter-based KPIs and building
#' KPI values for each module. These functions are primarily consumed by 
#' modular Shiny components.
#'
#' @seealso \code{\link{kpi_metric_format}}, \code{\link{build_ts_kpis}}, 
#'   \code{\link{build_choro_kpis}}, \code{\link{build_genre_kpis}},
#'   \code{\link{build_artist_kpis}}, \code{\link{build_retention_kpis}}
#' @keywords internal

#' Format Metric-Filter-Based KPI values
#'
#' Given raw values for a KPI determined by the "Metric" filter, apply
#' formatting to the values (currency as dollars, numbers with commas, etc.).
#'
#' @param kpi_vals Vector of values for a KPI selected by metric filter.
#' @param metric_col Variable name associated with the metric filter.
#'
#' @return A named list of KPIs or NULL if no data available.
#' @keywords internal
kpi_metric_format <- function(kpi_vals, metric_col){
  dplyr::case_when(
    metric_col == 'revenue' ~ scales::dollar(kpi_vals),
    metric_col == 'num_customers' ~ scales::comma(kpi_vals),
    metric_col == 'first_time_customers' ~ scales::comma(kpi_vals),
    metric_col == 'num_purchases' ~ scales::comma(kpi_vals),
    metric_col == 'tracks_sold' ~ scales::comma(kpi_vals)
  )
}

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
    total_rev = scales::dollar(total_revenue),
    avg_rev = scales::dollar(mean(data_monthly$revenue, na.rm = TRUE)),
    total_purchases = scales::comma(total_purchases),
    total_tracks = scales::comma(sum(data_monthly$tracks_sold, na.rm = TRUE)),
    avg_per_purchase = if (total_purchases > 0) {
      scales::dollar(total_revenue / total_purchases)
    } else {
      "NA"
    },
    total_customers = scales::comma(dplyr::n_distinct(data_raw$customer_id)),
    first_time = scales::comma(sum(data_raw$first_time_customer, na.rm = TRUE))
  )
}

#' Build KPIs from Choropleth Data
#'
#' Given aggregated transaction data, return a list of choropleth/geographic 
#' KPIs, or NULL if the input data has 0 rows.
#'
#' @param agg_df Aggregated country-based data.
#' @param metric_col Name of the metric col from filters()$metric.
#'
#' @return A named list of KPIs or NULL if no data available.
#' @keywords internal
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
  
  # Package KPIs
  list(
    top_countries = top_countries,
    top_countries_with_flags = top_countries$country_with_flag,
    num_countries = scales::comma(nrow(agg_df)),
    top5_metric = kpi_metric_format(top_countries[[metric_col]], metric_col),
    total_revenue = scales::dollar(top_countries$revenue),
    revenue_pct = if (global_total_revenue > 0) {
      scales::percent(
        top_countries$revenue / global_total_revenue, 
        accuracy = 0.1
      )
    } else {
      "NA"
    },
    num_customers = scales::comma(top_countries$num_customers),
    avg_rev_per_customer = ifelse(
      top_countries$num_customers == 0,
      "NA",
      scales::dollar(top_countries$revenue / top_countries$num_customers)
    )
    
  )
}

#' Build KPIs from Genre Data
#'
#' Given aggregated transaction data, return a list of by-Genre KPIs, or 
#' NULL if the input data has 0 rows.
#'
#' @param agg_df Aggregated genre-based data.
#' @param metric_col Name of the metric col from filters()$metric.
#'
#' @return A named list of KPIs or NULL if no data available.
#' @keywords internal
build_genre_kpis <- function(agg_df, metric_col){
  if (nrow(agg_df) == 0 || nrow(agg_df) == 0) return(NULL)
  
  # Top N (default to 5, can be less if needed)
  top_n <- min(nrow(agg_df), 5)
  top_genres <- agg_df |> 
    dplyr::arrange(dplyr::desc(.data[[metric_col]])) |> 
    dplyr::slice_head(n = top_n)
  
  # Aggregate KPIs
  all_total_revenue <- sum(agg_df$revenue, na.rm = TRUE)
  
  # Package KPIs
  list(
    top_genres = top_genres,
    top_genre_names = top_genres$genre,
    num_genres = scales::comma(nrow(agg_df)),
    top5_metric = kpi_metric_format(top_genres[[metric_col]], metric_col),
    total_revenue = scales::dollar(top_genres$revenue),
    revenue_pct = if (all_total_revenue > 0) {
      scales::percent(top_genres$revenue / all_total_revenue, accuracy = 0.1)
    } else {
      "NA"
    },
    num_tracks_sold = scales::comma(top_genres$tracks_sold),
    catalog_size = scales::comma(top_genres$tracks_in_catalog),
    catalog_pct_sold = ifelse(
      top_genres$tracks_in_catalog == 0,
      "NA",
      scales::percent(
        top_genres$first_track_sold / top_genres$tracks_in_catalog, 
        accuracy = 0.1
      )
    )
  )
}

#' Build KPIs from Artist Data
#'
#' Given aggregated transaction data, return a list of by-Artist KPIs, or 
#' NULL if the input data has 0 rows.
#'
#' @param agg_df Aggregated artist-based data.
#' @param metric_col Name of the metric col from filters()$metric.
#'
#' @return A named list of KPIs or NULL if no data available.
#' @keywords internal
build_artist_kpis <- function(agg_df, metric_col){
  if (nrow(agg_df) == 0 || nrow(agg_df) == 0) return(NULL)
  
  # Top N (default to 5, can be less if needed)
  top_n <- min(nrow(agg_df), 5)
  top_artists <- agg_df |> 
    dplyr::arrange(dplyr::desc(.data[[metric_col]])) |> 
    dplyr::slice_head(n = top_n)
  
  # Aggregate KPIs
  all_total_revenue <- sum(agg_df$revenue, na.rm = TRUE)
  
  # Package KPIs
  list(
    top_artists = top_artists,
    top_artist_names = top_artists$artist,
    num_artists = scales::comma(nrow(agg_df)),
    top5_metric = kpi_metric_format(top_artists[[metric_col]], metric_col),
    total_revenue = scales::dollar(top_artists$revenue),
    # total_revenue = scales::dollar(top_revenue),
    revenue_pct = if (all_total_revenue > 0) {
      scales::percent(top_artists$revenue / all_total_revenue, accuracy = 0.1)
    } else {
      "NA"
    },
    num_tracks_sold = scales::comma(top_artists$tracks_sold),
    catalog_size = scales::comma(top_artists$tracks_in_catalog),
    catalog_pct_sold = ifelse(
      top_artists$tracks_in_catalog == 0,
      "NA",
      scales::percent(
        top_artists$first_track_sold / top_artists$tracks_in_catalog, 
        accuracy = 0.1
      )
    )
  )
}

#' Build KPIs from Customer Retention Data
#'
#' Given event and cohort-aggregated transaction data, return a list of 
#' Customer Retention KPIs, or NULL if the input data has 0 rows.
#'
#' @param events Data from `get_retention_kpi_events()`
#' @param cohort_df Data from `get_retention_cohort_data()`
#' @param date_range A length-2 Date vector specifying the start and end range.
#' @param offsets Numeric vector of month_offsets to check (e.g. c(3, 6, 9))
#'
#' @return A named list of KPIs or NULL if no data available.
#' @seealso \code{\link{get_retention_kpi_events}}, 
#'   \code{\link{get_retention_cohort_data}}
#' @keywords internal
build_retention_kpis <- function(
    events, 
    cohort_df, 
    date_range, 
    offsets = c(3,6,9)
) {
  if (nrow(events) == 0 || nrow(events) == 0) return(NULL)
  if (nrow(cohort_df) == 0 || nrow(cohort_df) == 0) return(NULL)
  
  # Ensure date range and events are sorted
  date_range <- sort(date_range)
  events <- events |>
    dplyr::arrange(CustomerId, dt)
  
  # Process Cohort Retention Data KPIs (best retention in offset)
  kpis <- purrr::map_dfc(offsets, function(offset) {
    filtered <- cohort_df |>
      dplyr::filter(month_offset == offset, !is.na(retention_pct)) |>
      dplyr::arrange(dplyr::desc(retention_pct)) |>
      dplyr::slice_head(n = 1)
    
    cohort_fmt <- if (nrow(filtered) == 1) {
      format(as.Date(filtered$cohort_month), "%b %Y")
    } else {
      NA_character_
    }
    
    retention_val <- if (nrow(filtered) == 1) {
      scales::percent(filtered$retention_pct / 100, accuracy = 0.1)
    } else {
      NA_character_
    }
    
    setNames(
      list(cohort_fmt, retention_val),
      c(paste0("top_cohort_month_", offset),
        paste0("top_cohort_retention_", offset))
    )
  }) |> purrr::flatten()
  
  
  # Group by customer, summarize dates and window-related counts
  stats <- events |>
    dplyr::group_by(CustomerId) |>
    dplyr::summarise(
      all_dates       = list(dt),
      first_date      = min(dt),
      second_date     = dplyr::if_else(length(dt) >= 2, dt[2], as.Date(NA)),
      last_date       = dplyr::if_else(length(dt) > 1, max(dt),as.Date(NA)),
      in_window_dates = list(dt[dt >= date_range[1] & dt <= date_range[2]]),
      num_after       = sum(dt > date_range[2]),
      num_before      = sum(dt < date_range[1]),
      num_in_window   = sum(dt >= date_range[1] & dt <= date_range[2]),
      .groups = "drop"
    )
  
  ## Helper to get window related dates (as dates, not doubles)
  get_window_bounds <- function(dates, range_start, range_end) {
    dates <- sort(dates)
    in_window   <- dates[dates >= range_start & dates <= range_end]
    before_win  <- dates[dates <  range_start]
    after_win   <- dates[dates >  range_end]
    tibble::tibble(
      first_in_window = if (length(in_window) > 0) min(in_window)
      else as.Date(NA),
      second_in_window = if (length(in_window) > 1) sort(in_window)[2]
      else as.Date(NA),
      last_in_window = if (length(in_window) > 0) max(in_window)
      else as.Date(NA),
      last_before_window = if (length(before_win) > 0) max(before_win)
      else as.Date(NA),
      first_after_window = if (length(after_win) > 0) min(after_win)
      else as.Date(NA)
    )
  }
  
  window_bounds <- purrr::map_dfr(
    stats$all_dates,
    get_window_bounds,
    range_start = date_range[1],
    range_end = date_range[2]
  )
  
  # Calculate initial KPI values for each customer
  stats <- dplyr::bind_cols(stats, window_bounds) |>
    dplyr::mutate(
      cust = num_in_window > 0,
      cust_new = cust & num_before == 0,
      repeat_in_window = lengths(in_window_dates) > 1,
      repeat_returning = !is.na(last_before_window) & num_in_window > 0,
      repeat_new = is.na(last_before_window) & (
        (num_in_window > 0 & !is.na(first_after_window)) |
          (num_in_window > 1)
      ),
      repeat_any = (lengths(all_dates) > 1 & num_in_window > 0),
      # Customer lifespan (months)
      lifespan_mo_lifetime = dplyr::if_else(
        !is.na(first_date) & !is.na(last_date),
        lubridate::interval(first_date, last_date) / months(1),
        NA_real_
      ),
      lifespan_mo_window = dplyr::case_when(
        is.na(last_before_window) & is.na(first_after_window) ~ 
          lubridate::interval(last_in_window, first_in_window) / months(1),
        is.na(last_before_window) ~ 
          lubridate::interval(first_in_window, date_range[2]) / months(1),
        is.na(first_after_window) ~ 
          lubridate::interval(date_range[1], last_in_window) / months(1),
        is.na(second_in_window) ~ NA_real_,
        TRUE ~ lubridate::interval(date_range[1], date_range[2]) / months(1)
      ),
      # Gap Between First and Second Purchase (Days)
      gap_overall = as.numeric(
        difftime(second_date, first_date, units = "days")
      ),
      gap_in_window = as.numeric(
        difftime(second_in_window, first_in_window, units = "days")
      ),
      gap_winback = as.numeric(
        difftime(first_in_window, last_before_window, units = "days")
      ),
      gap_retention = as.numeric(
        difftime(first_after_window, last_in_window, units = "days")
      ),
      # Average Gap Between Purchases (days)
      avg_gap_lifetime = if_else(
        (num_after + num_before + num_in_window - 1) > 0,
        as.numeric(
          difftime(last_date, first_date, units = "days")
        ) / (num_after + num_before + num_in_window - 1),
        NA_real_
      ),
      avg_gap_in_window = if_else(
        (num_in_window - 1) > 0,
        as.numeric(
          difftime(last_in_window, first_in_window, units = "days")
        ) / (num_in_window - 1),
        NA_real_
      ),
      avg_gap_bounded = dplyr::case_when(
        !is.na(last_before_window) & !is.na(first_after_window) ~
          as.numeric(
            difftime(first_after_window, last_before_window,
                     units = "days")
          ) / (num_in_window + 1),
        is.na(last_before_window) & !is.na(first_after_window) ~
          as.numeric(
            difftime(first_after_window, first_in_window,
                     units = "days")
          ) / (num_in_window),
        !is.na(last_before_window) & is.na(first_after_window) ~
          as.numeric(
            difftime(last_in_window, last_before_window,
                     units = "days")
          ) / (num_in_window),
        TRUE ~ avg_gap_in_window
      )
    )
  
  # Aggregate KPIs
  n_total <- nrow(stats)
  n_new <- sum(stats$cust_new, na.rm = TRUE)
  n_repeat <- sum(stats$repeat_any, na.rm = TRUE)
  n_returning <- sum(!stats$cust_new, na.rm = TRUE)
  n_repeat_ret <- sum(stats$repeat_returning, na.rm = TRUE)
  n_repeat_new <- sum(stats$repeat_new, na.rm = TRUE)
  
  # Package KPIs
  append(
    kpis,
    list(
      num_customers = scales::comma(n_total),
      num_new_customers = scales::comma(n_new),
      pct_new_customers = scales::percent(n_new / n_total, 0.1),
      avg_lifespan_months_total = scales::comma(
        mean(stats$lifespan_mo_lifetime, na.rm = TRUE), accuracy = 0.1
      ),
      avg_lifespan_months_window = scales::comma(
        mean(stats$lifespan_mo_window, na.rm = TRUE), accuracy = 0.1
      ),
      n_repeat = n_repeat,
      repeat_rate_total = scales::percent(n_repeat / n_total, accuracy = 0.1),
      n_returning = n_returning,
      n_repeat_ret = n_repeat_ret,
      repeat_rate_returning = scales::percent(
        n_repeat_ret / n_returning, accuracy = 0.1
      ),
      n_repeat_new = n_repeat_new,
      repeat_rate_new = scales::percent(n_repeat_new / n_new, accuracy = 0.1),
      median_gap_overall = scales::comma(
        stats::median(stats$gap_overall, na.rm = TRUE), accuracy = 0.1
      ),
      median_gap_in_window = scales::comma(
        stats::median(stats$gap_in_window, na.rm = TRUE), accuracy = 0.1
      ),
      median_gap_winback = scales::comma(
        stats::median(stats$gap_winback, na.rm = TRUE), accuracy = 0.1
      ),
      median_gap_retention = scales::comma(
        stats::median(stats$gap_retention, na.rm = TRUE), accuracy = 0.1
      ),
      avg_gap_lifetime = scales::comma(
        mean(stats$avg_gap_lifetime, na.rm = TRUE), accuracy = 0.1
      ),
      avg_gap_window = scales::comma(
        mean(stats$avg_gap_in_window, na.rm = TRUE), accuracy = 0.1
      ),
      avg_gap_bounded = scales::comma(
        mean(stats$avg_gap_bounded, na.rm = TRUE), accuracy = 0.1
      )
    )
  )
}
