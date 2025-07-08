#' @file kpi_cards.R
#' @title KPI Card Utilities for Themed Shiny Dashboards
#'
#' A set of helper functions for building and rendering KPI display cards 
#' in modular Shiny applications. These utilities support Bootstrap 5 theming, 
#' light/dark mode responsiveness, and flexible layouts using 
#' \code{bslib}, \code{bsplus}, and \code{shiny}.
#'
#' @section Functions:
#' - \code{\link{build_kpi}}: Define individual KPI entries with labels, 
#' values, and optional tooltips.
#' - \code{\link{render_kpi_card}}: Render one or more KPIs as a responsive, 
#' theme-aware UI card.
#'
#' @seealso \code{\link{generate_styles}}, \code{\link{bslib::bs_theme}}, 
#' \code{\link{bsplus::bs_embed_tooltip}}
#'
#' @keywords internal

#' Build a KPI Entry
#'
#' Creates a named list representing a KPI item with a label, value, 
#' and optional tooltip. Intended for use inside KPI card display logic.
#'
#' @param label A character string describing the metric (e.g., "Revenue").
#' @param value A character or numeric value to display.
#' @param tooltip Optional character string shown as a tooltip.
#'
#' @return A named list with `label`, `value`, and optional `tooltip`.
#' @examples
#' build_kpi("Revenue", "$125K", "Total income in USD")
build_kpi <- function(label, value, tooltip = NULL) {
  list(label = label, value = value, tooltip = tooltip)
}

#' Render a Thematic KPI Card
#'
#' Creates a responsive Bootstrap-styled card displaying one or more KPI 
#' metrics.
#' Supports tooltips, custom icons, and adaptive theming via an optional 
#' `styles` list.
#'
#' @param title The card title, displayed in the header.
#' @param kpi_list A list of KPI items, each created with 
#' \code{\link{build_kpi}}.
#' @param icon Optional HTML tag or icon prepended to the title.
#' @param tooltip Optional tooltip for the entire card header.
#' @param styles Optional list of color styles (e.g., from 
#' \code{generate_styles()}).
#' @param height Height of the card in CSS units (default "130px").
#'
#' @return A Shiny UI card component displaying formatted KPI values.
#' @examples
#' kpis <- list(
#'   build_kpi("Revenue", "$125K", "Total income"),
#'   build_kpi("Purchases", "512")
#' )
#' render_kpi_card("Sales Summary", kpis)
render_kpi_card <- function(
    title,
    kpi_list,
    icon = NULL,
    tooltip = NULL,
    styles = NULL,
    height = "130px"
) {
  
  # Fallback content if KPI list is empty
  if (is.null(kpi_list) || length(kpi_list) == 0) {
    kpi_list <- list(list(label = "Status", value = "No data available"))
  }
  
  stopifnot(is.character(title), is.list(kpi_list))
  
  # Resolve styling colors
  bg_color <- if (!is.null(styles) && !is.null(styles$kpi_bg)) {
    styles$kpi_bg
  } else {
    "var(--bs-secondary-bg)"
  }
  text_color <- if (!is.null(styles)) styles$text_color else "#000"
  secondary_color <- if (!is.null(styles)) styles$secondary_color else "#666"
  
  # Card header with optional tooltip
  card_header <- if (!is.null(tooltip)) {
    bsplus::bs_embed_tooltip(
      bslib::card_header(HTML(paste0(icon, " ", title))),
      title = tooltip,
      placement = "top",
      trigger = "hover"
    )
  } else {
    bslib::card_header(HTML(paste0(icon, " ", title)))
  }
  
  # Build card body: display all KPI items
  body_items <- lapply(kpi_list, function(kpi) {
    content <- div(
      HTML(paste0("<strong>", kpi$label, ":</strong> ", kpi$value)),
      style = paste0(
        "font-size: 1em; margin-bottom: 0.4em; color:", text_color
      )
    )
    if (!is.null(kpi$tooltip)) {
      bsplus::bs_embed_tooltip(
        content,
        title = kpi$tooltip,
        placement = "top",
        trigger = "hover"
      )
    } else {
      content
    }
  })
  
  # Return the final card UI
  bslib::card(
    full_screen = FALSE,
    style = paste0(
      "min-height:", height, ";",
      " flex: 1;",
      " display: flex;",
      " flex-direction: column;",
      " justify-content: center;",
      " background-color:", bg_color, ";",
      " border-radius: 6px;"
    ),
    card_header,
    bslib::card_body(tagList(body_items))
  )
}

#' Safely render a KPI box, with fallback for NULL KPI data
#'
#' @param kpis The list of KPIs (can be NULL)
#' @param body An expression that returns a list of build_kpi() calls
#' @param title,icon,tooltip,styles Passed to render_kpi_card()
#'
#' @return Output of render_kpi_card() with either real KPIs or fallback
safe_kpi_card <- function(
    kpis, body_fn, title, icon = NULL, tooltip = NULL, styles = NULL
) {
  
  if (is.null(kpis)) {
    return(render_kpi_card(
      kpi_list = NULL,
      title = title,
      icon = icon,
      tooltip = tooltip,
      styles = styles
    ))
  }
  
  kpi_list <- body_fn()
  
  render_kpi_card(
    kpi_list = kpi_list,
    title = title,
    icon = icon,
    tooltip = tooltip,
    styles = styles
  )
}
