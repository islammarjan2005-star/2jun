# ---- TREEMAP (point-in-time snapshot of a categorical breakdown) ----
# Self-contained builder in the same spirit as dbt_build_table():
# it takes an already-collected snapshot data frame and returns a plotly
# treemap, sizing each tile by `value_col` and labelling it by `label_col`.
dbt_build_treemap <- function(
  data,
  label_col    = "industry",
  value_col    = "value",
  palette      = dbt_palettes$gaf,
  value_prefix = "",
  value_suffix = "",
  root_label   = "All"
) {

  # ---------- Guards ----------
  if (is.null(data) || !nrow(data)) {
    return(plotly::plot_ly(type = "treemap"))
  }
  stopifnot(label_col %in% names(data), value_col %in% names(data))

  df <- data
  df[[value_col]] <- suppressWarnings(as.numeric(df[[value_col]]))

  # Treemaps can only size strictly positive, finite contributions
  df <- df[is.finite(df[[value_col]]) & df[[value_col]] > 0, , drop = FALSE]
  if (!nrow(df)) return(plotly::plot_ly(type = "treemap"))

  # ---------- One tile per label (sum within label) ----------
  df <- df |>
    dplyr::group_by(.data[[label_col]]) |>
    dplyr::summarise(.value = sum(.data[[value_col]], na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$.value))

  labels  <- as.character(df[[label_col]])
  values  <- df$.value

  # Flat treemap: every tile hangs off a single implicit root
  parents <- rep(root_label, length(labels))

  # Recycle the palette across tiles
  cols <- rep(palette, length.out = length(labels))

  # Pre-format values with thousands separators for tile + hover text
  fmt_vals <- govuk_format_number(values, digits = 0)

  # ---------- Plot ----------
  plotly::plot_ly(
    type         = "treemap",
    labels       = labels,
    parents      = parents,
    values       = values,
    customdata   = fmt_vals,
    branchvalues = "total",
    sort         = TRUE,
    marker       = list(
      colors = cols,
      line   = list(width = 1, color = "#ffffff")
    ),
    texttemplate = paste0(
      "%{label}<br>", value_prefix, "%{customdata}", value_suffix
    ),
    hovertemplate = paste0(
      "<b>%{label}</b><br>",
      value_prefix, "%{customdata}", value_suffix,
      "<br>%{percentParent:.1%} of total",
      "<extra></extra>"
    ),
    pathbar = list(visible = FALSE),
    tiling  = list(pad = 1)
  ) |>
    plotly::layout(
      margin = list(t = 10, l = 10, r = 10, b = 10)
    ) |>
    plotly::config(displayModeBar = FALSE)
}
