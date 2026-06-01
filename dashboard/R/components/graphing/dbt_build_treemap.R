# ---- TREEMAP (point-in-time snapshot of a categorical breakdown) ----
# Self-contained builder in the same spirit as dbt_build_table():
# it takes an already-collected snapshot data frame and returns a plotly
# treemap, sizing each tile by `value_col` and labelling it by `label_col`.
dbt_build_treemap <- function(
  data,
  label_col       = "industry",
  value_col       = "value",
  palette         = dbt_palettes$gaf,
  value_prefix    = "",
  value_suffix    = "",
  exclude_pattern = NULL,  # regex of labels to drop (e.g. totals); never to empty
  title           = NULL   # small caption shown above the treemap (e.g. snapshot period)
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
  agg <- df |>
    dplyr::group_by(.data[[label_col]]) |>
    dplyr::summarise(.value = sum(.data[[value_col]], na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data[[".value"]]))

  labels <- as.character(agg[[label_col]])
  values <- agg[[".value"]]

  # ---------- Optionally drop aggregate/total rows ----------
  # Never drop down to an empty plot: if every remaining row is a total,
  # keep what we have so the user still sees something.
  if (!is.null(exclude_pattern) && nzchar(exclude_pattern)) {
    keep <- !grepl(exclude_pattern, labels, ignore.case = TRUE, perl = TRUE)
    if (any(keep)) {
      labels <- labels[keep]
      values <- values[keep]
    }
  }

  # Flat treemap: every tile is top-level (parent ""), so a tile can never
  # be its own parent (which would stop plotly from rendering).
  parents <- rep("", length(labels))

  # Recycle the palette across tiles
  cols <- rep(palette, length.out = length(labels))

  # Pre-format values + shares for tile and hover text
  fmt_vals <- govuk_format_number(values, digits = 0)
  total    <- sum(values, na.rm = TRUE)
  share    <- if (total > 0) values / total else rep(0, length(values))
  pct      <- paste0(formatC(100 * share, format = "f", digits = 1), "%")

  hover <- paste0(
    "<b>", labels, "</b><br>",
    value_prefix, fmt_vals, value_suffix,
    "<br>", pct, " of total"
  )

  # ---------- Plot ----------
  has_title <- !is.null(title) && nzchar(title)

  plt <- plotly::plot_ly(
    type         = "treemap",
    labels       = labels,
    parents      = parents,
    values       = values,
    text         = fmt_vals,
    texttemplate = paste0("%{label}<br>", value_prefix, "%{text}", value_suffix),
    hovertext    = hover,
    hoverinfo    = "text",
    sort         = TRUE,
    marker       = list(
      colors = cols,
      line   = list(width = 1, color = "#ffffff")
    ),
    tiling = list(pad = 1)
  ) |>
    plotly::layout(
      margin = list(t = if (has_title) 34 else 10, l = 10, r = 10, b = 10)
    ) |>
    plotly::config(displayModeBar = FALSE)

  if (has_title) {
    plt <- plt |>
      plotly::layout(
        title = list(
          text    = title,
          x       = 0.02,
          xanchor = "left",
          y       = 0.98,
          yanchor = "top",
          font    = list(size = 13, color = "#505a5f")
        )
      )
  }

  plt
}
