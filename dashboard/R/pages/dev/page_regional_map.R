# regional map page

library(ggplot2)
library(plotly)
library(maps)

# region lookup
UK_REGIONS <- data.frame(
  area_code   = c("E12000001", "E12000002", "E12000003", "E12000004",
                   "E12000005", "E12000006", "E12000007", "E12000008",
                   "E12000009", "W92000004", "S92000003", "N92000002"),
  region_name = c("North East", "North West", "Yorkshire and The Humber",
                   "East Midlands", "West Midlands", "East of England",
                   "London", "South East", "South West",
                   "Wales", "Scotland", "Northern Ireland"),
  lat = c(55.0, 54.0, 53.8, 52.8, 52.5, 52.2, 51.5, 51.2, 50.7, 52.0, 56.5, 54.6),
  lon = c(-1.5, -2.7, -1.2, -1.0, -2.0,  0.5, -0.1, -0.8, -3.2, -3.5, -4.0, -6.8),
  stringsAsFactors = FALSE
)

# ITL1 (boundary table) code -> GSS area_code (survey/UK_REGIONS) crosswalk.
# ons.uk_itl_level1_2025 keys on itl125cd (TLC..TLN); our data uses E12.../W9.../etc.
ITL1_TO_AREA <- c(
  TLC = "E12000001", TLD = "E12000002", TLE = "E12000003", TLF = "E12000004",
  TLG = "E12000005", TLH = "E12000006", TLI = "E12000007", TLJ = "E12000008",
  TLK = "E12000009", TLL = "W92000004", TLM = "S92000003", TLN = "N92000002"
)


# data fetch
get_regional_tbl <- function(conn) {
  tryCatch(
    DBI::dbGetQuery(conn, "
      SELECT employment_group, age_group, value_type, area_code, value
      FROM (
        SELECT employment_group,
               age_group,
               value_type,
               area_code,
               CAST(value AS NUMERIC) AS value,
               ROW_NUMBER() OVER (
                 PARTITION BY employment_group, age_group, value_type, area_code
                 ORDER BY ctid DESC
               ) AS rn
        FROM   ons.labour_market__regional_survey
        WHERE  area_code IN (
                 'E12000001','E12000002','E12000003','E12000004',
                 'E12000005','E12000006','E12000007','E12000008',
                 'E12000009','W92000004','S92000003','N92000002'
               )
      ) sub
      WHERE rn = 1
      ORDER BY area_code
    "),
    error = function(e) {
      message("Regional query error: ", conditionMessage(e))
      data.frame(
        employment_group = character(),
        age_group        = character(),
        value_type       = character(),
        area_code        = character(),
        value            = numeric(),
        stringsAsFactors = FALSE
      )
    }
  )
}


# ITL1 boundary fetch -> GeoJSON FeatureCollection (list) keyed on area_code.
# Uses PostGIS ST_AsGeoJSON to turn the stored geometry into drawable GeoJSON.
# Returns NULL on any failure (e.g. PostGIS not enabled) so callers can fall
# back to the bubble map. Fully in-house: reads only from the ons schema.
get_itl1_geojson <- function(conn) {
  tryCatch({
    rows <- DBI::dbGetQuery(conn, "
      SELECT itl125cd AS itl_cd,
             ST_AsGeoJSON(geometry) AS gj
      FROM   ons.uk_itl_level1_2025
    ")
    if (is.null(rows) || nrow(rows) == 0) return(NULL)

    rows$area_code <- unname(ITL1_TO_AREA[rows$itl_cd])
    rows <- rows[!is.na(rows$area_code) & !is.na(rows$gj) & nzchar(rows$gj), , drop = FALSE]
    if (nrow(rows) == 0) return(NULL)

    features <- lapply(seq_len(nrow(rows)), function(i) {
      list(
        type       = "Feature",
        id         = rows$area_code[i],
        properties = list(area_code = rows$area_code[i], itl_cd = rows$itl_cd[i]),
        geometry   = jsonlite::fromJSON(rows$gj[i], simplifyVector = FALSE)
      )
    })

    list(type = "FeatureCollection", features = features)
  },
  error = function(e) {
    message("ITL1 boundary load failed (falling back to bubble map): ",
            conditionMessage(e))
    NULL
  })
}


# ui
regional_map_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(class = "govuk-width-container",
      tags$main(class = "govuk-main-wrapper",
        tags$span(class = "govuk-caption-xl", "Labour Market"),
        tags$h1(class = "govuk-heading-xl", "Regional Employment Map"),
        tags$p(class = "govuk-body-s", paste("Last updated:", Sys.Date())),

        # debug panel
        div(class = "govuk-grid-row",
          div(class = "govuk-grid-column-full",
            tags$details(class = "govuk-details",
              tags$summary(class = "govuk-details__summary",
                tags$span(class = "govuk-details__summary-text", "Debug: Query Info")
              ),
              tags$div(class = "govuk-details__text",
                verbatimTextOutput(ns("debug_info"))
              )
            )
          )
        ),

        # dropdowns
        div(class = "govuk-grid-row",
          div(class = "govuk-grid-column-one-third",
            selectInput(ns("measure"), "Employment Measure", choices = NULL)
          ),
          div(class = "govuk-grid-column-one-third",
            selectInput(ns("value_type"), "Value Type", choices = NULL)
          ),
          div(class = "govuk-grid-column-one-third",
            selectInput(ns("age_group"), "Age Group", choices = NULL)
          )
        ),

        # map
        div(class = "govuk-grid-row",
          div(class = "govuk-grid-column-full",
            tags$h2(class = "govuk-heading-l", "UK Regional Employment Map"),
            plotlyOutput(ns("uk_map"), height = "600px")
          )
        ),

        # bar chart
        div(class = "govuk-grid-row", style = "margin-top: 30px;",
          div(class = "govuk-grid-column-full",
            tags$h2(class = "govuk-heading-l", "Regional Comparison"),
            plotlyOutput(ns("bar_chart"), height = "450px")
          )
        ),

        # table
        div(class = "govuk-grid-row", style = "margin-top: 30px;",
          div(class = "govuk-grid-column-full",
            tags$h2(class = "govuk-heading-l", "Data Table"),
            reactableOutput(ns("region_table"), height = "400px")
          )
        )
      )
    )
  )
}


# server
regional_map_server <- function(id, conn = APP_DB$pool) {
  moduleServer(id, function(input, output, session) {

    # fetch data
    all_regional <- reactive({
      get_regional_tbl(conn)
    })

    # ITL1 boundaries (loaded once; NULL if unavailable -> bubble fallback)
    itl1_geo <- reactive({
      get_itl1_geojson(conn)
    })

    # init dropdowns
    observe({
      d <- all_regional()
      if (is.null(d) || nrow(d) == 0) return()

      measures <- sort(unique(d$employment_group))
      measure_sel <- if ("Economically inactive" %in% measures) "Economically inactive" else measures[1]
      updateSelectInput(session, "measure", choices = measures, selected = measure_sel)
    }) |> bindEvent(all_regional())

    # cascade value_type
    observeEvent(input$measure, {
      d <- all_regional()
      if (is.null(d) || nrow(d) == 0) return()

      sub <- d[d$employment_group == input$measure, ]
      vtypes <- sort(unique(sub$value_type))
      vtype_sel <- if ("Level" %in% vtypes) "Level" else vtypes[1]
      updateSelectInput(session, "value_type", choices = vtypes, selected = vtype_sel)
    })

    # cascade age_group
    observeEvent(list(input$measure, input$value_type), {
      d <- all_regional()
      if (is.null(d) || nrow(d) == 0 || is.null(input$measure) || is.null(input$value_type)) return()

      sub <- d[d$employment_group == input$measure & d$value_type == input$value_type, ]
      age_groups <- sort(unique(sub$age_group))
      if (length(age_groups) == 0) return()
      age_sel <- age_groups[1]
      updateSelectInput(session, "age_group", choices = age_groups, selected = age_sel)
    })

    # filter + merge
    region_data <- reactive({
      d <- all_regional()
      if (is.null(d) || nrow(d) == 0) return(NULL)
      req(input$measure, input$value_type, input$age_group)

      d <- d[d$employment_group == input$measure &
             d$value_type      == input$value_type &
             d$age_group       == input$age_group, ]

      if (nrow(d) == 0) return(NULL)

      merged <- merge(d, UK_REGIONS, by = "area_code", all.x = FALSE)
      merged$value <- as.numeric(merged$value)
      merged <- merged[!is.na(merged$value), ]
      if (nrow(merged) == 0) return(NULL)
      merged
    })

    # debug output
    output$debug_info <- renderPrint({
      d <- all_regional()
      cat("Query returned:", nrow(d), "rows\n")
      cat("Columns:", paste(names(d), collapse = ", "), "\n")
      cat("ITL1 boundaries:", if (is.null(itl1_geo())) "not available (bubble fallback)" else "loaded (choropleth)", "\n")
      if (nrow(d) > 0) {
        cat("\nUnique employment_group:", paste(unique(d$employment_group), collapse = " | "), "\n")
        cat("Unique value_type:", paste(unique(d$value_type), collapse = " | "), "\n")
        cat("Unique age_group:", paste(unique(d$age_group), collapse = " | "), "\n")

        # combo counts
        combos <- aggregate(area_code ~ employment_group + value_type + age_group, data = d, FUN = length)
        names(combos)[4] <- "n_regions"
        cat("\n--- Valid combinations (regions count) ---\n")
        print(combos)
      }

      rd <- region_data()
      cat("\n--- Current selection ---\n")
      cat("Selected measure:", input$measure, "\n")
      cat("Selected value_type:", input$value_type, "\n")
      cat("Selected age_group:", input$age_group, "\n")
      cat("Filtered rows:", if (is.null(rd)) 0 else nrow(rd), "\n")
    })

    # map plot
output$uk_map <- renderPlotly({
  d   <- region_data()
  geo <- itl1_geo()

  # Bubble map (fallback) as a local helper, so we can drop back to it on ANY
  # problem with the choropleth, not just when boundaries are missing.
  bubble_map <- function() {
    uk_map <- ggplot2::map_data("world", region = c("UK", "Ireland:Northern Ireland"))

    p <- ggplot() +
      geom_polygon(
        data = uk_map,
        aes(x = long, y = lat, group = group),
        fill = "#f3f2f1", colour = "#b1b4b6", linewidth = 0.3
      ) +
      coord_quickmap(xlim = c(-9, 3), ylim = c(49.5, 59.5)) +
      theme_void() +
      theme(
        legend.position = "right",
        plot.background = element_rect(fill = "#e8f4f8", colour = NA)
      )

    if (!is.null(d) && nrow(d) > 0) {
      suffix <- if (grepl("Rate", input$value_type, fixed = TRUE)) "%" else " (000s)"

      vals <- d$value
      size_range <- c(4, 12)
      if (length(unique(vals)) == 1L) {
        d$pt_size <- rep(mean(size_range), length(vals))
      } else {
        d$pt_size <- size_range[1] + (vals - min(vals, na.rm = TRUE)) /
          (max(vals, na.rm = TRUE) - min(vals, na.rm = TRUE)) *
          (size_range[2] - size_range[1])
      }

      d$hover_text <- paste0(
        d$region_name, "\n",
        input$measure, " (", input$value_type, "): ",
        format(round(d$value, 1), big.mark = ","), suffix
      )

      p <- p +
        geom_point(
          data = d,
          aes(x = lon, y = lat, size = pt_size, colour = value),
          alpha = 0.85
        ) +
        scale_colour_gradient(
          low  = "#1d70b8",
          high = "#cf102d",
          name = paste0(input$value_type, suffix)
        ) +
        scale_size_identity()
    }

    ggplotly(p, tooltip = "hover_text") %>%
      layout(
        showlegend = FALSE,
        margin = list(l = 0, r = 0, t = 10, b = 0)
      ) %>%
      config(displayModeBar = FALSE)
  }

  # ---- Choropleth (preferred: real ITL1 region shapes) ----
  if (!is.null(geo) && !is.null(d) && nrow(d) > 0) {
    chor <- tryCatch({
      suffix <- if (grepl("Rate", input$value_type, fixed = TRUE)) "%" else " (000s)"

      d$hover_text <- paste0(
        d$region_name, "<br>",
        input$measure, " (", input$value_type, "): ",
        format(round(d$value, 1), big.mark = ","), suffix
      )

      plot_ly() %>%
        add_trace(
          type         = "choropleth",
          geojson      = geo,
          locations    = d$area_code,
          z            = d$value,
          featureidkey = "properties.area_code",
          text         = d$hover_text,
          hoverinfo    = "text",
          colorscale   = list(c(0, "#1d70b8"), c(1, "#cf102d")),
          marker       = list(line = list(width = 0.5, color = "#ffffff")),
          colorbar     = list(title = paste0(input$value_type, suffix))
        ) %>%
        layout(
          geo = list(
            fitbounds  = "locations",
            visible    = FALSE,
            projection = list(type = "mercator"),
            bgcolor    = "#e8f4f8"
          ),
          margin        = list(l = 0, r = 0, t = 10, b = 0),
          paper_bgcolor = "#e8f4f8"
        ) %>%
        config(displayModeBar = FALSE)
    },
    error = function(e) {
      message("Choropleth render failed, using bubble map: ", conditionMessage(e))
      NULL
    })

    if (!is.null(chor)) return(chor)
  }

  # ---- Fallback: bubble map ----
  bubble_map()
})

# bar chart
output$bar_chart <- renderPlotly({
  d <- region_data()
  req(!is.null(d) && nrow(d) > 0)

  suffix  <- if (grepl("Rate", input$value_type, fixed = TRUE)) "%" else " (000s)"
  x_title <- paste0(input$measure, " ", input$value_type)

  d <- d[order(d$value), ]
  d$region_name <- factor(d$region_name, levels = d$region_name)

  # Precompute tooltip column instead of mapping aes(text = ...)
  d$hover_text <- paste0(
    d$region_name, ": ",
    format(round(d$value, 1), big.mark = ","), suffix
  )

  ggplotly(
    ggplot(d, aes(x = value, y = region_name, fill = value)) +
      geom_col(width = 0.7, show.legend = FALSE) +
      scale_fill_gradient(low = "#1d70b8", high = "#cf102d") +
      labs(x = x_title, y = NULL) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        axis.text.y  = element_text(size = 11),
        plot.margin   = margin(5, 15, 5, 5)
      ),
    tooltip = "hover_text"  # <-- pick up our data column
  ) %>%
    layout(
      xaxis = list(fixedrange = TRUE),
      yaxis = list(fixedrange = TRUE)
    ) %>%
    config(displayModeBar = FALSE)
})

    # data table
    output$region_table <- reactable::renderReactable({
      d <- region_data()
      req(!is.null(d) && nrow(d) > 0)

      display <- d[, c("region_name", "employment_group", "age_group",
                        "value_type", "value")]
      names(display) <- c("Region", "Measure", "Age Group", "Type", "Value")
      display$Value <- round(display$Value, 1)

      reactable::reactable(
        display,
        sortable    = TRUE,
        filterable  = TRUE,
        highlight   = TRUE,
        striped     = TRUE,
        defaultSorted = list(Value = "desc"),
        columns = list(
          Value = reactable::colDef(
            format = reactable::colFormat(separators = TRUE, digits = 1)
          )
        )
      )
    })
  })
}
