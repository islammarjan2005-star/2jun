employment_workforce_industry_stats_card_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # ---- Filters Row ----
    #tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "equal-height-buttons.css")),

    # ---- Your existing card ----
    mod_govuk_data_vis_card_ui(
      id = ns("workforce_industry_card"),
      publish_marker = "public",
      title = "Workforce Jobs by Industry",
      help_text = "Seasonally adjusted workforce jobs by industry (SIC 2007 section) in the United Kingdom. The treemap shows the latest period within the selected time range; the bar/line/area charts show the trend over time.",
      help_text_source = "Source: ONS - Workforce jobs by industry",
      help_link = "https://data.trade.gov.uk/datasets/4609dc12-0dfa-4734-8ecb-6c50b59d163d",
      help_link_text = "ONS Labour Market Overview",
      table_content = reactableOutput(ns("table"), height = "350px"),
      visual_content = plotlyOutput(ns("plot"),  height = "350px"),
      query = ns("sql_query"),
        controls =  list(
          mod_filter_picker_ui(
          id = ns("industry_filter"),
          label = "Industry Filtering",
          multiple = TRUE,
          actions_box = TRUE,
          live_search = TRUE,
          virtual_scroll = 10,
          selected_text_format = "count > 2"
        ),
        mod_quick_date_range_ui(
          id = ns("dates"),
          label_quick = "Time Period",
          label_picker  = "Time period",
          custom_picker = "calendar",
          presets = c("custom", "ytd", "past_year", "3y", "5y", "none")
        )

      ),
        accordion_controls = list(
                        shinyWidgets::radioGroupButtons(
                      inputId = ns("chart_type"),
                      label   = "Choose a graph :",
                      choiceNames = list(
                        tags$span(`data-toggle`="tooltip", title = "Stacked Bar Chart", tags$i(class = "fa fa-bar-chart")),
                        tags$span(`data-toggle`="tooltip", title = "Line Chart",        tags$i(class = "fa fa-line-chart")),
                        tags$span(`data-toggle`="tooltip", title = "Area Chart",        tags$i(class = "fa fa-area-chart")),
                        tags$span(`data-toggle`="tooltip", title = "Treemap (latest period)", tags$i(class = "fa fa-th-large"))
                      ),
                      choiceValues = c("stacked_bar","line","stacked_area","treemap"),
                      justified = TRUE,
                      size = "sm",
                      status = "danger"
                    ),

                    conditionalPanel(
                      condition = sprintf("input['%s'] == 'stacked_bar'", ns("chart_type")),
                      shinyWidgets::sliderTextInput(
                        inputId = ns("stack_mode"),
                        label = "Time Interval between bars",
                        choices = c("monthly","quarterly","annually","5year","decade"),
                        selected = "annually",
                        grid = TRUE
                      )
                    ),

                    conditionalPanel(
                      condition = sprintf("input['%s'] == 'treemap'", ns("chart_type")),
                      shinyWidgets::sliderTextInput(
                        inputId = ns("top_n"),
                        label   = "Industries to show",
                        choices = c("10", "15", "20", "All"),
                        selected = "All",
                        grid = TRUE
                      )
                    ),

                    mod_annotation_line_ui(ns("date_lines"),
                          type = "date",
                          title = "Add key dates",
                          add_label = "Add key date",
                          show_delete = TRUE,
                          auto_mask_date = TRUE),

                    mod_annotation_line_ui(ns("value_lines"),
                          type = "value",
                          title = "Add key values",
                          add_label = "Add key values",
                          show_delete = TRUE,
                          auto_mask_date = TRUE)

                  )
    )
  )
}

## WORKFORCE JOBS BY INDUSTRY SERVER ## ----
employment_workforce_industry_stats_card_server <- function(id, conn = APP_DB$pool) {
  moduleServer(id, function(input, output, session){

## 0) Defaults + annotations
    date_lines  <- mod_annotation_line_server("date_lines",  type = "date")
    value_lines <- mod_annotation_line_server("value_lines", type = "value")
    shinyWidgets::updateRadioGroupButtons(session, "chart_type", selected = "treemap")

## 1) Get a full clean lazy table
  cleaned_full_tbl <- reactive({
      get_workforce_jobs_by_industry_tbl()
    })

## 2) Build query variables from UI inputs
     # Industry filtering
    industry <- mod_filter_picker_server(
      id       = "industry_filter",
      data_tbl = cleaned_full_tbl,
      column   = "industry"
    )
    #Date Filtering
    dates <- mod_quick_date_range_server(
      id        = "dates",
      data_tbl  = cleaned_full_tbl,
      presets   = c("custom", "ytd", "past_year", "3y", "5y", "none"),
      default   = "5y", frequency = "quarterly",
      custom_picker = "slider",
      preserve_selection = TRUE
    )

## 3) Collect all variable inputs that drive the query
    query_inputs <- reactive({
      list(
        industries = industry$selected(),
        dates      = dates$date_range()
      )
    })

# 4) Debounce the *grouped* inputs (i.e, puts a delay on input from UI to it triggering the reactive, this prevents the query from updating too frequently, which causes instability in the app)
    query_inputs_deb <- debounce(query_inputs, 300)

# 5) Build data lazily off the debounced inputs
    dat <- eventReactive(query_inputs_deb(), {
      dr <- query_inputs_deb()$dates
      req(!is.null(dr), length(dr) == 2, !any(is.na(dr)))
      date_from <- as.Date(dr[[1]])
      date_to   <- as.Date(dr[[2]])

      t <- cleaned_full_tbl() %>%
        apply_filters_general(
          date_col  = "time_period",
          date_from = date_from,
          date_to   = date_to,
          where_in  = setNames(list(query_inputs_deb()$industries), "industry")  # list(<col> = <values>)
        )

      list(
        data = t %>% dplyr::collect(),
        sql  = sql_render_pool_safe(t)
      )
    })

# 6) Outputs
    # Table
    output$table <- reactable::renderReactable({
      out <- dat()
      req(nrow(out$data) > 0)

      dbt_build_table(
        data = out$data,
        formatted_cols = c("value")   # <- just change this
      )
    })

    #SQL String
    output$sql_query <- renderText({ dat()$sql })

    #Plot (treemap snapshot OR time-series, depending on chart type)
     output$plot <- plotly::renderPlotly({
      out <- dat(); req(nrow(out$data) > 0)
      df <- out$data

      # This is a "by industry" card, so drop aggregate/total rows
      # ("All jobs", "Total services") from the charts. Keep them only if
      # that would otherwise leave nothing to plot.
      keep <- !grepl("^(All|Total)\\b", df$industry, ignore.case = TRUE, perl = TRUE)
      if (any(keep)) df <- df[keep, , drop = FALSE]
      req(nrow(df) > 0)

      # ---- Treemap: snapshot of the latest period in the selected range ----
      if (identical(input$chart_type, "treemap")) {
        snap_date <- max(df$time_period, na.rm = TRUE)
        snap <- df[!is.na(df$time_period) & df$time_period == snap_date, , drop = FALSE]

        # Optional Top-N trimming (largest industries by jobs)
        n_sel <- input$top_n
        if (!is.null(n_sel) && !identical(n_sel, "All")) {
          n_keep <- suppressWarnings(as.integer(n_sel))
          if (!is.na(n_keep)) {
            snap <- snap[order(-snap$value), , drop = FALSE]
            snap <- utils::head(snap, n_keep)
          }
        }
        req(nrow(snap) > 0)

        return(
          dbt_build_treemap(
            data       = snap,
            label_col  = "industry",
            value_col  = "value",
            palette    = dbt_palettes$gaf,
            title      = paste0("Latest period: ", format(snap_date, "%b %Y"))
          )
        )
      }

      # ---- Time series: stacked bar / line / area ----
      dbt_ts_plot(
        df = df,
        chart_type = input$chart_type,
        bar_interval = input$stack_mode,
        bar_agg = "last", y_title = "No.",
        palette = dbt_palettes$gaf, initial_legend_mode = "hidden",
        group_col = "industry",
        vlines = date_lines$values_out(), vline_labels = date_lines$labels_out(),
        hlines = value_lines$values_out(), hline_labels = value_lines$labels_out()
      )
    })

  })
}

## WORKFORCE JOBS BY INDUSTRY DATA ## ----
get_workforce_jobs_by_industry_tbl <- function(){

  base <- dplyr::tbl(APP_DB$pool, dbplyr::in_schema("ons", "labour_market__workforce_jobs"))

  #Time period Parsing and Conversion

  # Build SQL fragment for time_period parsing ("Mar 88" / "Dec 25 (p)" -> date)
  # Recent periods carry provisional "(p)" / revised "(r)" flags, so use the
  # flag-aware variant of the "MMM YY" parser.
  time_sql <- .date_sql_for("MMM YY (P/R)", "time_period")

  base <- base %>%
    dplyr::mutate(
      time_period = !!time_sql,
      value       = value * 1000,
      # Strip soft word-break hyphens that the source inserts mid-word
      # e.g. "Accommod-ation" -> "Accommodation", "Manu- facturing" -> "Manufacturing"
      industry    = !!dbplyr::sql("regexp_replace(industry, '([[:alpha:]])-[[:space:]]*([[:alpha:]])', '\\1\\2', 'g')")
    )

  base
  }
