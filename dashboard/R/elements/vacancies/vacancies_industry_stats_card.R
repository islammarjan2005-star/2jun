vacancies_industry_stats_card_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # ---- Filters Row ----
    #tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "equal-height-buttons.css")),

    # ---- Your existing card ----
    mod_govuk_data_vis_card_ui(
      id = ns("vacancies_trend_card"),
      publish_marker = "public",
      title = "Vacancies by Industry",
      help_text = "Seasonally Adjusted Vacancies by industry in the United Kingdom",
      help_text_source = "Source: ONS - A01 Labour market statistics summary data table 21",
      help_link = "https://data.trade.gov.uk/datasets/4609dc12-0dfa-4734-8ecb-6c50b59d163d",
      help_link_text = "ONS Labour Market Overview",
      table_content = reactableOutput(ns("table"), height = "350px"),
      visual_content = plotlyOutput(ns("plot"),  height = "350px"),
      query = ns("sql_query"),
        controls =  list(
          mod_filter_picker_ui(
          id = ns("sector_filter"),
          label = "Sector Filtering",
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

## INDUSTRY SERVER ## ----
vacancies_industry_stats_card_server <- function(id, conn = APP_DB$pool) {
  moduleServer(id, function(input, output, session){

## 0) Defaults + annotations
    date_lines  <- mod_annotation_line_server("date_lines",  type = "date")
    value_lines <- mod_annotation_line_server("value_lines", type = "value")
    shinyWidgets::updateRadioGroupButtons(session, "chart_type", selected = "line")


## 1) Get a full clean lazy table
  cleaned_full_tbl <- reactive({
      get_vacancy_by_industry_tbl()
    })

## 2) Build query variables from UI inputs
     # Sector filtering
    sector <- mod_filter_picker_server(
      id       = "sector_filter",
      data_tbl = cleaned_full_tbl,
      column   = "business_metric",
      defaults_pretty = c("All vacancies")
    )
    #Date Filtering
    dates <- mod_quick_date_range_server(
      id        = "dates",
      data_tbl  = cleaned_full_tbl,
      presets   = c("custom", "ytd", "past_year", "3y", "5y", "none"),
      default   = "5y", frequency = "monthly",
      custom_picker = "slider",
      preserve_selection = TRUE
    )

## 3) Collect all variable inputs that drive the query
    query_inputs <- reactive({
      list(
        sectors = sector$selected(),
        dates = dates$date_range()
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
          where_in     = setNames(list(query_inputs_deb()$sectors), "business_metric")  # list(<col> = <values>)
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

    #Plot
     output$plot <- plotly::renderPlotly({
      out <- dat(); req(nrow(out$data) > 0)

      # Treemap: snapshot of the latest period in the selected range
      if (identical(input$chart_type, "treemap")) {
        df <- out$data
        snap_date <- max(df$time_period, na.rm = TRUE)
        snap <- df[!is.na(df$time_period) & df$time_period == snap_date, , drop = FALSE]
        req(nrow(snap) > 0)

        dbt_build_treemap(
          data            = snap,
          label_col       = "business_metric",
          value_col       = "value",
          palette         = dbt_palettes$gaf,
          exclude_pattern = "^(All|Total)\\b",   # drop "All vacancies", "Total services", etc.
          title           = paste0("Latest period: ", format(snap_date, "%b %Y"))
        )
      } else {
        dbt_ts_plot(
          df = out$data,
          chart_type = input$chart_type,
          bar_interval = input$stack_mode,
          bar_agg = "last", y_title = "No.",
          palette = dbt_palettes$gaf, initial_legend_mode = "hidden",
          group_col = "business_metric",
          vlines = date_lines$values_out(), vline_labels = date_lines$labels_out(),
          hlines = value_lines$values_out(), hline_labels = value_lines$labels_out()
        )
      }
    })

  })
}

## VACANCIES BY INDUSTRY DATA ## ----
get_vacancy_by_industry_tbl <- function(){

  base <- dplyr::tbl(APP_DB$pool, dbplyr::in_schema("ons", "labour_market__vacancies_industry"))

  #Time period Parsing and Conversion

  # Build SQL fragment for time_period parsing (or NULL)
  time_sql <- .date_sql_for("MMM-MMM YYYY", "time_period")


  base <- base %>%
    dplyr::mutate(time_period = !!time_sql,
                  value = value*1000,
                  business_metric = dplyr::case_when(
                            business_metric == "Accomoda-tion & food service activities" ~ "Accomodation & food service activities",
                            business_metric == "Administra-tive & support service activities" ~ "Administrative & support service activities",
                            business_metric == "All vacancies" ~ "All vacancies",
                            business_metric == "Arts, entertainment & recreation" ~ "Arts, entertainment & recreation",
                            business_metric == "Construc-tion" ~ "Construction",
                            business_metric == "Education" ~ "Education",
                            business_metric == "Electricity, gas, steam & air conditioning supply" ~ "Electricity, gas, steam & air conditioning supply",
                            business_metric == "Financial & insurance activities" ~ "Financial & insurance activities",
                            business_metric == "Human health & social work activities" ~ "Human health & social work activities",
                            business_metric == "Information & communica-tion" ~ "Information & communication",
                            business_metric == "Manu- facturing" ~ "Manufacturing",
                            business_metric == "Mining & quarrying" ~ "Mining & quarrying",
                            business_metric == "Motor Trades" ~ "Motor Trades",
                            business_metric == "Other service activities" ~ "Other service activities",
                            business_metric == "Professional scientific & technical activities" ~ "Professional scientific & technical activities",
                            business_metric == "Public admin & defence; compulsory social security" ~ "Public admin & defence; compulsory social security",
                            business_metric == "Real estate activities" ~ "Real estate activities",
                            business_metric == "Retail" ~ "Retail",
                            business_metric == "Total services" ~ "Total services",
                            business_metric == "Transport & storage" ~ "Transport & storage",
                            business_metric == "Water supply, sewerage, waste & remediation activities" ~ "Water supply, sewerage, waste & remediation activities",
                            business_metric == "Wholesale" ~ "Wholesale",
                            business_metric == "Wholesale & retail trade; repair of motor vehicles and motor cycles" ~ "Wholesale & retail trade; repair of motor vehicles and motor cycles",
                            TRUE ~ business_metric
                          )
                  )

  base
  }
