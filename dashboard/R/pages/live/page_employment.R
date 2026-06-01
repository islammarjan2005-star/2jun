
# R/pages/page_employment.R


# ---- Employment page ----

library(ggplot2)
library(scales)
library(plotly)

# ROUTER PAGE ----------
employment_ui <- function(id) {
  ns <- NS(id)

  toc_sections <- list(
    list(
      heading = "General",
      items = c("Economic Activity" = "section-economic-activity"
                )
    ),
    list(
      heading = "Employment Status",
      items = c(
                "Employment by age group" = "section-age",
                "Employment by type"    = "section-ftpt",
                "Reason for working part time" = "section-pt-reason"
                )
    ),
    list(
      heading = "Industry",
      items = c(
                "Workforce jobs by industry" = "section-workforce-industry"
                )
    ),
    list(
      heading = "Earnings",
      items = c(
                "Real Average Weekly Earnings" = "section-real-awe",
                "Nominal Average Weekly Earnings"     = "section-awe"
                )
    )
    )

  tagList(
    # Load in the SideBar Nav
    side_nav(ns, sections = toc_sections, title = "On this page"),
    #Set Up Page
    div(class = "govuk-width-container",
        tags$main(class = "govuk-main-wrapper",
                  tags$h1(class = "govuk-heading-xl", "Employment"),
                  tags$h1(class = "mb-0 text-inherit", "General"),
#===== Stats Cards =====
                                       div(class = "govuk-grid-row",
                    employment_employ_kpi_card_ui(ns("employ_card")),
                    employment_employ_rate_kpi_card_ui(ns("employ_rate_card")),
                    employment_awe_totalpay_kpi_card_ui(ns("awe_totalpay_card"))
                                       ),
#===== Overview =====
                  div(class = "govuk-grid-row",
                    div(class = "govuk-grid-column-full",
                    tags$section(id = "section-economic-activity",
                    employment_economic_activity_stats_card_ui(ns("economic_activity"))
                    ))),


      tags$h1(class = "mb-0 text-inherit", "Employment Status"),


#===== Age Employment =====
                  div(class = "govuk-grid-row",
                    div(class = "govuk-grid-column-full",
                    tags$section(id = "section-age",
                    employment_age_stats_card_ui(ns("age_trend_card"))
                    ))),
#===== Full Time/ Part Time =====
                  div(class = "govuk-grid-row",
                    div(class = "govuk-grid-column-full",
                    tags$section(id = "section-ftpt",
                    employment_ftpt_stats_card_ui(ns("ftpt_card"))
                    ))),
#==== Reasons for part time =====
                  div(class = "govuk-grid-row",
                    div(class = "govuk-grid-column-full",
                    tags$section(id = "section-pt-reason",
                    employment_pt_reason_stats_card_ui(ns("pt_reason_card"))
                    ))),

      tags$h1(class = "mb-0 text-inherit", "Industry"),


#===== Workforce Jobs by Industry =====
                  div(class = "govuk-grid-row",
                    div(class = "govuk-grid-column-full",
                    tags$section(id = "section-workforce-industry",
                    employment_workforce_industry_stats_card_ui(ns("workforce_industry_card"))
                    ))),

      tags$h1(class = "mb-0 text-inherit", "Earnings"),


#===== AWE Real =====
                  div(class = "govuk-grid-row",
                    div(class = "govuk-grid-column-full",
                    tags$section(id = "section-real-awe",
                    employment_real_awe_stats_card_ui(ns("real_awe_card"))
                    ))),
#===== AWE Nominal =====
                  div(class = "govuk-grid-row",
                    div(class = "govuk-grid-column-full",
                    tags$section(id = "section-awe",
                    employment_nom_awe_stats_card_ui(ns("awe_card"))
                    )))


# #~~~~SPECIFIC TOPIC SECTION~~~~~
# ,tags$h1(class = "mb-0 text-inherit", "Specific Topic")
)))
}

employment_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    #KPI Cards
    employment_employ_kpi_card_server("employ_card")
    employment_employ_rate_kpi_card_server("employ_rate_card")
    employment_awe_totalpay_kpi_card_server("awe_totalpay_card")

    #Stats Cards
    mod_govuk_data_vis_card_server("trend_card")
    employment_economic_activity_stats_card_server("economic_activity")
    employment_real_awe_stats_card_server("real_awe_card")
    employment_nom_awe_stats_card_server("awe_card")
    employment_ftpt_stats_card_server("ftpt_card")
    employment_age_stats_card_server("age_trend_card")
    employment_pt_reason_stats_card_server("pt_reason_card")
    employment_workforce_industry_stats_card_server("workforce_industry_card")

  })
}
