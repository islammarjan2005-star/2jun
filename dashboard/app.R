#hi4
# app.R -------------------------------------------------------

# Core packages
options(warn = 1)

#renv::restore()

# Core packages
library(shiny)
library(shinyBS)
library(bslib)
library(dplyr)
library(stringr)
library(lubridate)
suppressWarnings(library(ggplot2))
suppressWarnings(library(plotly))
library(DBI)
library(RPostgres)
library(shinyGovstyle)
library(shinyWidgets)
library(rlang)
library(shiny.router)
library(bs4Dash)
#devtools::install_git("git@gitlab.data.trade.gov.uk:analysis_and_wages/utils/lmutils.git", upgrade = 'never')
#library(lmutils)
#devtools::install_git("git@gitlab.data.trade.gov.uk:ag-data-science/utils/rdwutils.git", upgrade = 'never')
#library(rdwutils)
library(pool)
library(DT)
library(reactable)
#devtools::install_git("git@gitlab.data.trade.gov.uk:dbt-data-visualisation-library/dbtplotr.git", upgrade = 'never')
library(remotes)
suppressWarnings(library(dbtplotr))
library(maps)
library(bsicons)



#Small helpers -----

`%||%` <- function(x, y) if (!is.null(x)) x else y

# ---- Recursive sourcing helper ----
source_recursive <- function(path, env = parent.frame()) {
  files <- list.files(
    path,
    pattern = "\\.R$",
    recursive = TRUE,      # discovers ALL subfolders
    full.names = TRUE
  )
  for (f in files) {
    message("Sourcing: ", f)
    source(f, local = env)
  }
}

#Create single environment for whole app
app_env <- environment()

#Create a DB pool to manage connections to the DB..
APP_DB <- new.env(parent = emptyenv())

APP_DB$pool <- pool::dbPool(
  drv      = RPostgres::Postgres(),
  host     = Sys.getenv("PGHOST"),
  port     = Sys.getenv("PGPORT"),
  dbname   = Sys.getenv("PGDATABASE"),
  user     = Sys.getenv("PGUSER"),
  password = Sys.getenv("PGPASSWORD")
)


shiny::onStop(function() {
  pool::poolClose(APP_DB$pool)
})

# ---- Load all app modules in correct dependency order ----
source_recursive("R/helpers",    env = app_env)
source_recursive("R/components", env = app_env)
source_recursive("R/elements",   env = app_env)
source_recursive("R/pages",      env = app_env)


# ------------------------------------------------------------
# Inputs
# ------------------------------------------------------------

tabs <- c(Home = "home", Employment = "employment", Unemployment = "unemployment", Vacancies = "vacancies", `Regional Map` = "regional_map")
default_route <- "home"
style <- "DBT"
lfs_tables_full <- get_latest_lfs_table_list(APP_DB$pool)

# ------------------------------------------------------------
# 404 page UI
# ------------------------------------------------------------
page_404_ui <- tags$div(
  class = "govuk-width-container",
  tags$main(class = "govuk-main-wrapper",
            tags$h1(class = "govuk-heading-l", "Page not found"),
            tags$p(class = "govuk-body", "Use the navigation to choose a page."),
            tags$a(class = "govuk-link", href = paste0("#/", default_route), "Back to Home"))
)
# ------------------------------------------------------------
# UI
# ------------------------------------------------------------


ui <- fluidPage(


  tags$head(
    tags$style(HTML("
      #initial-loading {
        position: fixed;
        inset: 0;
        background: white;
        z-index: 9999;
        display: flex;
        align-items: center;
        justify-content: center;
        flex-direction: column;
      }

      .loader {
        width: 48px;
        height: 48px;
        border: 5px solid #b1b4b6;
        border-top-color: #cf102d;
        border-radius: 50%;
        animation: spin 1s linear infinite;
      }

      @keyframes spin {
        to { transform: rotate(360deg); }
      }
    ")),

    tags$script(HTML("
      Shiny.addCustomMessageHandler('removeLoader', function(_) {
        const el = document.getElementById('initial-loading');
        if (el) el.remove();
      });
    "))
  ),

  tags$div(
    id = "initial-loading",
    tags$div(class = "loader"),
    tags$p(
      class = "govuk-heading-m govuk-!-margin-top-4",
      "Loading Labour Markets Dashboard…"
    )
  ),


  # Head assets & design system
  govuk_apply_rebrand(),
  govuk_head_assets(),
  govuk_dependencies(style = style, include_ons = TRUE),
  DBT_widget_styling(),
  govuk_toc_assets(),
  ukhsa_card_tabs_assets(
    accent_underline  = FALSE,
    button_label_mode = "wrap",     # or "ellipsis"g
    mobile_stack      = TRUE        # stack on small screens (optional)
),
stringDateListInitJS(),  # register the one-time JS handler

  # Full-page wrapper
  tags$div(
    class = "govuk-template",

    # Header
    govuk_header(service_name = "Labour Markets Dashboard",
                 style = style,
                 home_href = if (style == "GOVUK") {"https://www.gov.uk/"} else {"https://data.trade.gov.uk/"}, # Replace later with link back to datawroksapce page for the LM dashboard
                 service_href = "#!/", default_route),

    # Top navigation
    uiOutput("topnav"),



    # Inline router: same style as the vignette
    router_ui(
      route("/",            home_ui("home")),
      route("home",         home_ui("home")),
      route("employment",   employment_ui("employment")),
      route("unemployment", unemployment_ui("unemployment")),
      route("vacancies",    vacancies_ui("vacancies")),
      route("regional_map", regional_map_ui("regional_map")),
      page_404 = page_404_ui
    ),


    # Footer (using shinyGovstyle)
    footer(TRUE)

  )
)




# ------------------------------------------------------------
# Server (NO lazy mounting for now)
# ------------------------------------------------------------
server <- function(input, output, session) {


  session$onFlushed(function() {
    session$sendCustomMessage("removeLoader", TRUE)
  }, once = TRUE)



  # Start router — IMPORTANT: no arguments here in this inline pattern
  router_server()

  # Watch hash changes and update nav
  observeEvent(session$clientData$url_hash, {
    # Extract route from hash (strip "#/" if present)
    route <- sub("^#!/?", "", session$clientData$url_hash)
    sel <- if (!nzchar(route)) "home" else route

    output$topnav <- renderUI({
      govuk_top_nav(tabs, selected = sel, id_prefix = "nav")
    })
  })


  # Eagerly mount all page servers for now (no lazy load)
  home_server("home")
  employment_server("employment")
  unemployment_server("unemployment")
  vacancies_server("vacancies")
  regional_map_server("regional_map")
}



shinyApp(ui, server)
