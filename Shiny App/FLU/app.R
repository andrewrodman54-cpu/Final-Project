# ============================================================
# NYS Flu Incidence & Uninsured Rate Explorer
# VTPEH 6270 – Final Project
# Author: Andrew Rodman
# ============================================================

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

# ── Load & prepare data ────────────────────────────────────
# NOTE: Update these paths to match your local directory.
# The app expects two CSVs in a "Data/" subfolder:
#   1. Influenza_Laboratory-Confirmed_Cases_by_County__Beginning_2009-10_Season_20260202.csv
#   2. SVI_2016_US_county.csv
#
# If the files are not found, the app uses built-in simulated data
# so you can preview the interface immediately.

load_data <- function() {
  flu_path <- "Data/Influenza_Laboratory-Confirmed_Cases_by_County__Beginning_2009-10_Season_20260202.csv"
  svi_path <- "Data/SVI_2016_US_county.csv"
  
  if (file.exists(flu_path) && file.exists(svi_path)) {
    fludata <- read.csv(flu_path)
    svi     <- read.csv(svi_path)
    
    flu_svi <- merge(fludata, svi, by = "FIPS", all.x = TRUE)
    flu_svi$Count             <- as.numeric(flu_svi$Count)
    flu_svi$Week.Ending.Date  <- as.Date(flu_svi$Week.Ending.Date, format = "%m/%d/%Y")
    flu_svi$Season_Year       <- as.numeric(substr(flu_svi$Season, 6, 9))
    
    flu_svi <- flu_svi[, c("Season_Year", "Region", "County",
                           "Week.Ending.Date", "Count", "E_TOTPOP",
                           "EP_POV", "E_PCI", "EP_UNEMP", "FIPS", "EP_UNINSUR")]
    flu_svi <- flu_svi %>%
      rename(Total_Pop        = E_TOTPOP,
             Poverty_Rate     = EP_POV,
             Income_Per_Capita = E_PCI,
             Date             = Week.Ending.Date,
             Unemployment_Rate = EP_UNEMP,
             Uninsured_Rate   = EP_UNINSUR)
    
    flu_svi_year <- flu_svi %>%
      group_by(Season_Year, County) %>%
      summarise(
        Region            = first(Region),
        Total_Count       = sum(Count, na.rm = TRUE),
        Total_Pop         = first(Total_Pop),
        Poverty_Rate      = first(Poverty_Rate),
        Income_Per_Capita = first(Income_Per_Capita),
        Unemployment_Rate = first(Unemployment_Rate),
        Uninsured_Rate    = first(Uninsured_Rate),
        FIPS              = first(FIPS),
        .groups           = "drop"
      ) %>%
      mutate(Incidence_Rate = (Total_Count / Total_Pop) * 100000) %>%
      filter(!is.na(Uninsured_Rate), !is.na(Incidence_Rate),
             !is.na(County), !is.na(Season_Year))
    
    return(flu_svi_year)
  } else {
    # ── Simulated demo data ──────────────────────────────────
    set.seed(42)
    nys_counties <- c(
      "Albany", "Allegany", "Bronx", "Broome", "Cattaraugus", "Cayuga",
      "Chautauqua", "Chemung", "Chenango", "Clinton", "Columbia", "Cortland",
      "Delaware", "Dutchess", "Erie", "Essex", "Franklin", "Fulton",
      "Genesee", "Greene", "Hamilton", "Herkimer", "Jefferson", "Kings",
      "Lewis", "Livingston", "Madison", "Monroe", "Montgomery", "Nassau",
      "New York", "Niagara", "Oneida", "Onondaga", "Ontario", "Orange",
      "Orleans", "Oswego", "Otsego", "Putnam", "Queens", "Rensselaer",
      "Richmond", "Rockland", "St. Lawrence", "Saratoga", "Schenectady",
      "Schoharie", "Schuyler", "Seneca", "Steuben", "Suffolk", "Sullivan",
      "Tioga", "Tompkins", "Ulster", "Warren", "Washington", "Wayne",
      "Westchester", "Wyoming", "Yates"
    )
    
    regions <- c("Capital Region", "Central NY", "Finger Lakes", "Long Island",
                 "Mid-Hudson", "Mohawk Valley", "New York City",
                 "North Country", "Southern Tier", "Western NY")
    
    county_meta <- data.frame(
      County           = nys_counties,
      Region           = sample(regions, length(nys_counties), replace = TRUE),
      Uninsured_Rate   = round(runif(length(nys_counties), 3, 18), 2),
      Poverty_Rate     = round(runif(length(nys_counties), 5, 28), 2),
      Unemployment_Rate = round(runif(length(nys_counties), 3, 12), 2),
      Income_Per_Capita = round(runif(length(nys_counties), 18000, 65000)),
      Total_Pop        = round(runif(length(nys_counties), 5000, 2600000)),
      FIPS             = 36001:(36000 + length(nys_counties)),
      stringsAsFactors = FALSE
    )
    
    years <- 2010:2023
    df_list <- lapply(years, function(yr) {
      d <- county_meta
      d$Season_Year  <- yr
      # Incidence driven primarily by year effect + weak uninsured signal + noise
      year_fx        <- rnorm(1, 0, 80)
      d$Total_Count  <- pmax(0, round(
        (d$Total_Pop / 100000) * (120 + 0.8 * d$Uninsured_Rate + year_fx +
                                    rnorm(nrow(d), 0, 40))
      ))
      d$Incidence_Rate <- (d$Total_Count / d$Total_Pop) * 100000
      d
    })
    do.call(rbind, df_list)
  }
}

flu_data <- load_data()
all_years   <- sort(unique(flu_data$Season_Year))
all_regions <- c("All Regions", sort(unique(flu_data$Region)))

# ── Colour palette ──────────────────────────────────────────
pal_blue  <- "#1B4F72"
pal_teal  <- "#148F77"
pal_light <- "#D6EAF8"
pal_bg    <- "#F0F4F8"
pal_text  <- "#1A2332"
pal_muted <- "#7F8C8D"
pal_warn  <- "#E74C3C"

# ── UI ──────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Source+Serif+4:wght@400;600;700&family=IBM+Plex+Mono:wght@400;500&family=Inter:wght@400;500;600&display=swap",
      rel = "stylesheet"
    ),
    tags$style(HTML(paste0("
      *, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }

      body {
        background-color: ", pal_bg, ";
        color: ", pal_text, ";
        font-family: 'Inter', sans-serif;
        font-size: 14px;
      }

      /* ── Header ── */
      .app-header {
        background: linear-gradient(135deg, ", pal_blue, " 0%, #0E3460 100%);
        color: #fff;
        padding: 28px 36px 22px;
        border-bottom: 4px solid ", pal_teal, ";
      }
      .app-header h1 {
        font-family: 'Source Serif 4', serif;
        font-size: 26px;
        font-weight: 700;
        letter-spacing: -0.3px;
        margin-bottom: 4px;
      }
      .app-header .subtitle {
        font-family: 'IBM Plex Mono', monospace;
        font-size: 11px;
        color: rgba(255,255,255,0.65);
        letter-spacing: 1.5px;
        text-transform: uppercase;
      }
      .header-badge {
        display: inline-block;
        background: rgba(255,255,255,0.15);
        border: 1px solid rgba(255,255,255,0.3);
        color: #fff;
        font-family: 'IBM Plex Mono', monospace;
        font-size: 10px;
        padding: 3px 10px;
        border-radius: 20px;
        margin-top: 10px;
        letter-spacing: 0.8px;
      }

      /* ── Main layout ── */
      .main-layout {
        display: flex;
        gap: 0;
        min-height: calc(100vh - 110px);
      }

      /* ── Sidebar ── */
      .sidebar-panel {
        width: 270px;
        min-width: 270px;
        background: #fff;
        border-right: 1px solid #D5DCE4;
        padding: 24px 20px;
        display: flex;
        flex-direction: column;
        gap: 22px;
      }
      .sidebar-section-title {
        font-family: 'IBM Plex Mono', monospace;
        font-size: 9px;
        letter-spacing: 1.8px;
        text-transform: uppercase;
        color: ", pal_muted, ";
        margin-bottom: 10px;
        border-bottom: 1px solid #E8ECF0;
        padding-bottom: 6px;
      }
      .sidebar-panel label {
        font-size: 12.5px;
        font-weight: 600;
        color: ", pal_text, ";
        margin-bottom: 5px;
        display: block;
      }
      .sidebar-panel select,
      .sidebar-panel input[type='number'] {
        width: 100%;
        border: 1.5px solid #CBD5E1;
        border-radius: 6px;
        padding: 7px 10px;
        font-size: 13px;
        background: #FAFBFC;
        color: ", pal_text, ";
        outline: none;
        transition: border-color 0.2s;
      }
      .sidebar-panel select:focus,
      .sidebar-panel input:focus {
        border-color: ", pal_teal, ";
      }
      .form-group { margin-bottom: 14px; }

      /* ── Action Button ── */
      .btn-primary-custom {
        width: 100%;
        background: ", pal_teal, ";
        color: #fff;
        border: none;
        border-radius: 7px;
        padding: 10px 0;
        font-family: 'Inter', sans-serif;
        font-size: 13px;
        font-weight: 600;
        cursor: pointer;
        letter-spacing: 0.3px;
        transition: background 0.2s, transform 0.1s;
      }
      .btn-primary-custom:hover { background: #0F7060; }
      .btn-primary-custom:active { transform: scale(0.98); }

      /* Shiny's actionButton wrapper */
      #update_plot > button,
      #update_plot {
        width: 100% !important;
        background: ", pal_teal, " !important;
        color: #fff !important;
        border: none !important;
        border-radius: 7px !important;
        padding: 10px 0 !important;
        font-family: 'Inter', sans-serif !important;
        font-size: 13px !important;
        font-weight: 600 !important;
        cursor: pointer !important;
      }

      /* ── Content area ── */
      .content-area {
        flex: 1;
        padding: 26px 32px;
        display: flex;
        flex-direction: column;
        gap: 24px;
        overflow-y: auto;
      }

      /* ── Stat cards ── */
      .stat-row {
        display: grid;
        grid-template-columns: repeat(4, 1fr);
        gap: 14px;
      }
      .stat-card {
        background: #fff;
        border: 1px solid #E2E8F0;
        border-radius: 10px;
        padding: 16px 18px;
        border-top: 3px solid ", pal_teal, ";
      }
      .stat-card .stat-value {
        font-family: 'Source Serif 4', serif;
        font-size: 26px;
        font-weight: 700;
        color: ", pal_blue, ";
        line-height: 1;
      }
      .stat-card .stat-label {
        font-size: 11px;
        color: ", pal_muted, ";
        margin-top: 5px;
        font-weight: 500;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }

      /* ── Tabs ── */
      .nav-tabs {
        border-bottom: 2px solid #E2E8F0;
        margin-bottom: 0;
      }
      .nav-tabs > li > a {
        font-family: 'Inter', sans-serif;
        font-size: 13px;
        font-weight: 600;
        color: ", pal_muted, ";
        border: none !important;
        border-bottom: 3px solid transparent !important;
        border-radius: 0 !important;
        padding: 10px 20px;
        margin-bottom: -2px;
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover {
        color: ", pal_blue, " !important;
        border-bottom: 3px solid ", pal_teal, " !important;
        background: transparent !important;
      }
      .tab-content {
        background: #fff;
        border: 1px solid #E2E8F0;
        border-top: none;
        border-radius: 0 0 10px 10px;
        padding: 22px;
      }

      /* ── Plot ── */
      .plot-container { position: relative; }
      .plot-container .plot-title {
        font-family: 'Source Serif 4', serif;
        font-size: 16px;
        font-weight: 600;
        color: ", pal_blue, ";
        margin-bottom: 14px;
      }

      /* ── Info box ── */
      .info-box {
        background: #EBF5FB;
        border-left: 4px solid ", pal_teal, ";
        border-radius: 0 8px 8px 0;
        padding: 14px 16px;
        font-size: 13px;
        line-height: 1.6;
        color: #2C3E50;
      }
      .info-box strong { color: ", pal_blue, "; }

      /* ── Result box ── */
      .result-box {
        background: #F8FAFC;
        border: 1px solid #E2E8F0;
        border-radius: 8px;
        padding: 16px;
        font-family: 'IBM Plex Mono', monospace;
        font-size: 12px;
        white-space: pre-wrap;
        line-height: 1.7;
        color: ", pal_text, ";
      }

      /* ── About tab cards ── */
      .about-card {
        background: #fff;
        border: 1px solid #E2E8F0;
        border-radius: 10px;
        overflow: hidden;
      }
      .about-card-header {
        background: linear-gradient(90deg, ", pal_blue, " 0%, #1a6b9a 100%);
        color: #fff;
        font-family: 'Source Serif 4', serif;
        font-size: 15px;
        font-weight: 600;
        padding: 12px 18px;
        display: flex;
        align-items: center;
        gap: 10px;
      }
      .about-icon { font-size: 16px; }
      .about-card-body {
        padding: 18px 20px;
        font-size: 13.5px;
        line-height: 1.65;
        color: ", pal_text, ";
      }
      .about-table {
        width: 100%;
        border-collapse: collapse;
        font-size: 13px;
        margin-top: 4px;
      }
      .about-table tr { border-bottom: 1px solid #EDF2F7; }
      .about-table tr:last-child { border-bottom: none; }
      .about-table td {
        padding: 8px 10px;
        vertical-align: top;
      }
      .about-table td:first-child {
        width: 160px;
        font-weight: 600;
        color: ", pal_blue, ";
        white-space: nowrap;
      }
      .about-table td code {
        background: #EDF2F7;
        border-radius: 3px;
        padding: 1px 5px;
        font-family: 'IBM Plex Mono', monospace;
        font-size: 11.5px;
      }

      /* ── Footer ── */
      .app-footer {
        background: #fff;
        border-top: 1px solid #E2E8F0;
        padding: 10px 36px;
        font-size: 11px;
        color: ", pal_muted, ";
        font-family: 'IBM Plex Mono', monospace;
        letter-spacing: 0.4px;
      }
    ")))
  ),
  
  # ── Header ─────────────────────────────────────────────
  div(class = "app-header",
      h1("NYS Flu Incidence & Uninsured Rate Explorer"),
      div(class = "subtitle", "VTPEH 6270 · County-Level Surveillance · 2010–2023"),
      div(class = "header-badge", "Andrew Rodman")
  ),
  
  # ── Body ───────────────────────────────────────────────
  div(class = "main-layout",
      
      # ── Sidebar ────────────────────────────────────────
      div(class = "sidebar-panel",
          
          div(
            div(class = "sidebar-section-title", "About"),
            div(class = "info-box",
                "This app explores the relationship between county-level ",
                strong("uninsured rates"), " and influenza ",
                strong("incidence rates"), " across New York State. ",
                "Data sources: CDC Social Vulnerability Index (SVI) & NYS DOH laboratory-confirmed flu cases."
            )
          ),
          
          div(
            div(class = "sidebar-section-title", "Filters"),
            
            div(class = "form-group",
                selectInput("region_select", "NYS Region",
                            choices  = all_regions,
                            selected = "All Regions")
            ),
            
            div(class = "form-group",
                sliderInput("year_range", "Season Year Range",
                            min   = min(all_years),
                            max   = max(all_years),
                            value = c(min(all_years), max(all_years)),
                            step  = 1, sep = "")
            ),
            
            div(class = "form-group",
                selectInput("color_by", "Color Points By",
                            choices  = c("Region", "Uninsured Rate Quartile" = "Quartile",
                                         "Season Year" = "Season_Year"),
                            selected = "Region")
            ),
            
            div(class = "form-group",
                checkboxInput("show_lm",    "Show regression line",  value = TRUE),
                checkboxInput("show_se",    "Show confidence band",  value = TRUE),
                checkboxInput("show_loess", "Show LOESS smoother",   value = FALSE)
            ),
            
            actionButton("update_plot", "Update Plot",
                         class = "btn btn-primary")
          ),
          
          div(
            div(class = "sidebar-section-title", "Reference"),
            div(style = "font-size:11.5px; color:#5D6D7E; line-height:1.7;",
                "Model: Mixed Effects Negative Binomial",
                tags$br(),
                "Random effects: County, Season Year",
                tags$br(),
                HTML("Key finding: Uninsured rate <strong>not significant</strong> (p&nbsp;=&nbsp;0.419)"),
                tags$br(),
                "Year variance dominates (σ² = 1.58)"
            )
          )
      ),
      
      # ── Content ────────────────────────────────────────
      div(class = "content-area",
          
          # Stat cards
          div(class = "stat-row",
              div(class = "stat-card",
                  div(class = "stat-value", textOutput("n_counties", inline = TRUE)),
                  div(class = "stat-label", "Counties")
              ),
              div(class = "stat-card",
                  div(class = "stat-value", textOutput("n_obs", inline = TRUE)),
                  div(class = "stat-label", "Observations")
              ),
              div(class = "stat-card",
                  div(class = "stat-value", textOutput("avg_uninsured", inline = TRUE)),
                  div(class = "stat-label", "Avg Uninsured %")
              ),
              div(class = "stat-card",
                  div(class = "stat-value", textOutput("avg_incidence", inline = TRUE)),
                  div(class = "stat-label", "Avg Incidence / 100k")
              )
          ),
          
          # Tabs
          tabsetPanel(id = "main_tabs",
                      
                      # Tab 1 – Scatter
                      tabPanel("Scatter Plot",
                               div(class = "plot-container", style = "margin-top:20px;",
                                   div(class = "plot-title", "Uninsured Rate vs. Flu Incidence Rate"),
                                   plotOutput("scatter_plot", height = "420px")
                               )
                      ),
                      
                      # Tab 2 – Time trend
                      tabPanel("Trend Over Time",
                               div(style = "margin-top:20px;",
                                   div(class = "plot-title", "Mean Flu Incidence Rate by Season Year"),
                                   plotOutput("trend_plot", height = "420px")
                               )
                      ),
                      
                      # Tab 3 – Distribution
                      tabPanel("Distribution",
                               div(style = "margin-top:20px;",
                                   div(class = "plot-title", "Distribution of Key Variables"),
                                   fluidRow(
                                     column(6, plotOutput("hist_uninsured", height = "300px")),
                                     column(6, plotOutput("hist_incidence",  height = "300px"))
                                   ),
                                   div(style = "margin-top:18px;",
                                       div(class = "plot-title", "Incidence by NYS Region (Box Plot)"),
                                       plotOutput("box_region", height = "320px")
                                   )
                               )
                      ),
                      
                      # Tab 4 – Summary stats
                      tabPanel("Summary Statistics",
                               div(style = "margin-top:20px;",
                                   div(class = "plot-title", "Descriptive Statistics (Filtered Data)"),
                                   div(class = "result-box", verbatimTextOutput("summary_stats")),
                                   div(style = "margin-top:18px;",
                                       div(class = "plot-title", "Pearson Correlation: Uninsured Rate vs Incidence"),
                                       div(class = "result-box", verbatimTextOutput("correlation_out"))
                                   )
                               )
                      ),
                      
                      # Tab 5 – About
                      tabPanel("About",
                               div(style = "margin-top:20px; max-width:780px; display:flex; flex-direction:column; gap:22px;",
                                   
                                   # Author
                                   div(class = "about-card",
                                       div(class = "about-card-header",
                                           tags$span(class = "about-icon", "👤"),
                                           "Author Information"
                                       ),
                                       div(class = "about-card-body",
                                           tags$table(class = "about-table",
                                                      tags$tr(tags$td("Name"),        tags$td("Andrew Rodman")),
                                                      tags$tr(tags$td("Course"),      tags$td("VTPEH 6270")),
                                                      tags$tr(tags$td("Assignment"),  tags$td("Final Project")),
                                                      tags$tr(tags$td("Date"),        tags$td("May 9th 2026")),
                                                      tags$tr(tags$td("GitHub"),
                                                              tags$td(tags$a(href = "https://github.com/andrewrodman54-cpu/Final-Project",
                                                                             "github.com/andrewrodman54-cpu/Final-Project",
                                                                             target = "_blank")))
                                           )
                                       )
                                   ),
                                   
                                   # Research question & data sources
                                   div(class = "about-card",
                                       div(class = "about-card-header",
                                           tags$span(class = "about-icon", "🗄️"),
                                           "Data Sources"
                                       ),
                                       div(class = "about-card-body",
                                           tags$p(style = "margin-bottom:12px;",
                                                  tags$strong("Research Question: "),
                                                  "Do uninsured rates at the county level affect influenza incidence rates in New York State?"
                                           ),
                                           tags$table(class = "about-table",
                                                      tags$tr(
                                                        tags$td("Flu Case Data"),
                                                        tags$td(HTML("NYS Department of Health — <em>Influenza Laboratory-Confirmed Cases by County, Beginning 2009–10 Season</em>. Collected via public health surveillance; cases reported by healthcare providers and labs statewide. Aggregated from weekly to annual counts per county."))
                                                      ),
                                                      tags$tr(
                                                        tags$td("SVI Data"),
                                                        tags$td(HTML("CDC Social Vulnerability Index (SVI) 2016 — U.S. County Level. Derived from publicly available U.S. Census community survey data. Variable used: <code>EP_UNINSUR</code> (% population uninsured). Additional covariates: <code>EP_POV</code> (poverty rate), <code>E_PCI</code> (per capita income), <code>EP_UNEMP</code> (unemployment rate)."))
                                                      ),
                                                      tags$tr(
                                                        tags$td("Linkage"),
                                                        tags$td("Datasets merged on county FIPS code. Incidence rate calculated as (annual case count / total population) × 100,000.")
                                                      )
                                           )
                                       )
                                   ),
                                   
                                   # Methods
                                   div(class = "about-card",
                                       div(class = "about-card-header",
                                           tags$span(class = "about-icon", "📐"),
                                           "Statistical Methods"
                                       ),
                                       div(class = "about-card-body",
                                           tags$p(style = "margin-bottom:10px;",
                                                  "A ", tags$strong("Mixed Effects Negative Binomial model"), " (glmmTMB, nbinom2 family) was selected to account for:"
                                           ),
                                           tags$ul(style = "margin:0 0 12px 18px; line-height:2;",
                                                   tags$li("Repeated measures — the same counties appear across multiple seasons"),
                                                   tags$li("Non-normality and overdispersion in count-derived incidence rates"),
                                                   tags$li("Clustering at both the county and season-year level")
                                           ),
                                           tags$table(class = "about-table",
                                                      tags$tr(tags$td("Fixed Effect"),    tags$td("Uninsured Rate (%)")),
                                                      tags$tr(tags$td("Random Effects"),  tags$td("(1 | County) + (1 | Season_Year)")),
                                                      tags$tr(tags$td("Key Result"),      tags$td(HTML("Uninsured rate: <strong>not statistically significant</strong> (p = 0.419)"))),
                                                      tags$tr(tags$td("Variance (Year)"), tags$td("1.583 (SD = 1.26) — dominant driver")),
                                                      tags$tr(tags$td("Variance (County)"), tags$td("0.087 (SD = 0.29)")),
                                                      tags$tr(tags$td("Assumption Checks"), tags$td("DHARMa simulated residuals: uniformity, dispersion, zero-inflation, outlier tests"))
                                           ),
                                           tags$p(style = "margin-top:12px; color:#5D6D7E; font-size:12.5px;",
                                                  "Results differed from the literature, likely because inter-annual variation in flu burden (driven by circulating strain and vaccination match) overshadows county-level SVI predictors when repeated-measures structure is properly accounted for."
                                           )
                                       )
                                   ),
                                   
                                   # AI Disclosure
                                   div(class = "about-card",
                                       div(class = "about-card-header",
                                           tags$span(class = "about-icon", "🤖"),
                                           "AI Use Disclosure"
                                       ),
                                       div(class = "about-card-body",
                                           tags$p(
                                             tags$strong("Claude (Anthropic)"), " was used in two ways during this project:"
                                           ),
                                           tags$ul(style = "margin:8px 0 0 18px; line-height:2.1;",
                                                   tags$li("To ", tags$strong("identify an appropriate statistical test"), " for a complex repeated-measures, overdispersed count dataset (leading to the Mixed Effects Negative Binomial model)."),
                                                   tags$li("To ", tags$strong("generate DHARMa assumption-checking code"), " (residual simulations, uniformity, dispersion, zero-inflation, and outlier tests)."),
                                                   tags$li("To ", tags$strong("build this Shiny application"), " (Final Project).")
                                           ),
                                           tags$p(style = "margin-top:10px; color:#5D6D7E; font-size:12.5px;",
                                                  "All interpretations, conclusions, and scientific framing reflect the author's own analysis."
                                           )
                                       )
                                   )
                                   
                               )
                      )
          )
      )
  ),
  
  # ── Footer ─────────────────────────────────────────────
  div(class = "app-footer",
      "VTPEH 6270 · Check Point 07 · Andrew Rodman · Data: CDC SVI & NYS DOH · 2026"
  )
)

# ── Server ──────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # Reactive filtered dataset (updates only when button clicked)
  filtered <- eventReactive(input$update_plot, {
    d <- flu_data
    if (input$region_select != "All Regions")
      d <- d %>% filter(Region == input$region_select)
    d <- d %>% filter(Season_Year >= input$year_range[1],
                      Season_Year <= input$year_range[2])
    d <- d %>% mutate(
      Quartile = cut(Uninsured_Rate, breaks = quantile(Uninsured_Rate,
                                                       probs = 0:4/4, na.rm = TRUE), include.lowest = TRUE,
                     labels = c("Q1 (Low)", "Q2", "Q3", "Q4 (High)"))
    )
    d
  }, ignoreNULL = FALSE)
  
  # ── Stat cards ──────────────────────────────────────────
  output$n_counties   <- renderText(length(unique(filtered()$County)))
  output$n_obs        <- renderText(nrow(filtered()))
  output$avg_uninsured <- renderText(
    paste0(round(mean(filtered()$Uninsured_Rate, na.rm = TRUE), 1), "%")
  )
  output$avg_incidence <- renderText(
    round(mean(filtered()$Incidence_Rate, na.rm = TRUE), 0)
  )
  
  # ── Shared theme ───────────────────────────────────────
  base_theme <- function() {
    theme_minimal(base_family = "sans") +
      theme(
        plot.background    = element_rect(fill = "#FFFFFF", color = NA),
        panel.background   = element_rect(fill = "#FAFBFC", color = NA),
        panel.grid.major   = element_line(color = "#E8ECF0", linewidth = 0.5),
        panel.grid.minor   = element_blank(),
        axis.title         = element_text(size = 11, color = "#4A5568", face = "bold"),
        axis.text          = element_text(size = 10, color = "#718096"),
        plot.title         = element_text(size = 14, face = "bold", color = pal_blue, family = "serif"),
        plot.subtitle      = element_text(size = 11, color = pal_muted),
        legend.title       = element_text(size = 10, face = "bold", color = pal_text),
        legend.text        = element_text(size = 9,  color = pal_text),
        strip.text         = element_text(face = "bold", color = pal_blue)
      )
  }
  
  # ── Tab 1: Scatter ─────────────────────────────────────
  output$scatter_plot <- renderPlot({
    d <- filtered()
    req(nrow(d) > 0)
    
    color_var <- input$color_by
    
    p <- ggplot(d, aes(x = Uninsured_Rate, y = Incidence_Rate,
                       color = .data[[color_var]])) +
      geom_point(alpha = 0.65, size = 2.4, stroke = 0.3) +
      labs(
        x     = "Uninsured Rate (%)",
        y     = "Flu Incidence Rate (per 100,000)",
        color = gsub("_", " ", color_var),
        caption = "Each point = one county–season. Model: Mixed Effects Negative Binomial (p = 0.419)."
      ) +
      base_theme() +
      theme(plot.caption = element_text(size = 9, color = pal_muted, hjust = 0))
    
    if (input$show_lm) {
      p <- p + geom_smooth(method = "lm", se = input$show_se,
                           color = "#E74C3C", fill = "#FADBD8",
                           linewidth = 1, inherit.aes = FALSE,
                           aes(x = Uninsured_Rate, y = Incidence_Rate))
    }
    if (input$show_loess) {
      p <- p + geom_smooth(method = "loess", se = FALSE,
                           color = "#8E44AD", linetype = "dashed",
                           linewidth = 0.9, inherit.aes = FALSE,
                           aes(x = Uninsured_Rate, y = Incidence_Rate))
    }
    
    # Color scale
    if (color_var == "Season_Year") {
      p <- p + scale_color_viridis_c(option = "plasma")
    } else if (color_var == "Quartile") {
      p <- p + scale_color_manual(values = c("#2ECC71","#3498DB","#E67E22","#E74C3C"))
    } else {
      n_reg <- length(unique(d$Region))
      cols  <- colorRampPalette(c(pal_teal, "#2980B9","#8E44AD",
                                  "#E74C3C","#E67E22","#27AE60",
                                  "#1A5276","#7D6608","#4A235A","#1B2631"))(n_reg)
      p <- p + scale_color_manual(values = cols)
    }
    p
  }, bg = "white")
  
  # ── Tab 2: Time trend ──────────────────────────────────
  output$trend_plot <- renderPlot({
    d <- filtered()
    req(nrow(d) > 0)
    
    trend <- d %>%
      group_by(Season_Year) %>%
      summarise(
        Mean_IR  = mean(Incidence_Rate, na.rm = TRUE),
        SE_IR    = sd(Incidence_Rate,   na.rm = TRUE) / sqrt(n()),
        .groups  = "drop"
      )
    
    ggplot(trend, aes(x = Season_Year, y = Mean_IR)) +
      geom_ribbon(aes(ymin = Mean_IR - 1.96*SE_IR, ymax = Mean_IR + 1.96*SE_IR),
                  fill = pal_light, alpha = 0.7) +
      geom_line(color  = pal_blue,  linewidth = 1.2) +
      geom_point(color = pal_teal, size = 3.5, fill = "#fff", shape = 21, stroke = 2) +
      scale_x_continuous(breaks = unique(trend$Season_Year)) +
      labs(
        x       = "Season Year",
        y       = "Mean Flu Incidence Rate (per 100,000)",
        caption = "Ribbon = 95% CI around mean across counties. Year-to-year variation dominates (σ² = 1.58)."
      ) +
      base_theme() +
      theme(axis.text.x    = element_text(angle = 45, hjust = 1),
            plot.caption   = element_text(size = 9, color = pal_muted, hjust = 0))
  }, bg = "white")
  
  # ── Tab 3a: Histograms ─────────────────────────────────
  output$hist_uninsured <- renderPlot({
    d <- filtered()
    ggplot(d, aes(x = Uninsured_Rate)) +
      geom_histogram(bins = 20, fill = pal_teal, color = "#fff", alpha = 0.85) +
      labs(x = "Uninsured Rate (%)", y = "Count") +
      base_theme()
  }, bg = "white")
  
  output$hist_incidence <- renderPlot({
    d <- filtered()
    ggplot(d, aes(x = Incidence_Rate)) +
      geom_histogram(bins = 20, fill = pal_blue, color = "#fff", alpha = 0.85) +
      labs(x = "Flu Incidence Rate (per 100,000)", y = "Count") +
      base_theme()
  }, bg = "white")
  
  # ── Tab 3b: Box plot by region ─────────────────────────
  output$box_region <- renderPlot({
    d <- filtered()
    req(nrow(d) > 0)
    
    region_order <- d %>%
      group_by(Region) %>%
      summarise(med = median(Incidence_Rate, na.rm = TRUE)) %>%
      arrange(med) %>% pull(Region)
    
    d$Region <- factor(d$Region, levels = region_order)
    
    ggplot(d, aes(x = Region, y = Incidence_Rate, fill = Region)) +
      geom_boxplot(outlier.size = 1.2, outlier.alpha = 0.5,
                   color = "#4A5568", alpha = 0.8, linewidth = 0.4) +
      scale_fill_brewer(palette = "Set3") +
      coord_flip() +
      labs(x = NULL, y = "Flu Incidence Rate (per 100,000)") +
      guides(fill = "none") +
      base_theme()
  }, bg = "white")
  
  # ── Tab 4: Summary stats ───────────────────────────────
  output$summary_stats <- renderPrint({
    d <- filtered()
    vars <- d %>% select(Incidence_Rate, Uninsured_Rate,
                         Poverty_Rate, Unemployment_Rate, Income_Per_Capita)
    summary(vars)
  })
  
  output$correlation_out <- renderPrint({
    d <- filtered()
    ct <- cor.test(d$Uninsured_Rate, d$Incidence_Rate, method = "pearson")
    cat("Pearson r  =", round(ct$estimate, 4), "\n")
    cat("95% CI     = [", round(ct$conf.int[1], 4), ",",
        round(ct$conf.int[2], 4), "]\n")
    cat("t-statistic =", round(ct$statistic, 3), "\n")
    cat("p-value    =", format.pval(ct$p.value, digits = 4), "\n")
    cat("\nNote: Mixed Effects NB model result: p = 0.419 (not significant).\n")
    cat("Year-to-year variation is the dominant driver of flu incidence.\n")
  })
}

shinyApp(ui = ui, server = server)
