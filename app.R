# =============================================================================
# Canadian Crude Price Index — Sweet (SW)
# Interactive Shiny dashboard built on the broker-normalized trade data.
#   comm index = VWAP of all 4 brokers ; 1a index = VWAP of ICE + OneX.
# Run:  shiny::runApp()  from this folder  (or click "Run App" in RStudio)
# =============================================================================

suppressPackageStartupMessages({
  library(shiny); library(bslib); library(plotly); library(DT)
  library(dplyr); library(arrow)
})

# Allow broker file uploads up to 100 MB (default Shiny limit is 5 MB).
options(shiny.maxRequestSize = 100 * 1024^2)

# ---- Source the data pipeline + analytics ----
for (f in c("config", "helpers", "load_reference",
            "normalize_ice", "normalize_modern", "normalize_neon", "normalize_onex",
            "build_dataset", "indices", "plots")) {
  source(file.path("R", paste0(f, ".R")))
}

# ---- Initial data load (build cache if missing) ----
initial_data <- tryCatch(load_combined(), error = function(e) {
  warning("Initial data load failed: ", conditionMessage(e)); NULL
})

app_theme <- bs_theme(
  version = 5, bootswatch = "cosmo",
  primary = "#0d6efd",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter"),
  "navbar-bg" = "#0b1f3a"
)

month_choices <- function(months) {
  if (length(months) == 0) return(NULL)
  setNames(as.character(months), format(months, "%b %Y"))
}

# =============================================================================
# UI
# =============================================================================
ui <- page_navbar(
  title = "Canadian Crude Index · Sweet",
  theme = app_theme,
  fillable = FALSE,
  id = "nav",
  header = tags$style(HTML("
    .container, .container-lg, .container-md, .container-sm, .container-xl {
      max-width: 100% !important; padding-left: 22px; padding-right: 22px;
    }
    .bslib-value-box .value-box-title { font-size: .8rem; opacity: .85; }
    .bslib-value-box .value-box-value { font-size: 1.55rem; font-weight: 600; }
    .card-header { font-weight: 600; font-size: .92rem; }
    .navbar-brand { font-weight: 700; letter-spacing: .2px; }
    /* Single scrollbar: table grows naturally (no inner scroll); header sticks on scroll */
    #t_monthly .dataTables_scrollHead {
      position: sticky !important; top: 0 !important; z-index: 5; background: #fff;
      box-shadow: 0 2px 3px -2px rgba(0,0,0,.25);
    }
    /* In normal view let the table card grow to content so the page is the only scroller */
    .card:has(#t_monthly):not([data-full-screen='true']),
    .card:has(#t_monthly):not([data-full-screen='true']) .card-body {
      overflow: visible !important; height: auto !important; max-height: none !important;
      min-height: 0 !important; flex: 0 0 auto !important;
    }
  ")),

  sidebar = sidebar(
    width = 290,
    title = "Filters",
    selectInput("ig", "Index group", choices = ACTIVE_INDEX_GROUPS, selected = "SW"),
    selectInput("definition", "Index (Monitor / hourly)",
                choices = c("comm — all 4 brokers" = "comm",
                            "1a — ICE + OneX" = "1a",
                            "ICE only" = "ICE", "Modern only" = "Modern",
                            "Neon only" = "Neon", "OneX only" = "OneX"),
                selected = "comm"),
    selectizeInput("grades", "Grades (KeyID)", choices = NULL, multiple = TRUE,
                   options = list(placeholder = "All grades")),
    hr(),
    helpText(icon("circle-info"), "Index = volume-weighted avg differential to WTI CMA",
             "($/bbl) from physical outright trades inside the trade cycle."),
    div(class = "text-muted small", textOutput("built_at")),
    hr(),
    div(class = "small fw-semibold mb-1", "Broker raw files"),
    div(class = "small text-muted mb-1",
        "Drop an .xlsx into a broker, then click Rebuild."),
    do.call(accordion, c(
      list(id = "raw_files", open = FALSE, class = "raw-files-accordion"),
      lapply(BROKERS, function(b) {
        accordion_panel(
          title = uiOutput(paste0("rawhdr_", b), inline = TRUE),
          value = b,
          fileInput(paste0("upload_", b), NULL,
                    accept = c(".xlsx", ".xls", ".csv"),
                    buttonLabel = "Browse", placeholder = "No file",
                    width = "100%"),
          uiOutput(paste0("rawlist_", b))
        )
      })
    )),
    actionButton("refresh", "Rebuild from broker files", icon = icon("rotate"),
                 class = "btn-outline-primary btn-sm w-100 mt-2")
  ),

  # ---------------------------------------------------------------- Monitor ----
  nav_panel(
    "Trade Cycle Monitor", icon = icon("gauge-high"),
    layout_columns(
      col_widths = c(4, 8),
      card(card_header("Delivery month"),
           selectInput("dm", NULL, choices = NULL, width = "100%"),
           uiOutput("cycle_window")),
      uiOutput("kpis")
    ),
    card(
      full_screen = TRUE,
      card_header("Index development through the trade cycle (accumulated VWAP + daily volume)"),
      plotlyOutput("p_cycle", height = "330px")
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(full_screen = TRUE, card_header("Broker contribution"),
           plotlyOutput("p_broker", height = "260px")),
      card(full_screen = TRUE, card_header("Grade contribution"),
           plotlyOutput("p_grade", height = "260px"))
    ),
    card(full_screen = TRUE,
         card_header("Volume by time of day (trade-cycle trades)"),
         div(class = "px-1 pt-1",
             selectizeInput("hour_months", "Months (delivery)", choices = NULL, multiple = TRUE,
                            width = "100%", options = list(placeholder = "All months")),
             div(class = "d-flex flex-wrap gap-3 align-items-end",
                 selectInput("hour_gran", "Granularity", width = "120px",
                             choices = c("Hour" = "60", "30 min" = "30", "15 min" = "15",
                                         "5 min" = "5", "1 min" = "1"), selected = "60"),
                 radioButtons("hour_by", "Break down by", inline = TRUE,
                              choices = c("None" = "none", "Grade" = "grade", "Broker" = "broker"),
                              selected = "none"),
                 checkboxGroupInput("hour_dows", "Weekdays", inline = TRUE,
                                    choices = c("Mon" = 1, "Tue" = 2, "Wed" = 3, "Thu" = 4, "Fri" = 5),
                                    selected = c(1, 2, 3, 4, 5)))),
         plotlyOutput("p_hour", height = "300px")),
    card(full_screen = TRUE,
         card_header("Volume seasonality vs other trade cycles"),
         div(class = "px-1 pt-1",
             selectizeInput("season_months", "Months to compare", choices = NULL,
                            multiple = TRUE, width = "100%",
                            options = list(placeholder = "All months")),
             div(class = "d-flex flex-wrap gap-3 align-items-end",
                 radioButtons("season_x", "X-axis", inline = TRUE,
                              choices = c("Trading day" = "day", "% of cycle" = "pct"),
                              selected = "day"),
                 selectInput("season_y", "Volume axis", width = "215px",
                             choices = c("Daily — m³" = "daily_raw",
                                         "Daily — % of cycle" = "daily_pct",
                                         "Accumulated — m³" = "cum_raw",
                                         "Accumulated — % of cycle" = "cum_pct"),
                             selected = "daily_raw"),
                 radioButtons("season_chart", "Chart", inline = TRUE,
                              choices = c("Line" = "line", "Bar" = "bar"), selected = "line"),
                 conditionalPanel(
                   condition = "input.season_chart == 'bar'",
                   radioButtons("season_bar_by", "Break down by", inline = TRUE,
                                choices = c("Grade" = "grade", "Broker" = "broker"),
                                selected = "grade")),
                 radioButtons("season_color", "Colour (line)", inline = TRUE,
                              choices = c("Month" = "month", "Season" = "season"), selected = "month"),
                 radioButtons("season_avg", "Reference (line)", inline = TRUE,
                              choices = c("None" = "none", "Mean" = "mean", "Median" = "median"),
                              selected = "none"),
                 conditionalPanel(
                   condition = "input.season_chart == 'bar' && input.season_x == 'pct'",
                   selectInput("season_bin", "% bin width", width = "105px",
                               choices = c("5%" = "5", "10%" = "10", "20%" = "20", "25%" = "25"),
                               selected = "10")))),
         div(class = "small text-muted px-1",
             "Line: one cycle per line, selected month bold. Bar: stacked by grade or broker — pick a few months to compare."),
         plotlyOutput("p_season", height = "360px"))
  ),

  # --------------------------------------------------------------- Overview ----
  nav_panel(
    "Indices Overview", icon = icon("chart-line"),
    card(full_screen = TRUE,
         card_header("Monthly index — comm, 1a & each broker (filtered by selected grades)"),
         plotlyOutput("p_monthly", height = "380px")),
    card(full_screen = TRUE,
         card_header(div("Monthly index table",
                         downloadButton("dl_monthly", "CSV", class = "btn-sm float-end"))),
         DTOutput("t_monthly"))
  ),

  # -------------------------------------------------------- Index Trades ----
  nav_panel(
    "Index-Referenced Trades", icon = icon("file-invoice-dollar"),
    layout_columns(
      col_widths = c(4, 8),
      card(card_header("Delivery month"),
           selectInput("dm_ref", NULL, choices = NULL, width = "100%"),
           div(class = "small text-muted mt-2",
               tags$b("Trades executed AT a published index"), " (price flat to comm / 1a / Bi4). ",
               "Shows the volume & participation riding on the index — not a price. ",
               "The index ", tags$i("value"), " is the reconstructed VWAP on the other tabs.")),
      uiOutput("ref_kpis")
    ),
    card(full_screen = TRUE,
         card_header("At-index volume by month and index type"),
         plotlyOutput("p_ref_monthly", height = "300px")),
    layout_columns(
      col_widths = c(6, 6),
      card(full_screen = TRUE, card_header("By broker"),
           plotlyOutput("p_ref_broker", height = "260px")),
      card(full_screen = TRUE, card_header("By grade"),
           plotlyOutput("p_ref_grade", height = "260px"))
    ),
    card(full_screen = TRUE,
         card_header(div("Index-referenced trades",
                         downloadButton("dl_ref", "CSV", class = "btn-sm float-end"))),
         DTOutput("t_ref"))
  ),

  # ------------------------------------------------------------- Historical ----
  nav_panel(
    "Historical Analysis", icon = icon("clock-rotate-left"),
    layout_columns(
      col_widths = c(6, 6),
      card(full_screen = TRUE,
           card_header("Volume by cycle position (before / in / after)"),
           plotlyOutput("p_cmp_vol", height = "300px")),
      card(full_screen = TRUE,
           card_header(div(class = "d-flex justify-content-between align-items-center",
                           span("Price by cycle position"),
                           radioButtons("price_measure", NULL, inline = TRUE,
                                        choices = c("VWAP" = "vwap", "Simple avg" = "avg_price"),
                                        selected = "vwap"))),
           plotlyOutput("p_cmp_price", height = "300px"))
    ),
    card(full_screen = TRUE,
         card_header("Hour-of-day profile across all history"),
         plotlyOutput("p_hour_hist", height = "300px"))
  ),

  # --------------------------------------------------------- Brokers/Grades ----
  nav_panel(
    "Brokers & Grades", icon = icon("layer-group"),
    card(full_screen = TRUE,
         card_header("Per-broker monthly VWAP (dispersion around the combined index)"),
         plotlyOutput("p_dispersion", height = "330px")),
    layout_columns(
      col_widths = c(6, 6),
      card(full_screen = TRUE, card_header("Broker volume share over time"),
           plotlyOutput("p_share", height = "300px")),
      card(full_screen = TRUE, card_header("Grade volume mix over time"),
           plotlyOutput("p_grademix", height = "300px"))
    )
  ),

  # ------------------------------------------------------------ Candlesticks ----
  nav_panel(
    "Candlesticks", icon = icon("chart-column"),
    card(full_screen = TRUE,
         card_header("Price candlesticks & volume"),
         div(class = "d-flex flex-wrap gap-3 align-items-end px-1 pt-1",
             selectizeInput("cs_products", "Product(s) — grade", choices = NULL, multiple = TRUE,
                            width = "230px", options = list(placeholder = "Pick grade(s)")),
             selectInput("cs_dm", "Delivery month", choices = NULL, width = "190px"),
             selectInput("cs_gran", "Granularity", width = "120px",
                         choices = c("Day" = "day", "Hour" = "60", "30 min" = "30",
                                     "15 min" = "15", "5 min" = "5", "1 min" = "1"),
                         selected = "day")),
         div(class = "small text-muted px-1",
             "One product → candlestick (open/high/low/close); several → close-price comparison lines. ",
             "Drag the range slider (or use the toolbar) to zoom / scroll time."),
         plotlyOutput("p_candles", height = "480px"))
  ),

  # ------------------------------------------------------------- Data / QA ----
  nav_panel(
    "Data & QA", icon = icon("table"),
    card(card_header("Coverage by broker"), DTOutput("t_coverage")),
    card(full_screen = TRUE,
         card_header(div(
           span(icon("triangle-exclamation"), " Unparseable delivery months ",
                tags$span(class = "badge bg-warning text-dark", textOutput("n_unparsed", inline = TRUE))),
           downloadButton("dl_unparsed", "CSV", class = "btn-sm float-end"))),
         div(class = "small text-muted px-1 pb-1",
             "Physical trades with a grade but a period that didn't resolve to a month ",
             "(e.g. quarterly strips like \"Q3 25\"). These are excluded from every index — ",
             "fix the period at source or extend the parser to include them."),
         DTOutput("t_unparsed")),
    card(full_screen = TRUE,
         card_header(div("Normalized trades",
                         downloadButton("dl_data", "CSV", class = "btn-sm float-end"))),
         DTOutput("t_data"))
  ),

  nav_spacer(),
  nav_item(tags$span(class = "navbar-text small", "VWAP · m³ · WTI CMA basis"))
)

# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {

  rv <- reactiveValues(data = initial_data, built = if (file.exists(QA_FILE))
    tryCatch(readRDS(QA_FILE)$built_at, error = function(e) Sys.time()) else Sys.time())

  dat <- reactive({ validate(need(rv$data, "No data available. Click 'Rebuild from broker files'.")); rv$data })

  # Rebuild pipeline
  observeEvent(input$refresh, {
    showNotification("Rebuilding from broker files…", type = "message", duration = 4)
    res <- tryCatch({ d <- build_dataset(write_cache = TRUE); rv$data <- d
      rv$built <- Sys.time(); TRUE },
      error = function(e) { showNotification(paste("Build failed:", conditionMessage(e)),
                                             type = "error", duration = 8); FALSE })
    if (isTRUE(res)) showNotification("Data rebuilt.", type = "message", duration = 3)
  })

  output$built_at <- renderText(paste("Data built:", format(rv$built, "%Y-%m-%d %H:%M")))

  # ---- Broker raw-file browser & uploader ----
  # Bumped after every successful upload so the per-broker listings refresh.
  raw_tick <- reactiveVal(0)

  .raw_files <- function(b) {
    dir <- file.path(DATA_DIR, BROKER_FOLDER[[b]])
    if (!dir.exists(dir)) return(character(0))
    list.files(dir, pattern = "\\.(xlsx|xls|csv)$", ignore.case = TRUE)
  }

  lapply(BROKERS, function(b) {
    output[[paste0("rawhdr_", b)]] <- renderUI({
      raw_tick()
      n <- length(.raw_files(b))
      tagList(strong(b), span(class = "text-muted ms-1 small", sprintf("(%d)", n)))
    })

    output[[paste0("rawlist_", b)]] <- renderUI({
      raw_tick()
      files <- .raw_files(b)
      if (length(files) == 0) {
        return(div(class = "small text-muted fst-italic", "(no files yet)"))
      }
      div(class = "small",
          div(class = "text-muted mb-1", sprintf("In %s/:", BROKER_FOLDER[[b]])),
          tags$ul(class = "mb-0 ps-3",
                  lapply(sort(files), function(f) tags$li(f))))
    })

    observeEvent(input[[paste0("upload_", b)]], {
      inp <- input[[paste0("upload_", b)]]
      req(inp)
      dir <- file.path(DATA_DIR, BROKER_FOLDER[[b]])
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
      dest <- file.path(dir, inp$name)
      replaced <- file.exists(dest)
      ok <- tryCatch({ file.copy(inp$datapath, dest, overwrite = TRUE); TRUE },
                     error = function(e) {
                       showNotification(paste("Save failed:", conditionMessage(e)),
                                        type = "error", duration = 8); FALSE
                     })
      if (isTRUE(ok)) {
        msg <- if (replaced) sprintf("Replaced %s in %s.", inp$name, b)
               else sprintf("Saved %s to %s. Click Rebuild to refresh data.", inp$name, b)
        showNotification(msg, type = "message", duration = 5)
        raw_tick(raw_tick() + 1)
      }
    })
  })

  # Keep selectors in sync with the data + index group
  observe({
    d <- dat(); ig <- input$ig %||% "SW"
    g <- available_grades(d, ig)
    updateSelectizeInput(session, "grades", choices = g, server = TRUE)
    m <- available_months(d, ig)
    sel <- if (length(m)) as.character(max(m)) else NULL
    updateSelectInput(session, "dm", choices = month_choices(m), selected = sel)
    updateSelectizeInput(session, "season_months", choices = month_choices(m), server = TRUE)
    updateSelectizeInput(session, "hour_months", choices = month_choices(m), server = TRUE)
    updateSelectizeInput(session, "cs_products", choices = g,
                         selected = if ("PCE" %in% g) "PCE" else g[1], server = TRUE)
    updateSelectInput(session, "cs_dm",
                      choices = c("All cycles (continuous)" = "all", month_choices(m)), selected = "all")
  })

  sel_grades <- reactive({ if (length(input$grades)) input$grades else NULL })
  sel_dm <- reactive({ req(input$dm); as.Date(input$dm) })

  # ----------------------------------------------------------- Monitor ----
  output$cycle_window <- renderUI({
    d <- dat(); k <- month_kpis(d, sel_dm(), input$ig, input$definition, sel_grades())
    tcw <- d %>% filter(delivery_month == sel_dm(), index_group == input$ig) %>%
      summarise(s = suppressWarnings(min(trade_cycle_start, na.rm = TRUE)),
                e = suppressWarnings(max(trade_cycle_end, na.rm = TRUE)))
    div(class = "small text-muted mt-2",
        sprintf("Trade cycle: %s → %s", format(tcw$s, "%b %d"), format(tcw$e, "%b %d, %Y")),
        br(), sprintf("Trades observed: %s → %s",
                      format(k$first, "%b %d"), format(k$last, "%b %d")))
  })

  output$kpis <- renderUI({
    d <- dat(); k <- month_kpis(d, sel_dm(), input$ig, input$definition, sel_grades())
    layout_columns(
      col_widths = breakpoints(sm = 6, lg = 3), fill = FALSE,
      value_box(paste0(input$definition, " index"), fmt_diff(k$index),
                showcase = icon("arrow-trend-up"), theme = "primary"),
      value_box("Volume", fmt_int(k$vol_m3),
                showcase = icon("droplet"), theme = "secondary",
                p(class = "small mb-0", paste0(fmt_int(k$vol_bbl), " bbl"))),
      value_box("Trades", fmt_int(k$trades),
                showcase = icon("right-left"), theme = "secondary",
                p(class = "small mb-0", paste(k$n_brokers, "brokers ·", k$n_grades, "grades"))),
      value_box("Brokers in", paste0(k$n_brokers, " / 4"),
                showcase = icon("building"), theme = "secondary")
    )
  })

  output$p_cycle <- renderPlotly({
    plot_cycle_development(cycle_daily(dat(), sel_dm(), input$ig, input$definition, sel_grades()))
  })
  output$p_broker <- renderPlotly({
    plot_contribution(broker_contribution(dat(), sel_dm(), input$ig, input$definition, sel_grades()),
                      "broker", NULL)
  })
  output$p_grade <- renderPlotly({
    plot_contribution(grade_contribution(dat(), sel_dm(), input$ig, input$definition, sel_grades()),
                      "grade", NULL)
  })
  output$p_hour <- renderPlotly({
    by <- input$hour_by %||% "none"; by <- if (by == "none") NULL else by
    months <- if (length(input$hour_months)) as.Date(input$hour_months) else NULL
    dows <- if (length(input$hour_dows)) as.integer(input$hour_dows) else integer(0)
    validate(need(length(dows) > 0, "Select at least one weekday."))
    prof <- intraday_profile(dat(), input$ig, input$definition, sel_grades(),
                             dms = months, dows = dows, in_cycle = TRUE,
                             bucket_min = as.numeric(input$hour_gran %||% 60), by = by)
    plot_intraday(prof)
  })
  output$p_season <- renderPlotly({
    months <- if (length(input$season_months)) as.Date(input$season_months) else NULL
    x <- input$season_x %||% "day"; y <- input$season_y %||% "daily_raw"
    if ((input$season_chart %||% "line") == "bar") {
      seg_by <- input$season_bar_by %||% "grade"
      sg <- cycle_seasonality(dat(), input$ig, input$definition, sel_grades(), by = seg_by)
      plot_seasonality_bar(sg, x, y, months = months, bin = as.numeric(input$season_bin %||% 10),
                           seg_type = seg_by)
    } else {
      s <- cycle_seasonality(dat(), input$ig, input$definition, sel_grades())
      plot_seasonality_line(s, x, y, color_by = input$season_color %||% "month",
                            months = months, avg = input$season_avg %||% "none",
                            highlight = sel_dm())
    }
  })

  # ----------------------------------------------------------- Overview ----
  monthly_cmp <- reactive(monthly_compare_full(dat(), input$ig, sel_grades()))
  output$p_monthly <- renderPlotly({
    d <- dat(); g <- sel_grades()
    lbl <- if (length(g)) paste0("(", paste(g, collapse = ", "), ")") else "(all SW grades)"
    plot_monthly_index_multi(
      monthly_index(d, input$ig, "comm", g, by_grade = FALSE),
      monthly_index(d, input$ig, "1a", g, by_grade = FALSE),
      broker_monthly_vwap(d, input$ig, g), lbl)
  })
  output$t_monthly <- renderDT({
    m <- monthly_cmp()
    if (nrow(m) == 0) return(datatable(data.frame(Message = "No data")))
    series <- c("comm", "1a", "ICE", "Modern", "Neon", "OneX")
    series <- series[paste0(series, "_vwap") %in% names(m)]
    disp <- data.frame(Month = format(m$delivery_month, "%b %Y"), check.names = FALSE)
    for (s in series) {
      disp[[paste0(s, "_vwap")]] <- round(m[[paste0(s, "_vwap")]], 3)
      disp[[paste0(s, "_vol")]]  <- m[[paste0(s, "_vol_m3")]]
      disp[[paste0(s, "_trd")]]  <- m[[paste0(s, "_trades")]]
    }
    container <- htmltools::withTags(table(
      class = "display",
      thead(
        tr(th(rowspan = 2, "Month"),
           lapply(series, function(s)
             th(colspan = 3, s, style = "text-align:center; border-left:2px solid #ccd;"))),
        tr(lapply(series, function(s) tagList(
          th("VWAP", style = "border-left:2px solid #ccd;"), th("Vol m³"), th("Trd"))))
      )))
    datatable(disp, container = container, rownames = FALSE,
              options = list(dom = "t", scrollX = TRUE, paging = FALSE,
                             columnDefs = list(list(targets = "_all", className = "dt-right")))) %>%
      formatStyle(paste0(series, "_vwap"),
                  color = styleInterval(0, c("#b02a37", "#198754")), fontWeight = "500") %>%
      formatCurrency(paste0(series, "_vol"), currency = "", interval = 3, mark = ",", digits = 0)
  })
  output$dl_monthly <- downloadHandler(
    filename = function() paste0("monthly_index_by_broker_", input$ig, ".csv"),
    content = function(file) write.csv(monthly_cmp(), file, row.names = FALSE))

  # ----------------------------------------------- Index-Referenced Trades ----
  observe({
    d <- dat(); ig <- input$ig %||% "SW"
    m <- index_ref_months(d, ig)
    updateSelectInput(session, "dm_ref",
                      choices = c("All delivery months" = "all", month_choices(m)),
                      selected = "all")
  })
  ref_dm <- reactive({
    if (is.null(input$dm_ref) || input$dm_ref == "all") NULL else as.Date(input$dm_ref)
  })

  output$ref_kpis <- renderUI({
    k <- index_ref_kpis(dat(), input$ig, sel_grades(), ref_dm())
    layout_columns(
      col_widths = breakpoints(sm = 6, lg = 3), fill = FALSE,
      value_box("At-index volume", fmt_int(k$vol), showcase = icon("droplet"), theme = "primary",
                p(class = "small mb-0", paste(k$trades, "trades ·", k$n_brokers, "brokers"))),
      value_box("comm volume (4 brokers)", fmt_int(k$comm), showcase = icon("layer-group"), theme = "secondary"),
      value_box("1a volume (ICE+OneX)", fmt_int(k$a1), showcase = icon("code-branch"), theme = "secondary"),
      value_box("In-cycle volume", fmt_int(k$in_cycle), showcase = icon("calendar-check"), theme = "secondary")
    )
  })
  output$p_ref_monthly <- renderPlotly(plot_index_ref_monthly(index_ref_monthly(dat(), input$ig, sel_grades())))
  output$p_ref_broker <- renderPlotly(
    plot_index_ref_bars(index_ref_by(dat(), input$ig, sel_grades(), ref_dm(), "broker"), "broker", NULL))
  output$p_ref_grade <- renderPlotly(
    plot_index_ref_bars(index_ref_by(dat(), input$ig, sel_grades(), ref_dm(), "grade"), "grade", NULL))
  output$t_ref <- renderDT({
    d <- index_ref_trades(dat(), input$ig, sel_grades(), dm = ref_dm()) %>%
      transmute(Broker = broker, `Trade ID` = trade_id, Date = exec_date,
                Type = index_type, Instrument = instrument, Grade = grade,
                Period = period, `Deliv. month` = delivery_month,
                `Qty m³` = qty_m3, `Price diff` = round(price, 3),
                `In cycle` = in_trade_cycle, `Price basis` = price_basis)
    datatable(d, rownames = FALSE, filter = "top",
              options = list(pageLength = 20, scrollX = TRUE, dom = "tip"))
  })
  output$dl_ref <- downloadHandler(
    filename = function() paste0("index_referenced_", input$ig, ".csv"),
    content = function(file) write.csv(
      index_ref_trades(dat(), input$ig, sel_grades(), dm = ref_dm()), file, row.names = FALSE))

  # --------------------------------------------------------- Historical ----
  hist_cmp <- reactive(historic_cycle_compare(dat(), input$ig, sel_grades()))
  output$p_cmp_vol   <- renderPlotly(plot_cycle_compare(hist_cmp(), "vol_m3"))
  output$p_cmp_price <- renderPlotly(plot_cycle_compare(hist_cmp(), input$price_measure %||% "vwap"))
  output$p_hour_hist <- renderPlotly({
    plot_hourly(hourly_profile(dat(), input$ig, input$definition, sel_grades(), in_cycle = TRUE))
  })

  # ----------------------------------------------------- Brokers & Grades ----
  output$p_dispersion <- renderPlotly(plot_broker_dispersion(broker_monthly_vwap(dat(), input$ig, sel_grades())))
  output$p_share <- renderPlotly({
    d <- component_trades(dat(), input$ig, sel_grades(), NULL, in_cycle = TRUE)
    if (nrow(d) == 0) return(empty_plot())
    s <- d %>% group_by(delivery_month, broker) %>%
      summarise(vol = sum(qty_m3, na.rm = TRUE), .groups = "drop")
    p <- plot_ly()
    for (b in sort(unique(s$broker))) {
      db <- s[s$broker == b, ]
      p <- p %>% add_bars(data = db, x = ~delivery_month, y = ~vol, name = b,
                          marker = list(color = unname(BROKER_COLORS[b])))
    }
    p %>% .base_layout(ylab = "Volume (m³)") %>% layout(barmode = "stack")
  })
  output$p_grademix <- renderPlotly({
    d <- component_trades(dat(), input$ig, sel_grades(), NULL, in_cycle = TRUE)
    if (nrow(d) == 0) return(empty_plot())
    s <- d %>% group_by(delivery_month, grade) %>%
      summarise(vol = sum(qty_m3, na.rm = TRUE), .groups = "drop")
    gl <- sort(unique(s$grade))
    plot_ly(s, x = ~delivery_month, y = ~vol, color = ~factor(grade, levels = gl),
            colors = grade_color(gl), type = "bar") %>%
      .base_layout(ylab = "Volume (m³)") %>% layout(barmode = "stack")
  })

  # --------------------------------------------------------- Candlesticks ----
  output$p_candles <- renderPlotly({
    g <- if (length(input$cs_products)) input$cs_products else NULL
    validate(need(length(g) > 0, "Select at least one product (grade)."))
    dm <- if (is.null(input$cs_dm) || input$cs_dm == "all") NULL else as.Date(input$cs_dm)
    ohlc <- ohlc_profile(dat(), input$ig, input$definition, grades = g, dm = dm,
                         bucket = input$cs_gran %||% "day", in_cycle = TRUE)
    plot_candles(ohlc)
  })

  # ----------------------------------------------------------- Data / QA ----
  output$t_coverage <- renderDT({
    qa <- build_qa(dat())
    datatable(qa$coverage, rownames = FALSE,
              colnames = c("Broker", "Oldest trade", "Latest trade", "Rows", "SW rows",
                           "Component trades", "In-cycle components", "No rule match (incl. non-SW)"),
              options = list(dom = "t", pageLength = 10))
  })
  unparsed_tbl <- reactive(unparsed_month_trades(dat()))
  output$n_unparsed <- renderText(nrow(unparsed_tbl()))
  output$t_unparsed <- renderDT({
    u <- unparsed_tbl()
    if (nrow(u) == 0)
      return(datatable(tibble(Message = "None — every physical trade resolved to a delivery month."),
                       rownames = FALSE, options = list(dom = "t")))
    datatable(u %>% transmute(Broker = broker, `Trade ID` = trade_id, Date = exec_date,
                              Group = index_group, Grade = grade, Instrument = instrument,
                              Period = period, Price = round(price, 3), `Qty m³` = qty_m3,
                              `Source file` = source_file),
              rownames = FALSE, filter = "top",
              options = list(pageLength = 15, scrollX = TRUE, dom = "tip"))
  })
  output$dl_unparsed <- downloadHandler(
    filename = function() "unparseable_delivery_months.csv",
    content = function(file) write.csv(unparsed_tbl(), file, row.names = FALSE))

  output$t_data <- renderDT({
    d <- dat() %>%
      filter(index_group == input$ig) %>%
      transmute(Broker = broker, `Trade ID` = trade_id, Date = exec_date,
                Hour = exec_hour, Instrument = instrument, Grade = grade,
                Class = itf, Type = swap_leg_outright, `In idx` = in_index,
                Period = period, `Deliv. month` = delivery_month,
                Price = round(price, 3), `Qty m³` = qty_m3,
                `In cycle` = in_trade_cycle, Component = is_component_in_cycle)
    if (length(sel_grades())) d <- d %>% filter(Grade %in% sel_grades())
    datatable(d, rownames = FALSE, filter = "top",
              options = list(pageLength = 25, scrollX = TRUE, dom = "tip"))
  })
  output$dl_data <- downloadHandler(
    filename = function() paste0("normalized_", input$ig, ".csv"),
    content = function(file) {
      d <- dat() %>% filter(index_group == input$ig)
      if (length(sel_grades())) d <- d %>% filter(grade %in% sel_grades())
      write.csv(d, file, row.names = FALSE)
    })
}

shinyApp(ui, server)
