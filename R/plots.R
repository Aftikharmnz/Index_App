# =============================================================================
# R/plots.R
# Formatting helpers + reusable plotly chart builders for the Shiny app.
# =============================================================================

suppressPackageStartupMessages({ library(plotly); library(scales) })

BROKER_COLORS <- c(ICE = "#1f77b4", Modern = "#ff7f0e", Neon = "#2ca02c", OneX = "#9467bd")
ACCENT <- "#0d6efd"; ACCENT2 <- "#dc3545"; GRID <- "#e9ecef"

fmt_diff <- function(x) ifelse(is.na(x), "—", sprintf("%+.2f", x))
fmt_int  <- function(x) ifelse(is.na(x), "—", comma(round(x)))
fmt_m3   <- function(x) ifelse(is.na(x), "—", paste0(comma(round(x)), " m³"))

.base_layout <- function(p, title = NULL, ylab = "", xlab = "") {
  p %>% layout(
    title = list(text = title, x = 0, font = list(size = 14)),
    margin = list(l = 60, r = 60, t = 40, b = 40),
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
    font = list(family = "Inter, Segoe UI, sans-serif", size = 12),
    xaxis = list(title = xlab, gridcolor = GRID, zeroline = FALSE),
    yaxis = list(title = ylab, gridcolor = GRID, zeroline = TRUE, zerolinecolor = "#adb5bd"),
    legend = list(orientation = "h", x = 0, y = 1.12),
    hovermode = "x unified"
  ) %>% config(displayModeBar = FALSE)
}

empty_plot <- function(msg = "No trades for this selection") {
  plot_ly() %>%
    add_annotations(text = msg, showarrow = FALSE,
                    font = list(size = 14, color = "#868e96"),
                    x = 0.5, y = 0.5, xref = "paper", yref = "paper") %>%
    layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
           xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)) %>%
    config(displayModeBar = FALSE)
}

# Cumulative ("accumulated") index development through the cycle, with daily volume.
plot_cycle_development <- function(daily) {
  if (nrow(daily) == 0) return(empty_plot())
  plot_ly(daily) %>%
    add_bars(x = ~exec_date, y = ~day_vol, name = "Daily volume (m³)",
             marker = list(color = "rgba(13,110,253,0.18)"), yaxis = "y2",
             hovertemplate = "%{x|%b %d}: %{y:,.0f} m³<extra></extra>") %>%
    add_lines(x = ~exec_date, y = ~cum_vol, name = "Accumulated volume (m³)",
              line = list(color = "#20c997", width = 2.5), yaxis = "y2",
              hovertemplate = "Cum vol: %{y:,.0f} m³<extra></extra>") %>%
    add_lines(x = ~exec_date, y = ~cum_vwap, name = "Accumulated index (VWAP)",
              line = list(color = ACCENT, width = 3),
              hovertemplate = "Cum VWAP: %{y:+.3f}<extra></extra>") %>%
    add_markers(x = ~exec_date, y = ~day_vwap, name = "Daily VWAP",
                marker = list(color = ACCENT2, size = 6),
                hovertemplate = "Day VWAP: %{y:+.3f}<extra></extra>") %>%
    layout(
      margin = list(l = 60, r = 60, t = 30, b = 40),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      font = list(family = "Inter, Segoe UI, sans-serif", size = 12),
      xaxis = list(title = "", gridcolor = GRID),
      yaxis = list(title = "Differential to WTI CMA ($/bbl)", gridcolor = GRID,
                   zeroline = TRUE, zerolinecolor = "#adb5bd"),
      yaxis2 = list(title = "Volume (m³)", overlaying = "y", side = "right",
                    showgrid = FALSE, rangemode = "tozero"),
      legend = list(orientation = "h", x = 0, y = 1.14),
      hovermode = "x unified"
    ) %>% config(displayModeBar = FALSE)
}

# ---- Volume seasonality across trade cycles ----
# Curated grade colours. SW grades + SYN grades; any grade not listed here gets a
# stable, distinct colour from grade_color()'s fallback (never grey).
GRADE_COLORS <- c(# SW
                  PCE = "#1f77b4", PEM = "#ff7f0e", MSW = "#2ca02c", CSW = "#d62728",
                  FED = "#9467bd", RBW = "#8c564b", MSE = "#e377c2", MSY = "#17becf",
                  # SYN
                  SYN = "#1f77b4", CNS = "#ff7f0e", SSP = "#2ca02c", HSC = "#d62728",
                  SYN_EIL = "#9467bd")

# Fallback palette (distinct from collisions within a typical group) for grades
# that aren't in GRADE_COLORS — assigned deterministically by name so a grade
# keeps the same colour across every chart and session.
GRADE_FALLBACK_PAL <- c("#8c564b", "#e377c2", "#bcbd22", "#17becf", "#aec7e8",
                        "#ffbb78", "#98df8a", "#ff9896", "#c5b0d5", "#c49c94")

# Stable colour for one or more grades (curated first, else name-hash fallback).
grade_color <- function(g) {
  vapply(as.character(g), function(x) {
    if (is.na(x) || x == "") return("#888888")
    c0 <- GRADE_COLORS[x]
    if (!is.na(c0)) return(unname(c0))
    GRADE_FALLBACK_PAL[(sum(utf8ToInt(x)) %% length(GRADE_FALLBACK_PAL)) + 1L]
  }, character(1), USE.NAMES = FALSE)
}

# Colour for a segment that may be a broker OR a grade (used by stacked charts).
seg_color <- function(s, seg_type) {
  if (identical(seg_type, "broker")) {
    c0 <- BROKER_COLORS[as.character(s)]
    return(unname(ifelse(is.na(c0), "#888888", c0)))
  }
  grade_color(s)
}
SEASON_COLORS <- c(Winter = "#4c78a8", Spring = "#54a24b", Summer = "#e45756", Fall = "#f58518")

season_of <- function(dm) {
  m <- lubridate::month(dm)
  dplyr::case_when(m %in% c(12, 1, 2) ~ "Winter", m %in% 3:5 ~ "Spring",
                   m %in% 6:8 ~ "Summer", TRUE ~ "Fall")
}

# Resolve axis selections -> column names, labels, and hover format.
.season_xy <- function(x, y) {
  xvar <- if (x == "day") "trading_day_rank" else "norm_pct"
  yvar <- switch(y, daily_raw = "day_vol", daily_pct = "day_pct",
                 cum_raw = "cum_vol", cum_pct = "cum_pct")
  ypct <- y %in% c("daily_pct", "cum_pct")
  list(
    xvar = xvar, yvar = yvar,
    xlab = if (x == "day") "Trading day of cycle" else "% through trade cycle",
    ylab = switch(y, daily_raw = "Daily volume (m³)", daily_pct = "Daily % of cycle volume",
                  cum_raw = "Accumulated volume (m³)", cum_pct = "Accumulated % of cycle volume"),
    ht = paste0(if (x == "pct") "%{x:.0f}%" else "day %{x}", ": ",
                if (ypct) "%{y:.1f}%" else "%{y:,.0f} m³"),
    even = y == "cum_pct"
  )
}

# Mean/median reference curve across ALL cycles (interpolated onto a common grid).
.season_avg_curve <- function(season, xvar, yvar, stat = "mean") {
  months <- sort(unique(season$delivery_month))
  if (length(months) < 2) return(NULL)
  grid <- if (xvar == "norm_pct") seq(0, 100, by = 2.5) else
          seq(min(season[[xvar]], na.rm = TRUE), max(season[[xvar]], na.rm = TRUE))
  mat <- vapply(seq_along(months), function(i) {
    d <- season[season$delivery_month == months[i], ]; d <- d[order(d[[xvar]]), ]
    if (nrow(d) < 2) return(rep(NA_real_, length(grid)))
    stats::approx(d[[xvar]], d[[yvar]], xout = grid, rule = 2)$y
  }, numeric(length(grid)))
  if (is.null(dim(mat))) mat <- matrix(mat, nrow = length(grid))
  agg <- if (stat == "median") apply(mat, 1L, function(z) stats::median(z, na.rm = TRUE))
         else rowMeans(mat, na.rm = TRUE)
  data.frame(x = grid, y = agg)
}

plot_seasonality_line <- function(season, x = "day", y = "daily_raw",
                                  color_by = "month", months = NULL,
                                  avg = "none", highlight = NULL) {
  if (nrow(season) == 0) return(empty_plot())
  cfg <- .season_xy(x, y)
  all_months <- sort(unique(season$delivery_month))
  show <- if (is.null(months) || !length(months)) all_months else all_months[all_months %in% as.Date(months)]
  pal <- grDevices::hcl.colors(max(length(all_months), 2), "Spectral")
  names(pal) <- as.character(all_months)
  p <- plot_ly()
  if (cfg$even && x == "pct") p <- p %>% add_lines(x = c(0, 100), y = c(0, 100), name = "Even pace",
                                     line = list(color = "#ced4da", dash = "dash", width = 1),
                                     hoverinfo = "skip")
  for (mi in seq_along(show)) {
    m <- show[mi]
    d <- season[season$delivery_month == m, ]; d <- d[order(d[[cfg$xvar]]), ]
    is_hl <- !is.null(highlight) && !is.na(highlight) && m == highlight
    col <- if (color_by == "season") unname(SEASON_COLORS[season_of(m)]) else unname(pal[as.character(m)])
    if (is_hl) col <- "#0b1f3a"
    p <- p %>% add_lines(
      data = d, x = d[[cfg$xvar]], y = d[[cfg$yvar]], name = format(m, "%b %Y"),
      legendgroup = if (color_by == "season") season_of(m) else format(m, "%b %Y"),
      line = list(color = col, width = if (is_hl) 4 else 1.6), opacity = if (is_hl) 1 else 0.55,
      hovertemplate = paste0(format(m, "%b %Y"), " — ", cfg$ht, "<extra></extra>"))
  }
  if (avg != "none") {
    sub <- season[season$delivery_month %in% show, , drop = FALSE]
    ac <- .season_avg_curve(sub, cfg$xvar, cfg$yvar, avg)
    if (!is.null(ac)) p <- p %>% add_lines(
      x = ac$x, y = ac$y, name = paste0(tools::toTitleCase(avg), " (selected)"),
      line = list(color = "#111", width = 3, dash = "dot"),
      hovertemplate = paste0(tools::toTitleCase(avg), " ", cfg$ht, "<extra></extra>"))
  }
  p %>% .base_layout(xlab = cfg$xlab, ylab = cfg$ylab)
}

plot_seasonality_bar <- function(season_seg, x = "day", y = "daily_raw", months = NULL,
                                 bin = 10, max_months = 8, seg_type = "grade") {
  if (nrow(season_seg) == 0) return(empty_plot())
  cfg <- .season_xy(x, y)
  all_months <- sort(unique(season_seg$delivery_month))
  show <- if (is.null(months) || !length(months)) all_months else all_months[all_months %in% as.Date(months)]
  if (!length(show)) return(empty_plot())
  capped_note <- NULL
  if (length(show) > max_months) {
    capped_note <- sprintf("Showing %d most recent of %d cycles — narrow 'Months to compare' to see specific ones.",
                           max_months, length(show))
    show <- utils::tail(show, max_months)
  }
  d <- season_seg[season_seg$delivery_month %in% show, , drop = FALSE]
  if (x == "day") d$base <- d[[cfg$xvar]] else d$base <- round(d$norm_pct / bin) * bin
  ag <- if (y %in% c("cum_raw", "cum_pct")) function(v) max(v, na.rm = TRUE) else function(v) sum(v, na.rm = TRUE)
  da <- d %>% group_by(delivery_month, base, seg) %>%
    summarise(val = ag(.data[[cfg$yvar]]), .groups = "drop")
  segs <- sort(unique(da$seg))
  pctmode <- y %in% c("daily_pct", "cum_pct")
  show_lab <- format(show, "%b %Y")
  days <- sort(unique(da$base))
  multi <- length(show) > 1
  sep <- " · "

  # One categorical slot per (day, month); a spacer between day-groups gives the
  # grouped look on a clean categorical axis (no fractional x). Day number is
  # ticked at each group's centre.
  lvls <- character(0); day_tickval <- character(0); day_ticktext <- character(0)
  for (di in seq_along(days)) {
    grp <- paste0(days[di], sep, show_lab)
    lvls <- c(lvls, grp)
    day_tickval <- c(day_tickval, grp[ceiling(length(grp) / 2)])
    day_ticktext <- c(day_ticktext, as.character(days[di]))
    if (multi && di < length(days)) lvls <- c(lvls, paste0("__sp", di))
  }
  da$cat <- factor(paste0(da$base, sep, format(da$delivery_month, "%b %Y")), levels = lvls)

  p <- plot_ly()
  for (s in segs) {
    dg <- da[da$seg == s, ]
    p <- p %>% add_bars(
      x = dg$cat, y = dg$val, name = s,
      marker = list(color = seg_color(s, seg_type),
                    line = list(color = "white", width = 1)),
      customdata = format(dg$delivery_month, "%b %Y"),
      hovertemplate = paste0("%{customdata} · ", s, ": ",
                             if (pctmode) "%{y:.1f}%" else "%{y:,.0f} m³", "<extra></extra>"))
  }
  anns <- list()
  if (multi) anns <- c(anns, list(list(
    x = 0, y = 1.02, xref = "paper", yref = "paper",
    text = paste0("Each ", if (x == "day") "day" else "bucket", "'s bars, left→right: ",
                  paste(format(show, "%b'%y"), collapse = ", ")),
    showarrow = FALSE, xanchor = "left", font = list(size = 10, color = "#666"))))
  if (!is.null(capped_note)) anns <- c(anns, list(list(
    x = 0, y = 1.10, xref = "paper", yref = "paper", text = capped_note,
    showarrow = FALSE, xanchor = "left", font = list(size = 10, color = "#b02a37"))))
  p %>% layout(
    barmode = "stack", bargap = 0.32, annotations = anns,
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
    font = list(family = "Inter, Segoe UI, sans-serif", size = 12),
    xaxis = list(title = cfg$xlab, type = "category", categoryorder = "array",
                 categoryarray = lvls, tickvals = day_tickval, ticktext = day_ticktext,
                 gridcolor = GRID),
    yaxis = list(title = cfg$ylab, gridcolor = GRID),
    legend = list(orientation = "h", x = 0, y = 1.16),
    margin = list(l = 60, r = 30, t = 52, b = 40)) %>%
    config(displayModeBar = FALSE)
}

plot_hourly <- function(hourly) {
  if (nrow(hourly) == 0) return(empty_plot())
  hourly$hr <- sprintf("%02d:00", hourly$exec_hour)
  plot_ly(hourly) %>%
    add_bars(x = ~hr, y = ~vol_m3, name = "Volume",
             marker = list(color = ACCENT),
             hovertemplate = "%{x}: %{y:,.0f} m³<extra></extra>") %>%
    add_lines(x = ~hr, y = ~vwap, name = "VWAP", yaxis = "y2",
              line = list(color = ACCENT2, width = 2),
              hovertemplate = "VWAP %{y:+.2f}<extra></extra>") %>%
    layout(
      margin = list(l = 60, r = 60, t = 30, b = 40),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      font = list(family = "Inter, Segoe UI, sans-serif", size = 12),
      xaxis = list(title = "Hour of day (local)", gridcolor = GRID),
      yaxis = list(title = "Volume (m³)", gridcolor = GRID, rangemode = "tozero"),
      yaxis2 = list(title = "VWAP", overlaying = "y", side = "right", showgrid = FALSE),
      legend = list(orientation = "h", x = 0, y = 1.14)
    ) %>% config(displayModeBar = FALSE)
}

# Intraday volume profile: bars by time-of-day bucket (optionally stacked by
# grade/broker), with the total VWAP overlaid. Numeric x (minutes) -> any
# granularity renders cleanly with hour-marked ticks.
plot_intraday <- function(prof) {
  total <- prof$total
  if (nrow(total) == 0) return(empty_plot())
  bw <- prof$bucket_min
  fmt_tod <- function(m) sprintf("%02d:%02d", m %/% 60, m %% 60)
  hr_ticks <- seq(floor(min(total$tod) / 60) * 60,
                  ceiling((max(total$tod) + bw) / 60) * 60, by = 60)
  p <- plot_ly()
  if (nrow(prof$byseg) > 0) {
    for (s in sort(unique(prof$byseg$seg))) {
      dg <- prof$byseg[prof$byseg$seg == s, ]
      p <- p %>% add_bars(x = dg$tod + bw / 2, y = dg$vol_m3, name = s, width = bw * 0.9,
                          marker = list(color = seg_color(s, prof$by),
                                        line = list(color = "white", width = 0.5)),
                          customdata = fmt_tod(dg$tod),
                          hovertemplate = paste0("%{customdata} · ", s, ": %{y:,.0f} m³<extra></extra>"))
    }
  } else {
    p <- p %>% add_bars(x = total$tod + bw / 2, y = total$vol_m3, name = "Volume", width = bw * 0.9,
                        marker = list(color = ACCENT),
                        customdata = fmt_tod(total$tod),
                        hovertemplate = "%{customdata}: %{y:,.0f} m³<extra></extra>")
  }
  p <- p %>% add_lines(x = total$tod + bw / 2, y = total$vwap, name = "VWAP (per bucket)", yaxis = "y2",
                       line = list(color = ACCENT2, width = 1.5),
                       customdata = fmt_tod(total$tod),
                       hovertemplate = "%{customdata} bucket VWAP %{y:+.2f}<extra></extra>") %>%
    add_lines(x = total$tod + bw / 2, y = total$cum_vwap, name = "Cumulative VWAP", yaxis = "y2",
              line = list(color = ACCENT, width = 3),
              customdata = fmt_tod(total$tod),
              hovertemplate = "%{customdata} cum VWAP %{y:+.3f}<extra></extra>")
  p %>% layout(
    barmode = "stack",
    margin = list(l = 60, r = 60, t = 30, b = 40),
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
    font = list(family = "Inter, Segoe UI, sans-serif", size = 12),
    xaxis = list(title = "Time of day (local)", gridcolor = GRID,
                 tickvals = hr_ticks, ticktext = fmt_tod(hr_ticks)),
    yaxis = list(title = "Volume (m³)", gridcolor = GRID, rangemode = "tozero"),
    yaxis2 = list(title = "VWAP", overlaying = "y", side = "right", showgrid = FALSE),
    legend = list(orientation = "h", x = 0, y = 1.14)
  ) %>% config(displayModeBar = FALSE)
}

# Candlestick + volume. One product -> OHLC candles; several -> close-price lines.
# Price panel on top (y), volume panel on bottom (y2) via axis domains; x range slider.
plot_candles <- function(ohlc) {
  if (nrow(ohlc) == 0) return(empty_plot("No trades for this selection"))
  grades <- sort(unique(ohlc$grade))
  single <- length(grades) == 1
  p <- plot_ly()
  if (single) {
    d <- ohlc[order(ohlc$bkt), ]
    p <- p %>%
      add_trace(data = d, x = ~bkt, type = "candlestick",
                open = ~open, high = ~high, low = ~low, close = ~close, name = grades[1],
                increasing = list(line = list(color = "#26a69a")),
                decreasing = list(line = list(color = "#ef5350"))) %>%
      add_bars(data = d, x = ~bkt, y = ~volume, yaxis = "y2", name = "Volume",
               marker = list(color = "rgba(13,110,253,0.45)"),
               hovertemplate = "%{x}<br>%{y:,.0f} m³<extra></extra>")
  } else {
    for (g in grades) {
      dg <- ohlc[ohlc$grade == g, ]; dg <- dg[order(dg$bkt), ]
      col <- grade_color(g)
      p <- p %>%
        add_lines(data = dg, x = ~bkt, y = ~close, name = g,
                  line = list(color = col, width = 2),
                  hovertemplate = paste0(g, " close %{y:+.2f}<extra></extra>")) %>%
        add_bars(data = dg, x = ~bkt, y = ~volume, yaxis = "y2", name = g, showlegend = FALSE,
                 marker = list(color = col),
                 hovertemplate = paste0(g, " %{y:,.0f} m³<extra></extra>"))
    }
  }
  p %>% layout(
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
    font = list(family = "Inter, Segoe UI, sans-serif", size = 12),
    barmode = "stack",
    yaxis = list(title = if (single) "Differential ($/bbl)" else "Close diff ($/bbl)",
                 domain = c(0.24, 1), gridcolor = GRID, zeroline = TRUE, zerolinecolor = "#adb5bd"),
    yaxis2 = list(title = "Vol m³", domain = c(0, 0.18), gridcolor = GRID, anchor = "x", rangemode = "tozero"),
    xaxis = list(rangeslider = list(visible = TRUE, thickness = 0.07), gridcolor = GRID),
    legend = list(orientation = "h", x = 0, y = 1.04),
    margin = list(l = 60, r = 20, t = 28, b = 20)
  ) %>% config(displayModeBar = TRUE, displaylogo = FALSE,
               modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"))
}

plot_contribution <- function(df, key, title) {
  if (nrow(df) == 0) return(empty_plot())
  df <- df[order(df$vol_m3), ]
  cols <- if (key == "broker") unname(BROKER_COLORS[df[[key]]]) else ACCENT
  plot_ly(df, x = ~vol_m3, y = stats::reorder(df[[key]], df$vol_m3), type = "bar",
          orientation = "h", marker = list(color = cols),
          text = ~paste0(percent(share, accuracy = 0.1), "  ·  ", fmt_diff(vwap)),
          textposition = "auto",
          hovertemplate = paste0("%{y}: %{x:,.0f} m³<extra></extra>")) %>%
    .base_layout(title = title, xlab = "Volume (m³)")
}

plot_monthly_index <- function(mi_comm, mi_1a, grade_label = "") {
  if (nrow(mi_comm) == 0) return(empty_plot())
  p <- plot_ly() %>%
    add_lines(data = mi_comm, x = ~delivery_month, y = ~vwap, name = "comm (4 brokers)",
              line = list(color = ACCENT, width = 3),
              hovertemplate = "%{x|%b %Y}: %{y:+.2f}<extra>comm</extra>")
  if (!is.null(mi_1a) && nrow(mi_1a) > 0) {
    p <- p %>% add_lines(data = mi_1a, x = ~delivery_month, y = ~vwap, name = "1a (ICE+OneX)",
                         line = list(color = ACCENT2, width = 2, dash = "dot"),
                         hovertemplate = "%{x|%b %Y}: %{y:+.2f}<extra>1a</extra>")
  }
  p %>% .base_layout(title = paste0("Monthly index ", grade_label),
                     ylab = "Differential to WTI CMA ($/bbl)")
}

# comm + 1a + every individual broker on one chart.
plot_monthly_index_multi <- function(comm, a1, bmv, grade_label = "") {
  if (nrow(comm) == 0) return(empty_plot())
  p <- plot_ly()
  for (b in sort(unique(bmv$broker))) {
    d <- bmv[bmv$broker == b, ]
    p <- p %>% add_lines(data = d, x = ~delivery_month, y = ~vwap, name = b,
                         line = list(color = unname(BROKER_COLORS[b]), width = 1.5),
                         hovertemplate = paste0(b, " %{x|%b %Y}: %{y:+.2f}<extra></extra>"))
  }
  p <- p %>% add_lines(data = comm, x = ~delivery_month, y = ~vwap, name = "comm (4 brokers)",
                       line = list(color = "#0b1f3a", width = 3.5),
                       hovertemplate = "comm %{x|%b %Y}: %{y:+.2f}<extra></extra>")
  if (!is.null(a1) && nrow(a1) > 0) {
    p <- p %>% add_lines(data = a1, x = ~delivery_month, y = ~vwap, name = "1a (ICE+OneX)",
                         line = list(color = ACCENT2, width = 2.5, dash = "dot"),
                         hovertemplate = "1a %{x|%b %Y}: %{y:+.2f}<extra></extra>")
  }
  p %>% .base_layout(title = paste0("Monthly index ", grade_label),
                     ylab = "Differential to WTI CMA ($/bbl)")
}

plot_broker_dispersion <- function(bmv) {
  if (nrow(bmv) == 0) return(empty_plot())
  p <- plot_ly()
  for (b in sort(unique(bmv$broker))) {
    d <- bmv[bmv$broker == b, ]
    p <- p %>% add_lines(data = d, x = ~delivery_month, y = ~vwap, name = b,
                         line = list(color = unname(BROKER_COLORS[b]), width = 2),
                         hovertemplate = paste0(b, " %{x|%b %Y}: %{y:+.2f}<extra></extra>"))
  }
  p %>% .base_layout(title = "Per-broker monthly VWAP", ylab = "Differential ($/bbl)")
}

TYPE_COLORS <- c(comm = "#0d6efd", `1a` = "#dc3545", Bi4 = "#fd7e14",
                 XAPP = "#6f42c1", other = "#adb5bd")

plot_index_ref_monthly <- function(df) {
  if (nrow(df) == 0) return(empty_plot("No index-referenced trades for this selection"))
  p <- plot_ly()
  for (ty in names(TYPE_COLORS)) {
    d <- df[df$index_type == ty, , drop = FALSE]
    if (nrow(d) == 0) next
    p <- p %>% add_bars(data = d, x = ~delivery_month, y = ~vol_m3, name = ty,
                        marker = list(color = unname(TYPE_COLORS[ty])),
                        hovertemplate = paste0(ty, " %{x|%b %Y}: %{y:,.0f} m³<extra></extra>"))
  }
  p %>% .base_layout(ylab = "At-index volume (m³)") %>% layout(barmode = "stack")
}

plot_index_ref_bars <- function(df, key, title) {
  if (nrow(df) == 0) return(empty_plot())
  df <- df[order(df$vol_m3), ]
  cols <- if (key == "broker") unname(BROKER_COLORS[df[[key]]]) else ACCENT
  plot_ly(df, x = ~vol_m3, y = stats::reorder(df[[key]], df$vol_m3), type = "bar",
          orientation = "h", marker = list(color = cols),
          text = ~paste0(comma(round(vol_m3)), " m³ · ", trades, " trd"), textposition = "auto",
          hovertemplate = "%{y}: %{x:,.0f} m³<extra></extra>") %>%
    .base_layout(title = title, xlab = "At-index volume (m³)")
}

CYCLE_LEVELS <- c("Before cycle", "In cycle", "After cycle", "No TC match")
CYCLE_COLORS <- c("Before cycle" = "#74a9cf", "In cycle" = "#0d6efd",
                  "After cycle" = "#fd7e14", "No TC match" = "#adb5bd")

plot_cycle_compare <- function(hc, metric = c("vol_m3", "vwap", "avg_price")) {
  metric <- match.arg(metric)
  if (nrow(hc) == 0) return(empty_plot())
  ylab <- switch(metric, vol_m3 = "Volume (m³)",
                 vwap = "VWAP (diff $/bbl)", avg_price = "Avg price (diff $/bbl)")
  ht <- if (metric == "vol_m3") "%{y:,.0f} m³" else "%{y:+.2f}"
  p <- plot_ly()
  for (lv in CYCLE_LEVELS) {
    d <- hc[hc$cycle == lv, , drop = FALSE]
    if (nrow(d) == 0) next
    p <- p %>% add_bars(data = d, x = ~delivery_month, y = d[[metric]], name = lv,
                        marker = list(color = unname(CYCLE_COLORS[lv])),
                        hovertemplate = paste0(lv, " %{x|%b %Y}: ", ht, "<extra></extra>"))
  }
  p %>% .base_layout(ylab = ylab) %>% layout(barmode = "group")
}
