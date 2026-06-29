# =============================================================================
# R/indices.R
# Index / VWAP analytics on the combined normalized table.
# An "index" for a grade (KeyID) and delivery month = volume-weighted average
# price of the physical component trades inside that month's trade cycle.
#   comm = all four brokers ; 1a = ICE + OneX (see INDEX_DEFINITIONS).
# =============================================================================

def_brokers <- function(definition) {
  if (is.null(definition) || length(definition) != 1) return(BROKERS)
  if (definition %in% BROKERS) return(definition)   # a single broker is its own index
  b <- INDEX_DEFINITIONS[[definition]]
  if (is.null(b)) BROKERS else b
}

# Core filter: physical component trades for a universe.
component_trades <- function(combined, ig = "SW", grades = NULL,
                             brokers = NULL, in_cycle = TRUE) {
  d <- dplyr::filter(combined, index_group == ig)
  d <- if (in_cycle) dplyr::filter(d, is_component_in_cycle) else dplyr::filter(d, is_component)
  if (!is.null(grades))  d <- dplyr::filter(d, grade %in% grades)
  if (!is.null(brokers)) d <- dplyr::filter(d, broker %in% brokers)
  d
}

vwap <- function(price, qty) {
  s <- sum(qty, na.rm = TRUE)
  if (s <= 0) return(NA_real_)
  sum(price * qty, na.rm = TRUE) / s
}

# ---- QA: physical trades whose delivery month couldn't be parsed ----
# These are real "Trade" rows with a grade + a non-empty period that didn't
# resolve to a MON-YY delivery month (e.g. quarterly strips "Q3 25"), so they
# are silently excluded from every index. Surfaced so they can be fixed at source.
unparsed_month_trades <- function(combined) {
  combined %>%
    dplyr::filter(itf %in% "Trade", !is.na(grade),
                  is.na(delivery_month),
                  !is.na(period), trimws(period) != "") %>%
    dplyr::transmute(broker, trade_id, exec_date, index_group, grade,
                     instrument, period, price, qty_m3, source_file) %>%
    dplyr::arrange(broker, period, grade)
}

# ---- Available dimensions (for UI selectors) ----
available_grades <- function(combined, ig = "SW") {
  combined %>%
    filter(index_group == ig, !is.na(grade)) %>%
    distinct(grade) %>% arrange(grade) %>% pull(grade)
}

available_months <- function(combined, ig = "SW") {
  component_trades(combined, ig, in_cycle = TRUE) %>%
    distinct(delivery_month) %>% filter(!is.na(delivery_month)) %>%
    arrange(delivery_month) %>% pull(delivery_month)
}

# ---- Monthly index: VWAP per delivery month (optionally per grade) ----
monthly_index <- function(combined, ig = "SW", definition = "comm",
                          grades = NULL, by_grade = TRUE) {
  d <- component_trades(combined, ig, grades, def_brokers(definition), in_cycle = TRUE)
  if (nrow(d) == 0) return(tibble())
  grp <- if (by_grade) c("delivery_month", "grade") else "delivery_month"
  d %>%
    group_by(across(all_of(grp))) %>%
    summarise(
      vwap = vwap(price, qty_m3),
      vol_m3 = sum(qty_m3, na.rm = TRUE),
      vol_bbl = sum(qty_m3, na.rm = TRUE) * BBL_PER_M3,
      trades = n(),
      n_brokers = n_distinct(broker),
      brokers = paste(sort(unique(broker)), collapse = ", "),
      first_trade = min(exec_date, na.rm = TRUE),
      last_trade = max(exec_date, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(definition = definition) %>%
    arrange(delivery_month)
}

# ---- Daily development within a delivery month's trade cycle ----
# Returns per-day volume, daily VWAP, and the cumulative ("accumulated") index.
cycle_daily <- function(combined, dm, ig = "SW", definition = "comm", grades = NULL) {
  d <- component_trades(combined, ig, grades, def_brokers(definition), in_cycle = TRUE) %>%
    filter(delivery_month == dm)
  if (nrow(d) == 0) return(tibble())
  d %>%
    group_by(exec_date) %>%
    summarise(
      day_vol = sum(qty_m3, na.rm = TRUE),
      day_num = sum(price * qty_m3, na.rm = TRUE),
      day_vwap = day_num / day_vol,
      trades = n(),
      n_brokers = n_distinct(broker),
      .groups = "drop"
    ) %>%
    arrange(exec_date) %>%
    mutate(
      cum_vol = cumsum(day_vol),
      cum_num = cumsum(day_num),
      cum_vwap = cum_num / cum_vol,
      cum_trades = cumsum(trades)
    )
}

# ---- Volume seasonality across trade cycles (one series per delivery month) ----
# Per delivery month, daily volume positioned by: cycle day, % through cycle
# (normalized by number of trading days), and cumulative % of that cycle's volume.
cycle_seasonality <- function(combined, ig = "SW", definition = "comm", grades = NULL,
                              by = NULL) {
  d <- component_trades(combined, ig, grades, def_brokers(definition), in_cycle = TRUE)
  if (nrow(d) == 0) return(tibble())

  # Per-day cycle position (shared by both line and grade views)
  pos <- d %>%
    group_by(delivery_month, exec_date) %>%
    summarise(cyc_start = min(trade_cycle_start, na.rm = TRUE), .groups = "drop") %>%
    arrange(delivery_month, exec_date) %>%
    group_by(delivery_month) %>%
    mutate(cycle_day = as.integer(exec_date - cyc_start) + 1L,
           trading_day_rank = row_number(),
           n_trading_days = n(),
           norm_pct = if_else(n_trading_days > 1,
                              (trading_day_rank - 1) / (n_trading_days - 1) * 100, 0)) %>%
    ungroup() %>%
    select(delivery_month, exec_date, cycle_day, norm_pct, trading_day_rank, n_trading_days)

  totals <- d %>% group_by(delivery_month) %>%
    summarise(total_vol = sum(qty_m3, na.rm = TRUE), .groups = "drop")

  if (!is.null(by)) {
    vol <- d %>%
      mutate(seg = .data[[by]]) %>%
      filter(!is.na(seg)) %>%
      group_by(delivery_month, exec_date, seg) %>%
      summarise(day_vol = sum(qty_m3, na.rm = TRUE), .groups = "drop") %>%
      group_by(delivery_month) %>%
      tidyr::complete(exec_date, seg, fill = list(day_vol = 0)) %>%
      ungroup()
    out <- vol %>%
      left_join(pos, by = c("delivery_month", "exec_date")) %>%
      left_join(totals, by = "delivery_month") %>%
      filter(!is.na(cycle_day)) %>%
      group_by(delivery_month, seg) %>% arrange(exec_date, .by_group = TRUE) %>%
      mutate(cum_vol = cumsum(day_vol)) %>% ungroup()
  } else {
    vol <- d %>%
      group_by(delivery_month, exec_date) %>%
      summarise(day_vol = sum(qty_m3, na.rm = TRUE), .groups = "drop")
    out <- vol %>%
      left_join(pos, by = c("delivery_month", "exec_date")) %>%
      left_join(totals, by = "delivery_month") %>%
      group_by(delivery_month) %>% arrange(exec_date, .by_group = TRUE) %>%
      mutate(cum_vol = cumsum(day_vol)) %>% ungroup()
  }
  out %>% mutate(day_pct = day_vol / total_vol * 100,
                 cum_pct = cum_vol / total_vol * 100)
}

# ---- Hour-of-day volume / price profile ----
hourly_profile <- function(combined, ig = "SW", definition = "comm",
                           grades = NULL, dms = NULL, in_cycle = TRUE) {
  d <- component_trades(combined, ig, grades, def_brokers(definition), in_cycle = in_cycle)
  if (!is.null(dms)) d <- filter(d, delivery_month %in% dms)
  d <- filter(d, !is.na(exec_hour))
  if (nrow(d) == 0) return(tibble(exec_hour = integer(), vol_m3 = numeric(),
                                  trades = integer(), avg_price = numeric(), vwap = numeric()))
  d %>%
    group_by(exec_hour) %>%
    summarise(
      vol_m3 = sum(qty_m3, na.rm = TRUE),
      trades = n(),
      avg_price = mean(price, na.rm = TRUE),
      vwap = vwap(price, qty_m3),
      .groups = "drop"
    ) %>%
    arrange(exec_hour)
}

# ---- Intraday (time-of-day) volume profile at a chosen granularity ----
# bucket_min = minute bucket size (1/5/15/30/60). by = NULL | "grade" | "broker".
# Returns $total (per bucket: vol/trades/vwap) and $byseg (per bucket × segment) for stacking.
intraday_profile <- function(combined, ig = "SW", definition = "comm", grades = NULL,
                             dms = NULL, dows = NULL, in_cycle = TRUE, bucket_min = 60, by = NULL) {
  d <- component_trades(combined, ig, grades, def_brokers(definition), in_cycle = in_cycle)
  if (!is.null(dms)) d <- dplyr::filter(d, delivery_month %in% dms)
  d <- dplyr::filter(d, !is.na(exec_hour), !is.na(exec_datetime))
  # dows: vector of weekday numbers (1 = Mon … 7 = Sun) to keep
  if (!is.null(dows)) d <- dplyr::filter(d, lubridate::wday(exec_date, week_start = 1) %in% dows)
  if (nrow(d) == 0) return(list(total = tibble(), byseg = tibble(), bucket_min = bucket_min, by = by))
  d <- d %>% mutate(
    tod = floor((lubridate::hour(exec_datetime) * 60 + lubridate::minute(exec_datetime)) / bucket_min) * bucket_min
  )
  total <- d %>% group_by(tod) %>%
    summarise(vol_m3 = sum(qty_m3, na.rm = TRUE),
              num = sum(price * qty_m3, na.rm = TRUE), trades = n(),
              vwap = vwap(price, qty_m3), avg_price = mean(price, na.rm = TRUE), .groups = "drop") %>%
    arrange(tod) %>%
    # running VWAP through the session; last point = full end-of-session VWAP
    mutate(cum_vwap = cumsum(num) / cumsum(vol_m3))
  byseg <- if (!is.null(by)) {
    d %>% mutate(seg = .data[[by]]) %>% filter(!is.na(seg)) %>%
      group_by(tod, seg) %>%
      summarise(vol_m3 = sum(qty_m3, na.rm = TRUE), trades = n(), .groups = "drop")
  } else tibble()
  list(total = total, byseg = byseg, bucket_min = bucket_min, by = by)
}

# ---- OHLC candles + volume per product over time ----
# bucket = "day" or a minute size ("60","30","15","5","1"). grade = the "product".
# Open/Close use first/last trade by exec time within each bucket.
ohlc_profile <- function(combined, ig = "SW", definition = "comm", grades = NULL,
                         dm = NULL, bucket = "day", in_cycle = TRUE) {
  d <- component_trades(combined, ig, grades, def_brokers(definition), in_cycle = in_cycle)
  if (!is.null(dm)) d <- dplyr::filter(d, delivery_month %in% dm)
  d <- dplyr::filter(d, !is.na(exec_datetime), !is.na(price), !is.na(grade))
  if (nrow(d) == 0) return(tibble())
  if (identical(bucket, "day")) {
    d$bkt <- lubridate::floor_date(d$exec_datetime, "day")
  } else {
    bs <- as.numeric(bucket) * 60
    d$bkt <- as.POSIXct(floor(as.numeric(d$exec_datetime) / bs) * bs,
                        origin = "1970-01-01", tz = LOCAL_TZ)
  }
  d %>% arrange(exec_datetime) %>%
    group_by(grade, bkt) %>%
    summarise(open = dplyr::first(price), high = max(price, na.rm = TRUE),
              low = min(price, na.rm = TRUE), close = dplyr::last(price),
              volume = sum(qty_m3, na.rm = TRUE), trades = n(),
              vwap = vwap(price, qty_m3), .groups = "drop") %>%
    arrange(grade, bkt)
}

# ---- Broker contribution to a month's index ----
broker_contribution <- function(combined, dm, ig = "SW", definition = "comm", grades = NULL) {
  d <- component_trades(combined, ig, grades, def_brokers(definition), in_cycle = TRUE) %>%
    filter(delivery_month == dm)
  if (nrow(d) == 0) return(tibble())
  tot <- sum(d$qty_m3, na.rm = TRUE)
  d %>%
    group_by(broker) %>%
    summarise(vol_m3 = sum(qty_m3, na.rm = TRUE), trades = n(),
              vwap = vwap(price, qty_m3), .groups = "drop") %>%
    mutate(share = vol_m3 / tot) %>%
    arrange(desc(vol_m3))
}

# ---- Grade contribution to a month's index ----
grade_contribution <- function(combined, dm, ig = "SW", definition = "comm", grades = NULL) {
  d <- component_trades(combined, ig, grades, def_brokers(definition), in_cycle = TRUE) %>%
    filter(delivery_month == dm)
  if (nrow(d) == 0) return(tibble())
  tot <- sum(d$qty_m3, na.rm = TRUE)
  d %>%
    group_by(grade) %>%
    summarise(vol_m3 = sum(qty_m3, na.rm = TRUE), trades = n(),
              vwap = vwap(price, qty_m3), n_brokers = n_distinct(broker), .groups = "drop") %>%
    mutate(share = vol_m3 / tot) %>%
    arrange(desc(vol_m3))
}

# ---- Per-broker monthly VWAP (for broker dispersion vs combined) ----
broker_monthly_vwap <- function(combined, ig = "SW", grades = NULL) {
  d <- component_trades(combined, ig, grades, NULL, in_cycle = TRUE)
  if (nrow(d) == 0) return(tibble())
  d %>%
    group_by(delivery_month, broker) %>%
    summarise(vwap = vwap(price, qty_m3), vol_m3 = sum(qty_m3, na.rm = TRUE),
              trades = n(), .groups = "drop") %>%
    arrange(delivery_month, broker)
}

# ---- Wide monthly comparison: comm, 1a, and each individual broker VWAP ----
monthly_compare <- function(combined, ig = "SW", grades = NULL) {
  comm <- monthly_index(combined, ig, "comm", grades, by_grade = FALSE)
  if (nrow(comm) == 0) return(tibble())
  comm <- comm %>% transmute(delivery_month, comm = vwap, vol_m3, trades, n_brokers)
  a1 <- monthly_index(combined, ig, "1a", grades, by_grade = FALSE) %>%
    transmute(delivery_month, `1a` = vwap)
  bmv <- broker_monthly_vwap(combined, ig, grades) %>%
    select(delivery_month, broker, vwap) %>%
    tidyr::pivot_wider(names_from = broker, values_from = vwap)
  comm %>%
    left_join(a1, by = "delivery_month") %>%
    left_join(bmv, by = "delivery_month") %>%
    arrange(delivery_month)
}

# ---- Wide monthly table: VWAP + volume + trades for comm, 1a, and each broker ----
monthly_compare_full <- function(combined, ig = "SW", grades = NULL) {
  defs <- c("comm", "1a", "ICE", "Modern", "Neon", "OneX")
  out <- NULL
  for (s in defs) {
    mi <- monthly_index(combined, ig, s, grades, by_grade = FALSE)
    if (nrow(mi) == 0) next
    col <- tibble(delivery_month = mi$delivery_month)
    col[[paste0(s, "_vwap")]]   <- mi$vwap
    col[[paste0(s, "_vol_m3")]] <- mi$vol_m3
    col[[paste0(s, "_trades")]] <- mi$trades
    out <- if (is.null(out)) col else dplyr::full_join(out, col, by = "delivery_month")
  }
  if (is.null(out)) return(tibble())
  dplyr::arrange(out, delivery_month)
}

# ---- Historic in-cycle vs out-of-cycle comparison ----
historic_cycle_compare <- function(combined, ig = "SW", grades = NULL, brokers = NULL) {
  d <- filter(combined, index_group == ig, is_component)
  if (!is.null(grades))  d <- filter(d, grade %in% grades)
  if (!is.null(brokers)) d <- filter(d, broker %in% brokers)
  if (nrow(d) == 0) return(tibble())
  d %>%
    mutate(cycle = case_when(
      in_trade_cycle %in% TRUE ~ "In cycle",
      is.na(exec_date) | is.na(trade_cycle_start) | is.na(trade_cycle_end) ~ "No TC match",
      exec_date < trade_cycle_start ~ "Before cycle",
      exec_date > trade_cycle_end ~ "After cycle",
      TRUE ~ "No TC match"
    )) %>%
    group_by(delivery_month, cycle) %>%
    summarise(
      vol_m3 = sum(qty_m3, na.rm = TRUE),
      avg_price = mean(price, na.rm = TRUE),
      vwap = vwap(price, qty_m3),
      trades = n(),
      .groups = "drop"
    ) %>%
    arrange(delivery_month, cycle)
}

# ---- Index-referenced (reported 1a / comm / Bi4 / XAPP) trade activity ----
# These are trades executed AT a published index (price differential ~0), so the
# meaningful quantity is VOLUME / participation, not a price series.
index_ref_trades <- function(combined, ig = "SW", grades = NULL, types = NULL, dm = NULL) {
  d <- dplyr::filter(combined, index_group == ig, is_index_ref %in% TRUE)
  if (!is.null(grades)) d <- dplyr::filter(d, grade %in% grades)
  if (!is.null(types))  d <- dplyr::filter(d, index_type %in% types)
  if (!is.null(dm))     d <- dplyr::filter(d, delivery_month == dm)
  d
}

index_ref_monthly <- function(combined, ig = "SW", grades = NULL) {
  d <- index_ref_trades(combined, ig, grades)
  if (nrow(d) == 0) return(tibble())
  d %>%
    group_by(delivery_month, index_type) %>%
    summarise(vol_m3 = sum(qty_m3, na.rm = TRUE), trades = n(), .groups = "drop") %>%
    arrange(delivery_month)
}

index_ref_by <- function(combined, ig = "SW", grades = NULL, dm = NULL, by = "broker") {
  d <- index_ref_trades(combined, ig, grades, dm = dm)
  if (nrow(d) == 0) return(tibble())
  d %>%
    group_by(.group = .data[[by]]) %>%
    summarise(vol_m3 = sum(qty_m3, na.rm = TRUE), trades = n(),
              types = paste(sort(unique(index_type)), collapse = ", "), .groups = "drop") %>%
    rename(!!by := .group) %>%
    arrange(desc(vol_m3))
}

index_ref_kpis <- function(combined, ig = "SW", grades = NULL, dm = NULL) {
  d <- index_ref_trades(combined, ig, grades, dm = dm)
  list(
    vol = sum(d$qty_m3, na.rm = TRUE),
    comm = sum(d$qty_m3[d$index_type == "comm"], na.rm = TRUE),
    a1 = sum(d$qty_m3[d$index_type == "1a"], na.rm = TRUE),
    trades = nrow(d),
    in_cycle = sum(d$qty_m3[d$in_trade_cycle %in% TRUE], na.rm = TRUE),
    n_brokers = dplyr::n_distinct(d$broker)
  )
}

index_ref_months <- function(combined, ig = "SW") {
  index_ref_trades(combined, ig) %>% distinct(delivery_month) %>%
    filter(!is.na(delivery_month)) %>% arrange(delivery_month) %>% pull(delivery_month)
}

# ---- Summary stats for KPI boxes ----
month_kpis <- function(combined, dm, ig = "SW", definition = "comm", grades = NULL) {
  d <- component_trades(combined, ig, grades, def_brokers(definition), in_cycle = TRUE) %>%
    filter(delivery_month == dm)
  if (nrow(d) == 0) {
    return(list(index = NA_real_, vol_m3 = 0, vol_bbl = 0, trades = 0,
                n_brokers = 0, n_grades = 0, first = NA, last = NA))
  }
  list(
    index = vwap(d$price, d$qty_m3),
    vol_m3 = sum(d$qty_m3, na.rm = TRUE),
    vol_bbl = sum(d$qty_m3, na.rm = TRUE) * BBL_PER_M3,
    trades = nrow(d),
    n_brokers = dplyr::n_distinct(d$broker),
    n_grades = dplyr::n_distinct(d$grade),
    first = min(d$exec_date, na.rm = TRUE),
    last = max(d$exec_date, na.rm = TRUE)
  )
}
