# =============================================================================
# R/build_dataset.R
# Bind all broker normalizers into one combined table, enrich, and cache.
# =============================================================================

# Flags that mark a row as a physical "component" trade eligible for index VWAP.
enrich_combined <- function(combined) {
  combined %>%
    mutate(
      index_group = ukey(index_group),
      grade = blank_to_na(grade),
      exec_dow = lubridate::wday(exec_date, label = TRUE, abbr = TRUE, week_start = 1),
      qty_bbl = qty_m3 * BBL_PER_M3,
      # Physical component trade eligible for the index VWAP. Uses %in% so the
      # flag is strictly TRUE/FALSE (never NA), and requires a classified grade
      # (KeyID) — matching the original validated component-candidate rule.
      is_component =
        (in_index %in% TRUE) &
        (itf %in% "Trade") &
        (swap_leg_outright %in% "Outright") &
        !is.na(grade) &
        !is.na(qty_m3) & qty_m3 > 0 &
        !is.na(price),
      is_component_in_cycle = is_component & (in_trade_cycle %in% TRUE),
      # Reported / index-referenced rows: trades executed AT a published index.
      is_index_ref = itf %in% "Index",
      index_type = if_else(is_index_ref, derive_index_type(instrument, price_basis), NA_character_)
    )
}

build_qa <- function(combined) {
  coverage <- combined %>%
    group_by(broker) %>%
    summarise(
      oldest = suppressWarnings(min(exec_date, na.rm = TRUE)),
      latest = suppressWarnings(max(exec_date, na.rm = TRUE)),
      rows = n(),
      sw_rows = sum(index_group == "SW", na.rm = TRUE),
      component_rows = sum(is_component, na.rm = TRUE),
      component_in_cycle = sum(is_component_in_cycle, na.rm = TRUE),
      unmapped = sum(is.na(index_group)),
      .groups = "drop"
    ) %>%
    mutate(oldest = dplyr::if_else(is.finite(oldest), oldest, as.Date(NA)),
           latest = dplyr::if_else(is.finite(latest), latest, as.Date(NA))) %>%
    select(broker, oldest, latest, dplyr::everything())

  unmapped <- combined %>%
    filter(is.na(index_group)) %>%
    count(broker, instrument, price_basis, name = "rows") %>%
    arrange(desc(rows))

  list(
    coverage = coverage,
    unmapped = unmapped,
    built_at = Sys.time(),
    n_rows = nrow(combined)
  )
}

# Fill trade-cycle windows for delivery months that appear in the trades but are
# missing from the workbook's TC_Dates (e.g. 2024 history). Only fills rows whose
# window is currently NA — any official TC_Dates row always wins.
#   start = 1st of the prior month (X-1) — the rule TC_Dates follows 100% of the time
#   end   = the prior month's median official close day (~15th), matching the real
#           mid-month pattern (official ends cluster on the 12th-17th)
# Only months that actually traded inside the estimated window are filled, so
# forward delivery months whose cycle hasn't opened yet are left alone.
backfill_trade_cycle <- function(df, tc_dates) {
  end_dom <- suppressWarnings(stats::median(as.integer(format(tc_dates$trade_cycle_end, "%d")), na.rm = TRUE))
  end_dom <- if (is.finite(end_dom)) as.integer(round(end_dom)) else 15L

  need <- df %>%
    dplyr::filter(is.na(trade_cycle_start) | is.na(trade_cycle_end), !is.na(delivery_month)) %>%
    dplyr::distinct(delivery_month) %>% dplyr::pull(delivery_month)
  need <- sort(need[!(need %in% tc_dates$delivery_month)])
  if (length(need) == 0) return(df)

  keep <- logical(length(need)); starts <- rep(as.Date(NA), length(need)); ends <- starts
  for (i in seq_along(need)) {            # index iteration: keeps Date class
    dm <- need[i]
    st <- seq(dm, by = "-1 month", length.out = 2)[2]   # 1st of month X-1
    en <- st + (end_dom - 1L)                            # ~15th of month X-1
    starts[i] <- st; ends[i] <- en
    keep[i] <- any(df$delivery_month == dm & df$exec_date >= st & df$exec_date <= en, na.rm = TRUE)
  }
  need <- need[keep]; starts <- starts[keep]; ends <- ends[keep]
  if (length(need) == 0) return(df)
  est <- tibble::tibble(delivery_month = need, est_start = starts, est_end = ends)
  message(sprintf("backfill_trade_cycle: estimated %d cycle window(s) (close day ~%d): %s",
                  nrow(est), end_dom, paste(format(need), collapse = ", ")))

  df %>%
    dplyr::left_join(est, by = "delivery_month") %>%
    dplyr::mutate(
      trade_cycle_start = dplyr::coalesce(trade_cycle_start, est_start),
      trade_cycle_end   = dplyr::coalesce(trade_cycle_end,   est_end),
      in_trade_cycle = dplyr::case_when(
        is.na(exec_date) | is.na(trade_cycle_start) | is.na(trade_cycle_end) ~ NA,
        exec_date >= trade_cycle_start & exec_date <= trade_cycle_end ~ TRUE,
        TRUE ~ FALSE)
    ) %>%
    dplyr::select(-est_start, -est_end)
}

build_dataset <- function(ref = NULL, data_dir = DATA_DIR, write_cache = TRUE) {
  if (is.null(ref)) ref <- load_reference()

  parts <- list(
    ICE    = normalize_ice(ref, data_dir),
    Modern = normalize_modern(ref, data_dir),
    Neon   = normalize_neon(ref, data_dir),
    OneX   = normalize_onex(ref, data_dir)
  )

  combined <- bind_rows(parts)
  combined <- backfill_trade_cycle(combined, ref$tc_dates)   # cover 2024 / any gaps
  combined <- enrich_combined(combined)

  if (write_cache) {
    dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
    arrow::write_parquet(combined, CACHE_FILE)
    saveRDS(build_qa(combined), QA_FILE)
  }
  combined
}

# Load combined data from cache, building it if necessary.
load_combined <- function(rebuild = FALSE) {
  if (!rebuild && file.exists(CACHE_FILE)) {
    return(arrow::read_parquet(CACHE_FILE))
  }
  build_dataset()
}
