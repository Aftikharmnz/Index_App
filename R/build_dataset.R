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

build_dataset <- function(ref = NULL, data_dir = DATA_DIR, write_cache = TRUE) {
  if (is.null(ref)) ref <- load_reference()

  parts <- list(
    ICE    = normalize_ice(ref, data_dir),
    Modern = normalize_modern(ref, data_dir),
    Neon   = normalize_neon(ref, data_dir),
    OneX   = normalize_onex(ref, data_dir)
  )

  combined <- enrich_combined(bind_rows(parts))

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
