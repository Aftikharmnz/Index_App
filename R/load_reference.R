# =============================================================================
# R/load_reference.R
# Reads the classification workbook: trade-cycle calendar + per-broker rules.
# Depends on config.R + helpers.R already being sourced.
# =============================================================================

# ---- Trade cycle calendar (delivery_month -> cycle window) ----
load_tc_dates <- function(workbook = WORKBOOK) {
  read_excel(workbook, sheet = "TC_Dates") %>%
    clean_names() %>%
    transmute(
      delivery_month    = as.Date(delivery_month),
      trade_cycle_start = as.Date(trade_cycle_start),
      trade_cycle_end   = as.Date(trade_cycle_end),
      period_label      = blank_to_na(period_label)
    ) %>%
    filter(!is.na(delivery_month)) %>%
    distinct(delivery_month, .keep_all = TRUE)
}

# ---- Raw rules for one broker sheet (cleaned, active-only, upper keys) ----
# Returns a tibble with standardized helper columns the normalizers rely on.
load_rules <- function(sheet, workbook = WORKBOOK) {
  raw <- read_excel(workbook, sheet = sheet) %>%
    clean_names() %>%
    add_missing_cols(c(
      "broker", "raw_product", "raw_price_basis", "raw_index", "raw_location",
      "raw_pipeline", "raw_product_type", "raw_trade_type", "normalized_product",
      "y_n", "index_trade_financial", "swap", "index_group", "key_id",
      "rule_priority", "rule_id", "active", "notes"
    ))

  raw %>%
    mutate(
      active_bool = case_when(
        str_to_upper(str_squish(as.character(active))) %in% c("TRUE", "YES", "Y", "1") ~ TRUE,
        str_to_upper(str_squish(as.character(active))) %in% c("FALSE", "NO", "N", "0") ~ FALSE,
        is.na(active) | str_squish(as.character(active)) == "" ~ TRUE,
        TRUE ~ FALSE
      ),
      rule_priority = suppressWarnings(as.numeric(rule_priority)),
      rule_priority = if_else(is.na(rule_priority), 999999, rule_priority),

      raw_product_key     = ukey(raw_product),
      raw_price_basis_key = ukey(raw_price_basis),
      raw_index_key       = ukey(raw_index),
      raw_location_key    = ukey(raw_location),
      raw_pipeline_key    = ukey(raw_pipeline),

      key_id        = blank_to_na(key_id),
      index_group   = blank_to_na(index_group),
      itf           = blank_to_na(index_trade_financial),
      y_n           = blank_to_na(y_n)
    ) %>%
    filter(active_bool, !is.na(raw_product_key))
}

# Convenience: load everything once into a reference bundle.
load_reference <- function(workbook = WORKBOOK) {
  list(
    tc_dates = load_tc_dates(workbook),
    rules = setNames(
      lapply(BROKER_RULE_SHEET, load_rules, workbook = workbook),
      names(BROKER_RULE_SHEET)
    ),
    workbook = workbook
  )
}
