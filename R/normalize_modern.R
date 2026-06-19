# =============================================================================
# R/normalize_modern.R  ->  normalize_modern(ref)
# Modcom: finalized non-credit-facilitation only. Two rule tiers:
#   (product + price_basis) then (product). SW rules require price basis = WTI CMA,
#   so rows priced "COMM/1A + WTI CMA" stay unmatched on purpose (index-referencing).
# =============================================================================

normalize_modern <- function(ref, data_dir = DATA_DIR) {
  folder <- file.path(data_dir, BROKER_FOLDER[["Modern"]])
  files <- list_broker_files(folder)
  raw <- bind_rows(compact(map(files, ~ safe_read_file(.x, prefer_sheet = "Trade Report"))))
  if (nrow(raw) == 0) return(coerce_schema(NULL))

  raw <- raw %>%
    mutate(trade_number = as.character(trade_number)) %>%
    arrange(desc(source_file_modified)) %>%
    distinct(trade_number, .keep_all = TRUE) %>%
    mutate(
      state_clean = ukey(state),
      credit_fac  = coalesce(parse_bool(credit_facilitation), FALSE)
    ) %>%
    filter(state_clean == "FINALIZED", !credit_fac)

  mr <- ref$rules$Modern
  prod_rules <- mr %>%
    filter(is.na(raw_price_basis_key)) %>%
    arrange(rule_priority) %>%
    distinct(raw_product_key, .keep_all = TRUE) %>%
    select(raw_product_key, p_itf = itf, p_ig = index_group, p_kid = key_id)
  pb_rules <- mr %>%
    filter(!is.na(raw_price_basis_key)) %>%
    arrange(rule_priority) %>%
    distinct(raw_product_key, raw_price_basis_key, .keep_all = TRUE) %>%
    select(raw_product_key, raw_price_basis_key,
           pb_itf = itf, pb_ig = index_group, pb_kid = key_id)

  raw <- raw %>%
    mutate(
      product_key     = ukey(product),
      price_basis_key = ukey(price_basis)
    ) %>%
    left_join(pb_rules, by = c("product_key" = "raw_product_key",
                               "price_basis_key" = "raw_price_basis_key")) %>%
    left_join(prod_rules, by = c("product_key" = "raw_product_key")) %>%
    mutate(
      itf         = coalesce(pb_itf, p_itf),
      index_group = coalesce(pb_ig, p_ig),
      grade       = coalesce(pb_kid, p_kid),
      exec_datetime = parse_date_time(
        executed_timestamp,
        orders = c("Ymd IMS p", "Ymd HMS", "Ymd HM"),
        tz = LOCAL_TZ, quiet = TRUE
      ),
      exec_date = as.Date(exec_datetime),
      delivery_month = parse_delivery_month(term),
      swap_leg = case_when(
        ukey(trade_type) == "SPREAD" ~ "Swap",
        ukey(trade_type) %in% c("FIRST LEG", "SECOND LEG") ~ "Leg",
        ukey(trade_type) == "OUTRIGHT" ~ "Outright",
        TRUE ~ as.character(trade_type)
      ),
      in_index = coalesce(parse_bool(in_index), FALSE),
      qty_m3 = clean_num(volume),
      price = clean_num(price)
    )

  out <- raw %>%
    transmute(
      broker = "Modern",
      trade_id = trade_number,
      exec_datetime, exec_date,
      exec_hour = hour(exec_datetime),
      instrument = product,
      grade, index_group, itf,
      swap_leg_outright = swap_leg,
      in_index,
      period = term,
      delivery_month,
      price, qty_m3,
      unit_raw = unit_of_measure,
      location = location,
      pipeline = pipeline_terminal,
      price_basis = price_basis,
      source_file
    ) %>%
    attach_trade_cycle(ref$tc_dates)

  coerce_schema(out)
}
