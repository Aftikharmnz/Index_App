# =============================================================================
# R/normalize_ice.R  ->  normalize_ice(ref) returns the shared schema
# ICE/ACE: exec time = LOCAL DATETIME (local Edmonton); DEAL TIME is UTC.
# Rule match on market key (product + location, strip-month removed).
# =============================================================================

normalize_ice <- function(ref, data_dir = DATA_DIR) {
  folder <- file.path(data_dir, BROKER_FOLDER[["ICE"]])
  files <- list_broker_files(folder)
  raw <- bind_rows(compact(map(files, safe_read_file)))
  if (nrow(raw) == 0) return(coerce_schema(NULL))

  raw <- raw %>%
    mutate(deal_id = as.character(deal_id)) %>%
    arrange(desc(source_file_modified)) %>%
    distinct(deal_id, .keep_all = TRUE) %>%
    mutate(
      is_cancelled_bool = ukey(is_cancelled) %in% c("Y", "YES", "TRUE", "1"),
      parent_fin_assist_bool = !is.na(parent_fin_assist) &
        str_squish(as.character(parent_fin_assist)) != ""
    ) %>%
    filter(!is_cancelled_bool, !parent_fin_assist_bool) %>%
    mutate(
      market_no_strip = case_when(
        is.na(market) ~ NA_character_,
        str_detect(market, "\\s+-\\s+") ~ str_replace(market, "\\s+-\\s+[^-]+$", ""),
        TRUE ~ market
      ),
      market_key = ukey(market_no_strip)
    )

  ice_rules <- ref$rules$ICE %>%
    arrange(rule_priority) %>%
    distinct(raw_product_key, .keep_all = TRUE) %>%
    select(raw_product_key, r_itf = itf, r_ig = index_group, r_kid = key_id, r_yn = y_n)

  raw <- raw %>%
    left_join(ice_rules, by = c("market_key" = "raw_product_key")) %>%
    # one row per spread parent (drop duplicate spread legs); outrights unaffected
    group_by(spread_parent_id) %>%
    arrange(deal_id, .by_group = TRUE) %>%
    mutate(
      spread_leg_rank = if_else(
        is.na(spread_parent_id) | str_squish(as.character(spread_parent_id)) == "",
        1L, row_number()
      )
    ) %>%
    ungroup() %>%
    filter(
      is.na(spread_parent_id) |
        str_squish(as.character(spread_parent_id)) == "" |
        spread_leg_rank == 1L
    ) %>%
    mutate(
      ld = str_squish(as.character(local_datetime)),
      exec_date = mdy(str_sub(ld, 1, 10)),
      exec_time = str_sub(ld, 12, 19),
      exec_datetime = ymd_hms(paste(exec_date, exec_time), tz = LOCAL_TZ, quiet = TRUE),
      delivery_month = parse_delivery_month(strip),
      qty_m3 = clean_num(qty),
      price = clean_num(price),
      swap_leg = if_else(
        !is.na(spread_parent_id) & str_squish(as.character(spread_parent_id)) != "",
        "Leg", "Outright"
      ),
      in_index = case_when(
        ukey(r_yn) %in% c("Y", "YES", "TRUE", "1") ~ TRUE,
        is.na(r_yn) & r_itf == "Trade" & !is.na(r_ig) & !is.na(r_kid) ~ TRUE,
        TRUE ~ FALSE
      )
    )

  out <- raw %>%
    transmute(
      broker = "ICE",
      trade_id = deal_id,
      exec_datetime, exec_date,
      exec_hour = hour(exec_datetime),
      instrument = market_no_strip,
      grade = r_kid,
      index_group = r_ig,
      itf = r_itf,
      swap_leg_outright = swap_leg,
      in_index,
      period = strip,
      delivery_month,
      price, qty_m3,
      unit_raw = "m3",
      location = hub_name,
      pipeline = NA_character_,
      price_basis = trade_index,
      source_file
    ) %>%
    attach_trade_cycle(ref$tc_dates)

  coerce_schema(out)
}
