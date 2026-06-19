# =============================================================================
# R/normalize_onex.R  ->  normalize_onex(ref)
# OneX: dedupe by newest export (date in filename), drop cancelled, and remove
# duplicate sleeve/economic copies. Rule match on instrument. Price basis = buyer_pays.
# =============================================================================

.onex_parse_date <- function(x) {
  x <- str_squish(as.character(x)); x[x == ""] <- NA_character_
  out <- as.Date(rep(NA, length(x)))
  is_serial <- !is.na(x) & str_detect(x, "^\\d+(\\.\\d+)?$")
  d <- suppressWarnings(parse_date_time(x[!is_serial],
                                        orders = c("ymd", "mdy", "dmy"), tz = "UTC"))
  out[!is_serial] <- as.Date(d)
  out[is_serial] <- as.Date(as.numeric(x[is_serial]), origin = "1899-12-30")
  out
}

normalize_onex <- function(ref, data_dir = DATA_DIR) {
  folder <- file.path(data_dir, BROKER_FOLDER[["OneX"]])
  files <- list_broker_files(folder)
  raw <- bind_rows(compact(map(files, safe_read_file)))  # first sheet = trades sheet
  if (nrow(raw) == 0) return(coerce_schema(NULL))

  raw <- raw %>%
    add_missing_cols(c("trade_id", "exec_date", "exec_time", "instrument", "period",
                       "qty", "unit", "price", "location", "pipeline", "buyer_pays",
                       "buyer_receives", "swap_trade", "cancelled", "sleeved", "is_financial")) %>%
    filter(if_any(everything(), ~ !is.na(.x) & str_squish(as.character(.x)) != "")) %>%
    mutate(
      trade_id = na_if(str_squish(as.character(trade_id)), ""),
      qty_m3 = clean_num(qty),
      price = clean_num(price),
      exec_date_clean = .onex_parse_date(exec_date),
      exec_time_clean = suppressWarnings(parse_date_time(
        str_squish(as.character(exec_time)),
        orders = c("I:M:S p", "I:M p", "H:M:S", "H:M"), tz = LOCAL_TZ)),
      delivery_month = parse_delivery_month(period),
      cancelled_bool = coalesce(parse_bool(cancelled), FALSE),
      sleeved_bool   = coalesce(parse_bool(sleeved), FALSE),
      instrument_key = ukey(instrument),
      export_end = suppressWarnings(ymd(
        str_match(source_file, "(\\d{4}-\\d{2}-\\d{2})_(\\d{4}-\\d{2}-\\d{2})")[, 3])),
      trade_id_num = suppressWarnings(as.numeric(trade_id)),
      dedupe_key = if_else(is.na(trade_id),
                           paste0(source_file, "_row_", row_number()), trade_id)
    ) %>%
    arrange(desc(export_end), desc(source_file_modified)) %>%
    distinct(dedupe_key, .keep_all = TRUE) %>%
    filter(!cancelled_bool)

  # ---- remove duplicate sleeve/economic copies ----
  raw <- raw %>%
    mutate(
      economic_match_key = paste(
        exec_date_clean, as.character(exec_time_clean), ukey(instrument), ukey(period),
        qty_m3, ukey(unit), price, ukey(pipeline), ukey(buyer_pays),
        ukey(buyer_receives), ukey(swap_trade), sep = " | ")
    ) %>%
    group_by(economic_match_key) %>%
    mutate(
      grp_n = n(),
      grp_has_sleeve = any(sleeved_bool, na.rm = TRUE)
    ) %>%
    arrange(sleeved_bool, trade_id_num, .by_group = TRUE) %>%
    mutate(keep_rank = row_number()) %>%
    ungroup() %>%
    filter(!(grp_n > 1 & grp_has_sleeve & keep_rank > 1))

  onex_rules <- ref$rules$OneX %>%
    arrange(rule_priority) %>%
    distinct(raw_product_key, .keep_all = TRUE) %>%
    select(raw_product_key, r_itf = itf, r_ig = index_group, r_kid = key_id, r_yn = y_n)

  raw <- raw %>%
    left_join(onex_rules, by = c("instrument_key" = "raw_product_key")) %>%
    mutate(
      itf = r_itf, index_group = r_ig, grade = r_kid,
      in_index = coalesce(parse_bool(r_yn), FALSE),
      swap_leg = case_when(
        ukey(swap_trade) == "PARENT" ~ "Swap",
        ukey(swap_trade) == "LEG" ~ "Leg",
        TRUE ~ "Outright"
      ),
      exec_datetime = as.POSIXct(NA, tz = LOCAL_TZ),
      exec_datetime = if_else(
        !is.na(exec_date_clean) & !is.na(exec_time_clean),
        as.POSIXct(paste(exec_date_clean, format(exec_time_clean, "%H:%M:%S")),
                   tz = LOCAL_TZ),
        as.POSIXct(exec_date_clean, tz = LOCAL_TZ)
      ),
      exec_hour = if_else(!is.na(exec_time_clean), hour(exec_time_clean), NA_integer_)
    )

  out <- raw %>%
    transmute(
      broker = "OneX",
      trade_id,
      exec_datetime,
      exec_date = exec_date_clean,
      exec_hour,
      instrument,
      grade, index_group, itf,
      swap_leg_outright = swap_leg,
      in_index,
      period,
      delivery_month,
      price, qty_m3,
      unit_raw = unit,
      location = location,
      pipeline = pipeline,
      price_basis = buyer_pays,
      source_file
    ) %>%
    attach_trade_cycle(ref$tc_dates)

  coerce_schema(out)
}
