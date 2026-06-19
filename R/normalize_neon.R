# =============================================================================
# R/normalize_neon.R  ->  normalize_neon(ref)
# Marex/Net Energy. 4-tier rule match: (product+index+pipeline) ->
#   (product+index+location) -> (product+index) -> (product). Parent/leg via parent id.
# =============================================================================

normalize_neon <- function(ref, data_dir = DATA_DIR) {
  folder <- file.path(data_dir, BROKER_FOLDER[["Neon"]])
  files <- list_broker_files(folder)
  raw <- bind_rows(compact(map(files, ~ safe_read_file(.x, prefer_sheet = "Trades"))))
  if (nrow(raw) == 0) return(coerce_schema(NULL))

  raw <- raw %>%
    add_missing_cols(c("trade_id", "parent_trade_id", "trade_time", "product", "period",
                       "location", "pipeline", "index", "qty", "price",
                       "delivery_start", "delivery_end")) %>%
    mutate(
      trade_id = str_squish(as.character(trade_id)),
      parent_trade_id = na_if(str_squish(as.character(parent_trade_id)), "")
    ) %>%
    arrange(desc(source_file_modified)) %>%
    distinct(trade_id, .keep_all = TRUE)

  parents <- raw %>% filter(!is.na(parent_trade_id)) %>% pull(parent_trade_id) %>% unique()

  # ---- Marex rule tiers ----
  mx <- ref$rules$Neon %>%
    mutate(raw_index_key = coalesce(raw_index_key, raw_price_basis_key))

  pipeline_rules <- mx %>%
    filter(!is.na(raw_product_key), !is.na(raw_index_key), !is.na(raw_pipeline_key)) %>%
    arrange(rule_priority) %>%
    distinct(raw_product_key, raw_index_key, raw_pipeline_key, .keep_all = TRUE) %>%
    select(raw_product_key, raw_index_key, raw_pipeline_key,
           pl_itf = itf, pl_ig = index_group, pl_kid = key_id, pl_yn = y_n)
  location_rules <- mx %>%
    filter(!is.na(raw_product_key), !is.na(raw_index_key), !is.na(raw_location_key),
           is.na(raw_pipeline_key)) %>%
    arrange(rule_priority) %>%
    distinct(raw_product_key, raw_index_key, raw_location_key, .keep_all = TRUE) %>%
    select(raw_product_key, raw_index_key, raw_location_key,
           lo_itf = itf, lo_ig = index_group, lo_kid = key_id, lo_yn = y_n)
  index_rules <- mx %>%
    filter(!is.na(raw_product_key), !is.na(raw_index_key),
           is.na(raw_pipeline_key), is.na(raw_location_key)) %>%
    arrange(rule_priority) %>%
    distinct(raw_product_key, raw_index_key, .keep_all = TRUE) %>%
    select(raw_product_key, raw_index_key,
           ix_itf = itf, ix_ig = index_group, ix_kid = key_id, ix_yn = y_n)
  product_rules <- mx %>%
    filter(!is.na(raw_product_key), is.na(raw_index_key),
           is.na(raw_pipeline_key), is.na(raw_location_key)) %>%
    arrange(rule_priority) %>%
    distinct(raw_product_key, .keep_all = TRUE) %>%
    select(raw_product_key, pr_itf = itf, pr_ig = index_group, pr_kid = key_id, pr_yn = y_n)

  raw <- raw %>%
    mutate(
      product_key  = ukey(product),
      index_key    = ukey(index),
      location_key = ukey(location),
      pipeline_key = ukey(pipeline)
    ) %>%
    left_join(pipeline_rules, by = c("product_key" = "raw_product_key",
                                     "index_key" = "raw_index_key",
                                     "pipeline_key" = "raw_pipeline_key")) %>%
    left_join(location_rules, by = c("product_key" = "raw_product_key",
                                     "index_key" = "raw_index_key",
                                     "location_key" = "raw_location_key")) %>%
    left_join(index_rules, by = c("product_key" = "raw_product_key",
                                  "index_key" = "raw_index_key")) %>%
    left_join(product_rules, by = c("product_key" = "raw_product_key")) %>%
    mutate(
      itf         = coalesce(pl_itf, lo_itf, ix_itf, pr_itf),
      index_group = coalesce(pl_ig,  lo_ig,  ix_ig,  pr_ig),
      grade       = coalesce(pl_kid, lo_kid, ix_kid, pr_kid),
      rule_yn     = coalesce(pl_yn,  lo_yn,  ix_yn,  pr_yn),
      in_index = coalesce(
        parse_bool(rule_yn),
        if_else(!is.na(index) & str_squish(index) != "", TRUE, FALSE)
      ),
      swap_leg = case_when(
        trade_id %in% parents ~ "Swap",
        !is.na(parent_trade_id) ~ "Leg",
        TRUE ~ "Outright"
      ),
      exec_datetime = parse_date_time(trade_time,
                                      orders = c("ymd HMS", "ymd HM", "ymd"),
                                      tz = LOCAL_TZ, quiet = TRUE),
      exec_date = as.Date(exec_datetime),
      delivery_month = coalesce(
        parse_delivery_month(period),
        floor_date(as.Date(parse_date_time(delivery_start,
                                           orders = c("ymd HMS", "ymd HM", "ymd"),
                                           tz = LOCAL_TZ, quiet = TRUE)), "month")
      ),
      qty_m3 = clean_num(qty),
      price = clean_num(price)
    )

  out <- raw %>%
    transmute(
      broker = "Neon",
      trade_id,
      exec_datetime, exec_date,
      exec_hour = hour(exec_datetime),
      instrument = product,
      grade, index_group, itf,
      swap_leg_outright = swap_leg,
      in_index,
      period,
      delivery_month,
      price, qty_m3,
      unit_raw = "m3",
      location = location,
      pipeline = pipeline,
      price_basis = index,
      source_file
    ) %>%
    attach_trade_cycle(ref$tc_dates)

  coerce_schema(out)
}
