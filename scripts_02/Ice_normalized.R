# =============================================================================
# ICE / ACE normalization
# =============================================================================

source(file.path(getwd(), "scripts", "00_packages.R"))

folder_path <- "C:/Users/amominzada/OneDrive - Energy Transfer/Documents/Excel sheets/Price Index excel sheets/ICE"

validation_path <- "C:/Users/amominzada/OneDrive - Energy Transfer/Documents/Projects/Index_App/Index_App/data/reference/classification_validation/broker_classification_validation.xlsx"

BBL_PER_M3 <- 6.28981077

# -----------------------------
# 1. Read ICE broker files
# -----------------------------

files <- list.files(
  path = folder_path,
  pattern = "\\.(xlsx|xls|csv)$",
  full.names = TRUE,
  ignore.case = TRUE
)

clean_ice_names <- function(df) {
  df %>%
    janitor::clean_names() %>%
    rename(
      gtc_s = any_of(c("gtcs", "gtc_s", "g_t_c_s"))
    )
}

read_one_file <- function(file) {
  
  ext <- str_to_lower(tools::file_ext(file))
  
  df <- if (ext == "csv") {
    read_csv(
      file,
      col_types = cols(.default = col_character()),
      show_col_types = FALSE
    )
  } else {
    read_excel(
      file,
      col_types = "text"
    )
  }
  
  df %>%
    clean_ice_names() %>%
    mutate(
      source_file = basename(file),
      source_file_modified = file.info(file)$mtime,
      broker = "ICE"
    )
}

ice_raw_all <- files %>%
  map_dfr(read_one_file) %>%
  mutate(
    deal_id = as.character(deal_id)
  ) %>%
  arrange(desc(source_file_modified)) %>%
  distinct(deal_id, .keep_all = TRUE)

# -----------------------------
# 2. Clean ICE raw fields
# -----------------------------

ice_raw <- ice_raw_all %>%
  mutate(
    market_raw_without_strip = case_when(
      is.na(market) ~ NA_character_,
      str_detect(market, "\\s+-\\s+") ~ str_replace(market, "\\s+-\\s+[^-]+$", ""),
      TRUE ~ market
    ),
    
    market_key = str_to_upper(str_squish(as.character(market_raw_without_strip))),
    
    is_cancelled_bool = case_when(
      str_to_upper(str_squish(as.character(is_cancelled))) %in% c("Y", "YES", "TRUE", "1") ~ TRUE,
      TRUE ~ FALSE
    ),
    
    parent_fin_assist_bool = !is.na(parent_fin_assist) &
      str_squish(as.character(parent_fin_assist)) != "",
    
    product_group_clean = str_to_upper(str_squish(as.character(product_group))),
    product_name_clean = str_to_upper(str_squish(as.character(product_name))),
    market_clean = str_to_upper(str_squish(as.character(market))),
    product_full_name_clean = str_to_upper(str_squish(as.character(product_full_name))),
    
    is_financial_product = str_starts(product_group_clean, "FINANCIAL"),
    
    is_spread_parent = str_detect(product_name_clean, "SPR") |
      str_detect(market_clean, "SPR") |
      str_detect(product_full_name_clean, "SPR"),
    
    is_spread_leg = !is.na(spread_parent_id) &
      str_squish(as.character(spread_parent_id)) != ""
  ) %>%
  filter(
    is_cancelled_bool == FALSE,
    parent_fin_assist_bool == FALSE
  )

# -----------------------------
# 3. Read ICE classification rules
# -----------------------------

ice_rules <- read_excel(validation_path, sheet = "ICE_Rules") %>%
  clean_names() %>%
  mutate(
    raw_product_key = str_to_upper(str_squish(as.character(raw_product))),
    active_bool = case_when(
      active %in% TRUE ~ TRUE,
      str_to_upper(as.character(active)) %in% c("TRUE", "YES", "Y", "1") ~ TRUE,
      is.na(active) ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  filter(
    active_bool == TRUE,
    !is.na(raw_product_key),
    raw_product_key != ""
  ) %>%
  arrange(rule_priority) %>%
  distinct(raw_product_key, .keep_all = TRUE) %>%
  transmute(
    market_key = raw_product_key,
    rule_raw_product = raw_product,
    rule_index_trade_financial = index_trade_financial,
    rule_swap = swap,
    rule_index_group = index_group,
    rule_key_id = key_id,
    rule_y_n = y_n
  )

# -----------------------------
# 4. Normalize ICE data
# -----------------------------

parse_ice_strip_month <- function(x) {
  
  x <- str_squish(as.character(x))
  
  month_map <- c(
    JAN = 1, FEB = 2, MAR = 3, APR = 4,
    MAY = 5, JUN = 6, JUL = 7, AUG = 8,
    SEP = 9, OCT = 10, NOV = 11, DEC = 12
  )
  
  matched <- str_match(str_to_upper(x), "^([A-Z]{3})(\\d{2}|\\d{4})$")
  
  month_txt <- matched[, 2]
  year_txt <- matched[, 3]
  
  month_num <- unname(month_map[month_txt])
  
  year_num <- ifelse(
    is.na(year_txt),
    NA_integer_,
    ifelse(nchar(year_txt) == 2, 2000 + as.integer(year_txt), as.integer(year_txt))
  )
  
  as.Date(
    ifelse(
      is.na(month_num) | is.na(year_num),
      NA_character_,
      sprintf("%04d-%02d-01", year_num, month_num)
    )
  )
}

ice_normalized_base <- ice_raw %>%
  left_join(
    ice_rules,
    by = "market_key"
  ) %>%
  mutate(
    local_datetime_clean = str_squish(as.character(local_datetime)),
    exec_date = suppressWarnings(mdy(str_sub(local_datetime_clean, 1, 10))),
    exec_time = str_sub(local_datetime_clean, 12, 19),
    
    period_delivery_month = parse_ice_strip_month(strip),
    
    qty_num = as.numeric(str_remove_all(qty, ",")),
    price_num = as.numeric(str_remove_all(price, ",")),
    
    in_index_bool = case_when(
      str_to_upper(str_squish(as.character(rule_y_n))) %in% c("Y", "YES", "TRUE", "1") ~ TRUE,
      
      # ICE_Rules currently has Y_N blank, so a matched Trade rule counts as index-eligible.
      is.na(rule_y_n) &
        rule_index_trade_financial == "Trade" &
        !is.na(rule_index_group) &
        !is.na(rule_key_id) ~ TRUE,
      
      TRUE ~ FALSE
    ),
    
    swap_leg_outright = case_when(
      is_spread_parent == TRUE ~ "Swap",
      is_spread_leg == TRUE ~ "Leg",
      TRUE ~ "Outright"
    ),
    
    index_trade_financial = rule_index_trade_financial,
    index_group = rule_index_group,
    key_id = rule_key_id
  )

# -----------------------------
# 5. Keep only one row per spread_parent_id
# This prevents double-counting ICE spread legs.
# Outright rows are not affected.
# -----------------------------

ice_normalized_base <- ice_normalized_base %>%
  group_by(spread_parent_id) %>%
  arrange(deal_id, .by_group = TRUE) %>%
  mutate(
    spread_leg_rank = if_else(
      is.na(spread_parent_id) | str_squish(as.character(spread_parent_id)) == "",
      1L,
      row_number()
    )
  ) %>%
  ungroup() %>%
  filter(
    is.na(spread_parent_id) |
      str_squish(as.character(spread_parent_id)) == "" |
      spread_leg_rank == 1L
  )

# -----------------------------
# 6. Final ICE normalized shape
# Same structure as Modern / OneX
# -----------------------------

ice_normalized <- ice_normalized_base %>%
  transmute(
    `Trade ID` = deal_id,
    `Exec Date` = exec_date,
    `Exec Time` = exec_time,
    Instrument = market_raw_without_strip,
    Period = strip,
    Qty = qty_num,
    Unit = "m3",
    Price = price_num,
    `Price Basis` = trade_index,
    Location = hub_name,
    Broker = broker,
    `In Index?` = in_index_bool,
    `Index/Trade/Financial` = index_trade_financial,
    `Swap/Leg/Outright` = swap_leg_outright,
    `Index Group` = index_group,
    KeyID = key_id,
    period_delivery_month,
    source_file
  )

# -----------------------------
# 7. Add TC dates
# -----------------------------

tc_dates <- read_excel(validation_path, sheet = "TC_Dates") %>%
  clean_names() %>%
  transmute(
    delivery_month = as.Date(delivery_month),
    trade_cycle_start = as.Date(trade_cycle_start),
    trade_cycle_end = as.Date(trade_cycle_end),
    period_label = period_label
  )

ice_normalized <- ice_normalized %>%
  mutate(
    exec_date_check = as.Date(`Exec Date`)
  ) %>%
  left_join(
    tc_dates,
    by = c("period_delivery_month" = "delivery_month")
  ) %>%
  mutate(
    `Inside Trade Cycle?` = case_when(
      !is.na(exec_date_check) &
        !is.na(trade_cycle_start) &
        !is.na(trade_cycle_end) &
        exec_date_check >= trade_cycle_start &
        exec_date_check <= trade_cycle_end ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  select(
    `Trade ID`,
    `Exec Date`,
    `Exec Time`,
    Instrument,
    Period,
    Qty,
    Unit,
    Price,
    `Price Basis`,
    Location,
    Broker,
    `In Index?`,
    `Index/Trade/Financial`,
    `Swap/Leg/Outright`,
    `Index Group`,
    KeyID,
    period_delivery_month,
    trade_cycle_start,
    trade_cycle_end,
    `Inside Trade Cycle?`,
    source_file
  )

ice_normalized



# =============================================================================
# ICE SW VWAP only
# =============================================================================

ice_sw_vwap_input <- ice_normalized %>%
  mutate(
    index_group_clean = str_to_upper(str_squish(as.character(`Index Group`))),
    class_clean = str_to_upper(str_squish(as.character(`Index/Trade/Financial`))),
    swap_leg_clean = str_to_upper(str_squish(as.character(`Swap/Leg/Outright`))),
    price_num = as.numeric(Price),
    qty_m3_month = as.numeric(Qty)
  ) %>%
  filter(
    index_group_clean == "SW",
    class_clean == "TRADE",
    swap_leg_clean == "OUTRIGHT",
    `Inside Trade Cycle?` == TRUE,
    `In Index?` == TRUE,
    !is.na(price_num),
    !is.na(qty_m3_month),
    qty_m3_month > 0
  )

ice_sw_vwap <- ice_sw_vwap_input %>%
  group_by(
    Period,
    period_delivery_month,
    trade_cycle_start,
    trade_cycle_end,
    `Index Group`
  ) %>%
  summarise(
    trade_count = n(),
    total_volume_m3_per_month = sum(qty_m3_month, na.rm = TRUE),
    vwap = sum(price_num * qty_m3_month, na.rm = TRUE) /
      sum(qty_m3_month, na.rm = TRUE),
    min_price = min(price_num, na.rm = TRUE),
    max_price = max(price_num, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(period_delivery_month)

ice_sw_vwap