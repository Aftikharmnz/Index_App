source(file.path(getwd(), "scripts", "00_packages.R"))


folder_path <- "C:/Users/amominzada/OneDrive - Energy Transfer/Documents/Excel sheets/Price Index excel sheets/Modern"

validation_path <- "C:/Users/amominzada/OneDrive - Energy Transfer/Documents/Projects/Index_App/Index_App/data/reference/classification_validation/broker_classification_validation.xlsx"

# -----------------------------
# 1. Read Modern broker files
# -----------------------------

files <- list.files(
  path = folder_path,
  pattern = "\\.(xlsx|xls|csv)$",
  full.names = TRUE,
  ignore.case = TRUE
)

read_one_file <- function(file) {
  
  ext <- str_to_lower(tools::file_ext(file))
  
  df <- if (ext == "csv") {
    read_csv(file, show_col_types = FALSE)
  } else {
    read_excel(file)
  }
  
  df %>%
    clean_names() %>%
    mutate(
      source_file = basename(file),
      source_file_modified = file.info(file)$mtime,
      broker = "Modern"
    )
}

modern_raw_all <- files %>%
  map_dfr(read_one_file) %>%
  mutate(
    trade_number = as.character(trade_number),
    
    state_clean = str_to_upper(str_squish(as.character(state))),
    
    credit_facilitation_bool = case_when(
      credit_facilitation %in% TRUE ~ TRUE,
      str_to_upper(as.character(credit_facilitation)) %in% c("TRUE", "YES", "Y", "1") ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  arrange(desc(source_file_modified)) %>%
  distinct(trade_number, .keep_all = TRUE)

# Keep only finalized non-credit-facilitation trades
modern_raw <- modern_raw_all %>%
  filter(
    state_clean == "FINALIZED",
    credit_facilitation_bool == FALSE
  )

# -----------------------------
# 2. Read Modern classification rules
# -----------------------------

modern_rules <- read_excel(validation_path, sheet = "Modern_Rules") %>%
  clean_names() %>%
  filter(active == TRUE) %>%
  mutate(
    raw_product_key = str_to_upper(str_squish(as.character(raw_product))),
    raw_price_basis_key = str_to_upper(str_squish(as.character(raw_price_basis)))
  )

product_rules <- modern_rules %>%
  filter(
    !is.na(raw_product_key),
    is.na(raw_price_basis_key) | raw_price_basis_key == ""
  ) %>%
  arrange(rule_priority) %>%
  distinct(raw_product_key, .keep_all = TRUE) %>%
  transmute(
    product_key = raw_product_key,
    product_rule_index_trade_financial = index_trade_financial,
    product_rule_swap = swap,
    product_rule_index_group = index_group,
    product_rule_key_id = key_id
  )

price_basis_rules <- modern_rules %>%
  filter(
    !is.na(raw_product_key),
    !is.na(raw_price_basis_key),
    raw_price_basis_key != ""
  ) %>%
  arrange(rule_priority) %>%
  distinct(raw_product_key, raw_price_basis_key, .keep_all = TRUE) %>%
  transmute(
    product_key = raw_product_key,
    price_basis_key = raw_price_basis_key,
    pb_rule_index_trade_financial = index_trade_financial,
    pb_rule_swap = swap,
    pb_rule_index_group = index_group,
    pb_rule_key_id = key_id
  )




# -----------------------------
# 3. Normalize Modern data
# -----------------------------

modern_normalized <- modern_raw %>%
  mutate(
    product_key = str_to_upper(str_squish(as.character(product))),
    price_basis_key = str_to_upper(str_squish(as.character(price_basis))),
    product_type_key = str_to_upper(str_squish(as.character(product_type))),
    trade_type_key = str_to_upper(str_squish(as.character(trade_type))),
    
    credit_facilitation_bool = case_when(
      credit_facilitation %in% TRUE ~ TRUE,
      str_to_upper(as.character(credit_facilitation)) %in% c("TRUE", "YES", "Y", "1") ~ TRUE,
      TRUE ~ FALSE
    ),
    
    in_index_bool = case_when(
      in_index %in% TRUE ~ TRUE,
      str_to_upper(as.character(in_index)) %in% c("TRUE", "YES", "Y", "1") ~ TRUE,
      TRUE ~ FALSE
    ),
    
    executed_timestamp = as.POSIXct(
      as.character(executed_timestamp),
      format = "%Y-%m-%d %I:%M:%S %p",
      tz = "America/Edmonton"
    ),
    exec_date = as.Date(executed_timestamp),
    exec_time = format(executed_timestamp, "%H:%M:%S")
  ) %>%
  left_join(
    price_basis_rules,
    by = c("product_key", "price_basis_key")
  ) %>%
  left_join(
    product_rules,
    by = "product_key"
  ) %>%
  mutate(
    index_trade_financial = coalesce(
      pb_rule_index_trade_financial,
      product_rule_index_trade_financial
    ),
    
    swap_leg_outright = case_when(
      trade_type_key == "SPREAD" ~ "Swap",
      trade_type_key %in% c("FIRST LEG", "SECOND LEG") ~ "Leg",
      trade_type_key == "OUTRIGHT" ~ "Outright",
      TRUE ~ trade_type
    ),
    
    index_group = coalesce(
      pb_rule_index_group,
      product_rule_index_group
    ),
    
    key_id = coalesce(
      pb_rule_key_id,
      product_rule_key_id
    )
  ) %>%
  transmute(
    `Trade ID` = trade_number,
    `Exec Date` = exec_date,
    `Exec Time` = exec_time,
    Instrument = product,
    Period = term,
    Qty = volume,
    Unit = unit_of_measure,
    Price = price,
    `Price Basis` = price_basis,
    Location = pipeline_terminal,
    Broker = broker,
    `In Index?` = in_index_bool,
    `Index/Trade/Financial` = index_trade_financial,
    `Swap/Leg/Outright` = swap_leg_outright,
    `Index Group` = index_group,
    KeyID = key_id,
    source_file
  )

tc_dates <- read_excel(validation_path, sheet = "TC_Dates") %>%
  clean_names() %>%
  transmute(
    delivery_month = as.Date(delivery_month),
    trade_cycle_start = as.Date(trade_cycle_start),
    trade_cycle_end = as.Date(trade_cycle_end),
    period_label = period_label
  )


# -----------------------------
# 2. Convert Period to delivery month
# Example: JUL-26 -> 2026-07-01
# -----------------------------

parse_modern_period_month <- function(x) {
  
  x <- str_to_upper(str_squish(as.character(x)))
  
  month_map <- c(
    JAN = 1, FEB = 2, MAR = 3, APR = 4,
    MAY = 5, JUN = 6, JUL = 7, AUG = 8,
    SEP = 9, OCT = 10, NOV = 11, DEC = 12
  )
  
  matched <- str_match(x, "^([A-Z]{3})-(\\d{2}|\\d{4})$")
  
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


# -----------------------------
# 3. Add trade-cycle check
# -----------------------------

modern_normalized <- modern_normalized %>%
  mutate(
    period_delivery_month = parse_modern_period_month(Period),
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

modern_normalized




# calculating vwap:

BBL_PER_M3 <- 6.28981077

modern_sw_vwap_input <- modern_normalized %>%
  mutate(
    index_group_clean = str_to_upper(str_squish(as.character(`Index Group`))),
    unit_clean = str_to_lower(str_squish(as.character(Unit))),
    price_num = as.numeric(Price),
    qty_num = as.numeric(Qty),
    
    delivery_days = days_in_month(period_delivery_month),
    
    qty_bbl = case_when(
      str_detect(unit_clean, "m3|m³") ~ qty_num * BBL_PER_M3,
      str_detect(unit_clean, "bbl") & str_detect(unit_clean, "day|d") ~ qty_num * delivery_days,
      str_detect(unit_clean, "bbl") ~ qty_num,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(
    index_group_clean == "SW",
    `Index/Trade/Financial` == "Trade",
    `Swap/Leg/Outright` == "Outright",
    `Inside Trade Cycle?` == TRUE,
    `In Index?` == TRUE,
    !is.na(price_num),
    !is.na(qty_bbl),
    qty_bbl > 0
  )

modern_sw_vwap <- modern_sw_vwap_input %>%
  group_by(
    Period,
    period_delivery_month,
    `Index Group`
  ) %>%
  summarise(
    trade_count = n(),
    total_volume_bbl = sum(qty_bbl, na.rm = TRUE),
    total_volume_m3 = total_volume_bbl / BBL_PER_M3,
    vwap = sum(price_num * qty_bbl, na.rm = TRUE) / sum(qty_bbl, na.rm = TRUE),
    min_price = min(price_num, na.rm = TRUE),
    max_price = max(price_num, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(period_delivery_month, `Index Group`)

modern_sw_vwap





#

modern_sw_vwap <- modern_sw_vwap_input %>%
  group_by(
    Period,
    period_delivery_month,
    `Index Group`
  ) %>%
  summarise(
    trade_count = n(),
    total_volume_bbl = sum(qty_bbl, na.rm = TRUE),
    total_volume_m3 = total_volume_bbl / BBL_PER_M3,
    vwap = sum(price_num * qty_bbl, na.rm = TRUE) / sum(qty_bbl, na.rm = TRUE),
    min_price = min(price_num, na.rm = TRUE),
    max_price = max(price_num, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(period_delivery_month, `Index Group`)

modern_sw_vwap