source(file.path(getwd(), "scripts", "00_packages.R"))



folder_path <- "C:/Users/amominzada/OneDrive - Energy Transfer/Documents/Excel sheets/Price Index excel sheets/Neon"

validation_path <- "C:/Users/amominzada/OneDrive - Energy Transfer/Documents/Projects/Index_App/Index_App/data/reference/classification_validation/broker_classification_validation.xlsx"

broker_name <- "Neon"
rules_sheet <- "Marex_Rules"

# =============================================================================
# 1. Helper functions
# =============================================================================

add_missing_cols <- function(df, cols) {
  for (col in cols) {
    if (!col %in% names(df)) {
      df[[col]] <- NA_character_
    }
  }
  df
}

parse_number_clean <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all(",", "") %>%
    str_replace_all("\\$", "") %>%
    str_squish() %>%
    na_if("") %>%
    as.numeric()
}

parse_datetime_safe <- function(x) {
  suppressWarnings(
    parse_date_time(
      as.character(x),
      orders = c(
        "ymd HMS",
        "ymd HMS OS",
        "ymd IMS p",
        "mdy HMS",
        "mdy HMS OS",
        "mdy IMS p",
        "dmy HMS",
        "dmy HMS OS",
        "dmy IMS p",
        "ymd",
        "mdy",
        "dmy"
      ),
      tz = "America/Edmonton"
    )
  )
}

parse_bool_rule <- function(x) {
  x_chr <- str_to_upper(str_squish(as.character(x)))
  
  case_when(
    x_chr %in% c("TRUE", "YES", "Y", "1", "T") ~ TRUE,
    x_chr %in% c("FALSE", "NO", "N", "0", "F") ~ FALSE,
    TRUE ~ NA
  )
}

parse_period_month <- function(x) {
  
  x <- str_to_upper(str_squish(as.character(x)))
  
  month_map <- c(
    JAN = 1, FEB = 2, MAR = 3, APR = 4,
    MAY = 5, JUN = 6, JUL = 7, AUG = 8,
    SEP = 9, OCT = 10, NOV = 11, DEC = 12
  )
  
  matched <- str_match(x, "^([A-Z]{3})[- ]?(\\d{2}|\\d{4})$")
  
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

read_neon_one_file <- function(file) {
  
  ext <- str_to_lower(tools::file_ext(file))
  
  if (ext == "csv") {
    
    df <- read_csv(
      file,
      col_types = cols(.default = col_character()),
      show_col_types = FALSE
    )
    
  } else {
    
    sheets <- excel_sheets(file)
    sheet_to_read <- if ("Trades" %in% sheets) "Trades" else sheets[1]
    
    df <- read_excel(
      file,
      sheet = sheet_to_read,
      col_types = "text"
    )
  }
  
  df %>%
    clean_names() %>%
    filter(if_any(everything(), ~ !is.na(.x) & str_squish(as.character(.x)) != "")) %>%
    mutate(
      source_file = basename(file),
      source_file_modified = file.info(file)$mtime,
      broker = broker_name
    )
}

# =============================================================================
# 2. Read Neon raw files
# =============================================================================

files <- list.files(
  path = folder_path,
  pattern = "\\.(xlsx|xls|csv)$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(files) == 0) {
  stop("No Neon CSV/XLS/XLSX files found in folder_path.")
}

neon_raw_all <- files %>%
  map_dfr(read_neon_one_file) %>%
  add_missing_cols(c(
    "trade_id",
    "parent_trade_id",
    "type",
    "trade_time",
    "product",
    "period",
    "location",
    "pipeline",
    "index",
    "qty",
    "price",
    "delivery_start",
    "delivery_end"
  )) %>%
  mutate(
    trade_id = str_squish(as.character(trade_id)),
    parent_trade_id = str_squish(as.character(parent_trade_id)),
    parent_trade_id = na_if(parent_trade_id, ""),
    
    type = str_squish(as.character(type)),
    trade_time = str_squish(as.character(trade_time)),
    product = str_squish(as.character(product)),
    period = str_squish(as.character(period)),
    location = str_squish(as.character(location)),
    pipeline = str_squish(as.character(pipeline)),
    index = str_squish(as.character(index)),
    
    qty = parse_number_clean(qty),
    price = parse_number_clean(price),
    
    delivery_start = as.Date(parse_datetime_safe(delivery_start)),
    delivery_end = as.Date(parse_datetime_safe(delivery_end)),
    
    trade_id_sort = parse_number_clean(trade_id),
    
    dedupe_key = if_else(
      is.na(trade_id) | trade_id == "",
      paste0(source_file, "_row_", row_number()),
      trade_id
    )
  ) %>%
  arrange(desc(source_file_modified), desc(trade_id_sort)) %>%
  distinct(dedupe_key, .keep_all = TRUE)

neon_raw <- neon_raw_all

# =============================================================================
# 3. Define Swap / Leg / Outright logic
# =============================================================================

parent_trade_ids <- neon_raw %>%
  filter(!is.na(parent_trade_id), parent_trade_id != "") %>%
  distinct(parent_trade_id) %>%
  pull(parent_trade_id)

# Logic:
# If Trade ID appears anywhere in Parent Trade ID column -> Swap
# If Parent Trade ID is populated -> Leg
# Else -> Outright

neon_raw <- neon_raw %>%
  mutate(
    swap_leg_outright = case_when(
      trade_id %in% parent_trade_ids ~ "Swap",
      !is.na(parent_trade_id) & parent_trade_id != "" ~ "Leg",
      TRUE ~ "Outright"
    )
  )

# =============================================================================
# 4. Read Marex rules for Neon classification
# =============================================================================

available_sheets <- excel_sheets(validation_path)

if (!(rules_sheet %in% available_sheets)) {
  stop(paste0("Sheet '", rules_sheet, "' does not exist in validation workbook."))
}

marex_rules <- read_excel(validation_path, sheet = rules_sheet) %>%
  clean_names() %>%
  add_missing_cols(c(
    "active",
    "raw_product",
    "raw_price_basis",
    "raw_index",
    "raw_location",
    "raw_pipeline",
    "index_trade_financial",
    "swap",
    "index_group",
    "key_id",
    "y_n",
    "rule_priority"
  )) %>%
  mutate(
    active_bool = case_when(
      str_to_upper(str_squish(as.character(active))) %in% c("TRUE", "YES", "Y", "1") ~ TRUE,
      str_to_upper(str_squish(as.character(active))) %in% c("FALSE", "NO", "N", "0") ~ FALSE,
      is.na(active) | str_squish(as.character(active)) == "" ~ TRUE,
      TRUE ~ FALSE
    ),
    
    rule_priority = suppressWarnings(as.numeric(rule_priority)),
    rule_priority = if_else(is.na(rule_priority), 999999, rule_priority),
    
    raw_product_key = str_to_upper(str_squish(as.character(raw_product))),
    
    raw_index_key = coalesce(
      na_if(str_to_upper(str_squish(as.character(raw_index))), ""),
      na_if(str_to_upper(str_squish(as.character(raw_price_basis))), "")
    ),
    
    raw_location_key = na_if(str_to_upper(str_squish(as.character(raw_location))), ""),
    raw_pipeline_key = na_if(str_to_upper(str_squish(as.character(raw_pipeline))), "")
  ) %>%
  filter(active_bool == TRUE)

# Most specific: Product + Index + Pipeline
pipeline_rules <- marex_rules %>%
  filter(
    !is.na(raw_product_key), raw_product_key != "",
    !is.na(raw_index_key), raw_index_key != "",
    !is.na(raw_pipeline_key), raw_pipeline_key != ""
  ) %>%
  arrange(rule_priority) %>%
  distinct(raw_product_key, raw_index_key, raw_pipeline_key, .keep_all = TRUE) %>%
  transmute(
    product_key = raw_product_key,
    index_key = raw_index_key,
    pipeline_key = raw_pipeline_key,
    pipeline_rule_index_trade_financial = index_trade_financial,
    pipeline_rule_index_group = index_group,
    pipeline_rule_key_id = key_id,
    pipeline_rule_in_index = y_n
  )

# Second: Product + Index + Raw Location
location_rules <- marex_rules %>%
  filter(
    !is.na(raw_product_key), raw_product_key != "",
    !is.na(raw_index_key), raw_index_key != "",
    !is.na(raw_location_key), raw_location_key != "",
    is.na(raw_pipeline_key) | raw_pipeline_key == ""
  ) %>%
  arrange(rule_priority) %>%
  distinct(raw_product_key, raw_index_key, raw_location_key, .keep_all = TRUE) %>%
  transmute(
    product_key = raw_product_key,
    index_key = raw_index_key,
    location_key = raw_location_key,
    location_rule_index_trade_financial = index_trade_financial,
    location_rule_index_group = index_group,
    location_rule_key_id = key_id,
    location_rule_in_index = y_n
  )

# Third: Product + Index
index_rules <- marex_rules %>%
  filter(
    !is.na(raw_product_key), raw_product_key != "",
    !is.na(raw_index_key), raw_index_key != "",
    is.na(raw_pipeline_key) | raw_pipeline_key == "",
    is.na(raw_location_key) | raw_location_key == ""
  ) %>%
  arrange(rule_priority) %>%
  distinct(raw_product_key, raw_index_key, .keep_all = TRUE) %>%
  transmute(
    product_key = raw_product_key,
    index_key = raw_index_key,
    index_rule_index_trade_financial = index_trade_financial,
    index_rule_index_group = index_group,
    index_rule_key_id = key_id,
    index_rule_in_index = y_n
  )

# Fallback: Product only
product_rules <- marex_rules %>%
  filter(
    !is.na(raw_product_key), raw_product_key != "",
    is.na(raw_index_key) | raw_index_key == "",
    is.na(raw_pipeline_key) | raw_pipeline_key == "",
    is.na(raw_location_key) | raw_location_key == ""
  ) %>%
  arrange(rule_priority) %>%
  distinct(raw_product_key, .keep_all = TRUE) %>%
  transmute(
    product_key = raw_product_key,
    product_rule_index_trade_financial = index_trade_financial,
    product_rule_index_group = index_group,
    product_rule_key_id = key_id,
    product_rule_in_index = y_n
  )

# =============================================================================
# 5. Normalize Neon into same columns as modern_normalized
# =============================================================================

neon_normalized <- neon_raw %>%
  mutate(
    product_key = str_to_upper(str_squish(as.character(product))),
    index_key = str_to_upper(str_squish(as.character(index))),
    location_key = str_to_upper(str_squish(as.character(location))),
    pipeline_key = str_to_upper(str_squish(as.character(pipeline))),
    
    executed_timestamp = parse_datetime_safe(trade_time),
    exec_date = as.Date(executed_timestamp),
    exec_time = format(executed_timestamp, "%H:%M:%S")
  ) %>%
  left_join(
    pipeline_rules,
    by = c("product_key", "index_key", "pipeline_key")
  ) %>%
  left_join(
    location_rules,
    by = c("product_key", "index_key", "location_key")
  ) %>%
  left_join(
    index_rules,
    by = c("product_key", "index_key")
  ) %>%
  left_join(
    product_rules,
    by = "product_key"
  ) %>%
  mutate(
    index_trade_financial = coalesce(
      pipeline_rule_index_trade_financial,
      location_rule_index_trade_financial,
      index_rule_index_trade_financial,
      product_rule_index_trade_financial
    ),
    
    index_group = coalesce(
      pipeline_rule_index_group,
      location_rule_index_group,
      index_rule_index_group,
      product_rule_index_group
    ),
    
    key_id = coalesce(
      pipeline_rule_key_id,
      location_rule_key_id,
      index_rule_key_id,
      product_rule_key_id
    ),
    
    in_index_rule = coalesce(
      pipeline_rule_in_index,
      location_rule_in_index,
      index_rule_in_index,
      product_rule_in_index
    ),
    
    in_index_bool = coalesce(
      parse_bool_rule(in_index_rule),
      if_else(!is.na(index) & str_squish(index) != "", TRUE, FALSE)
    ),
    
    unit = case_when(
      index_trade_financial == "Financial" ~ "contracts/month",
      str_detect(index_key, "ICE") ~ "contracts/month",
      TRUE ~ "m3/day"
    )
  ) %>%
  transmute(
    `Trade ID` = trade_id,
    `Exec Date` = exec_date,
    `Exec Time` = exec_time,
    Instrument = product,
    Period = period,
    Qty = qty,
    Unit = unit,
    Price = price,
    `Price Basis` = index,
    Location = pipeline,
    Broker = broker,
    `In Index?` = in_index_bool,
    `Index/Trade/Financial` = index_trade_financial,
    `Swap/Leg/Outright` = swap_leg_outright,
    `Index Group` = index_group,
    KeyID = key_id,
    delivery_start = delivery_start,
    delivery_end = delivery_end,
    source_file = source_file
  )

# =============================================================================
# 6. Add TC_Dates and Inside Trade Cycle?
# =============================================================================

tc_dates <- read_excel(validation_path, sheet = "TC_Dates") %>%
  clean_names() %>%
  transmute(
    delivery_month = as.Date(delivery_month),
    trade_cycle_start = as.Date(trade_cycle_start),
    trade_cycle_end = as.Date(trade_cycle_end),
    period_label = period_label
  )

neon_normalized <- neon_normalized %>%
  mutate(
    period_delivery_month = coalesce(
      parse_period_month(Period),
      floor_date(delivery_start, unit = "month")
    ),
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


# =============================================================================
# Neon VWAP by trade cycle / period / index group
# =============================================================================


BBL_PER_M3 <- 6.28981077

neon_vwap_input <- neon_normalized %>%
  mutate(
    index_group_clean = str_to_upper(str_squish(as.character(`Index Group`))),
    
    price_num = as.numeric(Price),
    qty_num = as.numeric(Qty),
    
    # Neon physical quantity is already m3/month
    qty_m3_month = qty_num,
    qty_bbl_month = qty_m3_month * BBL_PER_M3
  ) %>%
  filter(
    `Inside Trade Cycle?` == TRUE,
    `In Index?` == TRUE,
    `Index/Trade/Financial` == "Trade",
    `Swap/Leg/Outright` == "Outright",
    !is.na(price_num),
    !is.na(qty_m3_month),
    qty_m3_month > 0
  )

neon_vwap <- neon_vwap_input %>%
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
    total_volume_bbl_per_month = sum(qty_bbl_month, na.rm = TRUE),
    
    vwap = sum(price_num * qty_m3_month, na.rm = TRUE) / 
      sum(qty_m3_month, na.rm = TRUE),
    
    min_price = min(price_num, na.rm = TRUE),
    max_price = max(price_num, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(period_delivery_month, `Index Group`)

neon_vwap

neon_sw_vwap <- neon_vwap_input %>%
  filter(index_group_clean == "SW") %>%
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
    total_volume_bbl_per_month = sum(qty_bbl_month, na.rm = TRUE),
    
    vwap = sum(price_num * qty_m3_month, na.rm = TRUE) / 
      sum(qty_m3_month, na.rm = TRUE),
    
    min_price = min(price_num, na.rm = TRUE),
    max_price = max(price_num, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(period_delivery_month, `Index Group`)

neon_sw_vwap