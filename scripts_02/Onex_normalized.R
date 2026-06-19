# =============================================================================
# OneX Normalization Script
# Output columns match modern_normalized exactly
# =============================================================================

library(dplyr)
library(purrr)
library(readr)
library(readxl)
library(janitor)
library(stringr)
library(lubridate)
library(hms)

# -----------------------------
# Paths
# -----------------------------

onex_folder_path <- "C:/Users/amominzada/OneDrive - Energy Transfer/Documents/Excel sheets/Price Index excel sheets/OneX"

validation_path <- "C:/Users/amominzada/OneDrive - Energy Transfer/Documents/Projects/Index_App/Index_App/data/reference/classification_validation/broker_classification_validation.xlsx"

broker_name <- "OneX"
rules_sheet <- "OneX_Rules"

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

normalize_key <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("[\u2010\u2011\u2012\u2013\u2014\u2015]", "-") %>%
    str_squish() %>%
    str_to_upper()
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

parse_onex_date <- function(x) {
  
  x_chr <- str_squish(as.character(x))
  x_chr[x_chr == ""] <- NA_character_
  
  out <- rep(as.Date(NA), length(x_chr))
  
  # Normal date strings
  date_idx <- which(
    !is.na(x_chr) &
      !str_detect(x_chr, "^\\d+(\\.\\d+)?$")
  )
  
  if (length(date_idx) > 0) {
    parsed <- suppressWarnings(
      parse_date_time(
        x_chr[date_idx],
        orders = c(
          "ymd",
          "mdy",
          "dmy",
          "Ymd",
          "mdY",
          "dmY"
        ),
        tz = "UTC"
      )
    )
    
    out[date_idx] <- as.Date(parsed)
  }
  
  # Excel serial dates
  serial_idx <- which(
    is.na(out) &
      !is.na(x_chr) &
      str_detect(x_chr, "^\\d+(\\.\\d+)?$")
  )
  
  if (length(serial_idx) > 0) {
    out[serial_idx] <- as.Date(
      as.numeric(x_chr[serial_idx]),
      origin = "1899-12-30"
    )
  }
  
  out
}

parse_onex_time <- function(x) {
  x_chr <- str_squish(as.character(x))
  
  parsed <- suppressWarnings(
    parse_date_time(
      x_chr,
      orders = c(
        "I:M:S p",
        "I:M p",
        "H:M:S",
        "H:M"
      ),
      tz = "UTC"
    )
  )
  
  hms::as_hms(parsed)
}

parse_onex_period_month <- function(x) {
  
  x_chr <- str_squish(as.character(x))
  
  parsed <- suppressWarnings(
    parse_date_time(
      paste("01", x_chr),
      orders = c(
        "d b y",
        "d B y",
        "d b Y",
        "d B Y"
      ),
      locale = "C",
      tz = "UTC"
    )
  )
  
  as.Date(floor_date(parsed, "month"))
}

parse_bool_yn <- function(x) {
  x_chr <- str_to_upper(str_squish(as.character(x)))
  
  case_when(
    x_chr %in% c("TRUE", "YES", "Y", "1", "T") ~ TRUE,
    x_chr %in% c("FALSE", "NO", "N", "0", "F") ~ FALSE,
    TRUE ~ NA
  )
}

# =============================================================================
# 2. Read OneX raw files
# =============================================================================

onex_files <- list.files(
  path = onex_folder_path,
  pattern = "\\.(xlsx|xls|csv)$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(onex_files) == 0) {
  stop("No Excel or CSV files found in the OneX folder path.")
}

read_onex_one_file <- function(file) {
  
  ext <- str_to_lower(tools::file_ext(file))
  
  if (ext == "csv") {
    
    read_csv(
      file,
      col_types = cols(.default = col_character()),
      show_col_types = FALSE
    ) %>%
      clean_names()
    
  } else {
    
    sheets <- excel_sheets(file)
    
    map_dfr(sheets, function(sheet_name) {
      
      read_excel(
        file,
        sheet = sheet_name,
        col_types = "text",
        .name_repair = "unique"
      ) %>%
        clean_names() %>%
        mutate(source_sheet = sheet_name)
    })
  }
}

onex_raw_all <- map_dfr(onex_files, function(file) {
  
  read_onex_one_file(file) %>%
    filter(if_any(everything(), ~ !is.na(.x) & str_squish(as.character(.x)) != "")) %>%
    mutate(
      broker = broker_name,
      source_file = basename(file),
      source_file_mtime = file.info(file)$mtime
    )
})

# =============================================================================
# 3. Clean raw OneX fields
# Actual OneX raw columns after clean_names():
# trade_id, exec_date, exec_time, instrument, period, qty, unit, price,
# location, pipeline, buyer_pays, buyer_receives, swap_trade,
# cancelled, sleeved, is_financial
# =============================================================================

onex_raw_all <- onex_raw_all %>%
  add_missing_cols(c(
    "trade_id",
    "exec_date",
    "exec_time",
    "instrument",
    "period",
    "qty",
    "unit",
    "price",
    "location",
    "pipeline",
    "buyer_pays",
    "buyer_receives",
    "swap_trade",
    "cancelled",
    "sleeved",
    "is_financial",
    "source_sheet"
  )) %>%
  mutate(
    trade_id = str_squish(as.character(trade_id)),
    trade_id = na_if(trade_id, ""),
    
    instrument = str_squish(as.character(instrument)),
    period = str_squish(as.character(period)),
    unit = str_squish(as.character(unit)),
    buyer_pays = str_squish(as.character(buyer_pays)),
    pipeline = str_squish(as.character(pipeline)),
    swap_trade = str_squish(as.character(swap_trade)),
    
    qty = parse_number_clean(qty),
    price = parse_number_clean(price),
    
    exec_date_clean = parse_onex_date(exec_date),
    exec_time_clean = parse_onex_time(exec_time),
    period_delivery_month = parse_onex_period_month(period),
    
    cancelled_bool = parse_bool_yn(cancelled),
    sleeved_bool = parse_bool_yn(sleeved),
    is_financial_bool = parse_bool_yn(is_financial),
    
    instrument_key = normalize_key(instrument),
    
    source_export_start = str_match(source_file, "(\\d{4}-\\d{2}-\\d{2})_(\\d{4}-\\d{2}-\\d{2})")[, 2],
    source_export_end   = str_match(source_file, "(\\d{4}-\\d{2}-\\d{2})_(\\d{4}-\\d{2}-\\d{2})")[, 3],
    source_export_start = ymd(source_export_start),
    source_export_end   = ymd(source_export_end),
    
    dedupe_key = if_else(
      is.na(trade_id) | trade_id == "",
      paste0(source_file, "_", source_sheet, "_row_", row_number()),
      trade_id
    )
  )

# =============================================================================
# 4. De-duplicate
# Keep newest export version of each Trade ID
# =============================================================================

onex_duplicate_check <- onex_raw_all %>%
  count(trade_id, name = "duplicate_count") %>%
  filter(!is.na(trade_id), duplicate_count > 1) %>%
  arrange(desc(duplicate_count), trade_id)

onex_duplicate_summary <- tibble(
  rows_before_dedup = nrow(onex_raw_all),
  unique_trade_ids = n_distinct(onex_raw_all$trade_id, na.rm = TRUE),
  duplicated_trade_ids = nrow(onex_duplicate_check),
  duplicate_rows_to_remove = nrow(onex_raw_all) - n_distinct(onex_raw_all$dedupe_key)
)

onex_duplicate_summary

onex_deduped <- onex_raw_all %>%
  arrange(
    desc(source_export_end),
    desc(source_file_mtime),
    desc(source_file),
    desc(source_sheet)
  ) %>%
  distinct(dedupe_key, .keep_all = TRUE)

# =============================================================================
# 4B. Remove cancelled trades and duplicate sleeve/economic copies
# Generalized OneX rule:
# - Keep non-cancelled trades
# - If multiple rows have the same economics and at least one row is sleeved,
#   keep only one row
# - Prefer non-sleeved row if available
# - If all are sleeved, keep the lowest Trade ID
# =============================================================================

onex_clean_base <- onex_deduped %>%
  mutate(
    cancelled_flag = coalesce(cancelled_bool, FALSE),
    sleeved_flag = coalesce(sleeved_bool, FALSE),
    trade_id_num = suppressWarnings(as.numeric(trade_id)),
    
    economic_match_key = paste(
      exec_date_clean,
      as.character(exec_time_clean),
      normalize_key(instrument),
      normalize_key(period),
      qty,
      normalize_key(unit),
      price,
      normalize_key(pipeline),
      normalize_key(buyer_pays),
      normalize_key(buyer_receives),
      normalize_key(swap_trade),
      is_financial_bool,
      sep = " | "
    )
  ) %>%
  filter(cancelled_flag == FALSE)

onex_clean_ranked <- onex_clean_base %>%
  group_by(economic_match_key) %>%
  mutate(
    economic_row_count = n(),
    group_has_sleeved_trade = any(sleeved_flag == TRUE, na.rm = TRUE),
    
    duplicate_sleeve_candidate = economic_row_count > 1 & group_has_sleeved_trade == TRUE
  ) %>%
  arrange(
    economic_match_key,
    sleeved_flag,       # FALSE first, TRUE second
    trade_id_num        # then lowest trade_id
  ) %>%
  mutate(
    economic_keep_rank = row_number(),
    
    drop_as_duplicate_economic_copy =
      duplicate_sleeve_candidate == TRUE &
      economic_keep_rank > 1
  ) %>%
  ungroup()

# Audit what gets dropped
onex_dropped_duplicate_economics <- onex_clean_ranked %>%
  filter(drop_as_duplicate_economic_copy == TRUE) %>%
  select(
    trade_id,
    exec_date,
    exec_time,
    instrument,
    period,
    qty,
    unit,
    price,
    pipeline,
    buyer_pays,
    buyer_receives,
    cancelled,
    sleeved,
    swap_trade,
    is_financial,
    economic_row_count,
    group_has_sleeved_trade,
    economic_keep_rank,
    source_file
  ) %>%
  arrange(exec_date, exec_time, instrument, period, price, qty, trade_id)

onex_dropped_duplicate_economics

# Clean data used going forward
onex_clean <- onex_clean_ranked %>%
  filter(drop_as_duplicate_economic_copy == FALSE) %>%
  select(
    -economic_row_count,
    -group_has_sleeved_trade,
    -duplicate_sleeve_candidate,
    -economic_keep_rank,
    -drop_as_duplicate_economic_copy
  )


# =============================================================================
# 5. Read OneX classification rules
# =============================================================================

available_sheets <- excel_sheets(validation_path)

if (!(rules_sheet %in% available_sheets)) {
  stop(paste0("Sheet '", rules_sheet, "' does not exist in validation workbook."))
}

onex_rules <- read_excel(validation_path, sheet = rules_sheet) %>%
  clean_names() %>%
  add_missing_cols(c(
    "active",
    "raw_product",
    "y_n",
    "index_trade_financial",
    "index_group",
    "key_id",
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
    
    instrument_key = normalize_key(raw_product)
  ) %>%
  filter(
    active_bool == TRUE,
    !is.na(instrument_key),
    instrument_key != ""
  ) %>%
  arrange(rule_priority) %>%
  distinct(instrument_key, .keep_all = TRUE) %>%
  transmute(
    instrument_key,
    rule_in_index = y_n,
    rule_index_trade_financial = index_trade_financial,
    rule_index_group = index_group,
    rule_key_id = key_id
  )

# =============================================================================
# 6. Normalize OneX into same columns as modern_normalized
# =============================================================================

onex_normalized <- onex_clean %>%
  left_join(
    onex_rules,
    by = "instrument_key"
  ) %>%
  mutate(
    in_index_bool = parse_bool_yn(rule_in_index),
    
    swap_trade_clean = str_to_upper(str_squish(as.character(swap_trade))),
    
    swap_leg_outright = case_when(
      swap_trade_clean == "PARENT" ~ "Swap",
      swap_trade_clean == "LEG" ~ "Leg",
      TRUE ~ "Outright"
    )
  ) %>%
  transmute(
    `Trade ID` = trade_id,
    `Exec Date` = exec_date_clean,
    `Exec Time` = as.character(exec_time_clean),
    Instrument = instrument,
    Period = period,
    Qty = qty,
    Unit = unit,
    Price = price,
    `Price Basis` = buyer_pays,
    Location = pipeline,
    Broker = broker,
    `In Index?` = in_index_bool,
    `Index/Trade/Financial` = rule_index_trade_financial,
    `Swap/Leg/Outright` = swap_leg_outright,
    `Index Group` = rule_index_group,
    KeyID = rule_key_id,
    period_delivery_month = period_delivery_month,
    source_file = source_file
  )

# =============================================================================
# 7. Add TC_Dates from validation workbook
# =============================================================================

tc_dates <- read_excel(validation_path, sheet = "TC_Dates") %>%
  clean_names() %>%
  transmute(
    delivery_month = as.Date(delivery_month),
    trade_cycle_start = as.Date(trade_cycle_start),
    trade_cycle_end = as.Date(trade_cycle_end),
    period_label = period_label
  )

onex_normalized <- onex_normalized %>%
  left_join(
    tc_dates,
    by = c("period_delivery_month" = "delivery_month")
  ) %>%
  mutate(
    `Inside Trade Cycle?` = case_when(
      !is.na(`Exec Date`) &
        !is.na(trade_cycle_start) &
        !is.na(trade_cycle_end) &
        `Exec Date` >= trade_cycle_start &
        `Exec Date` <= trade_cycle_end ~ TRUE,
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
# 8. Checks
# =============================================================================

onex_normalized

colnames(onex_normalized)

onex_summary_check <- onex_normalized %>%
  summarise(
    rows = n(),
    missing_trade_id = sum(is.na(`Trade ID`) | `Trade ID` == ""),
    missing_exec_date = sum(is.na(`Exec Date`)),
    missing_exec_time = sum(is.na(`Exec Time`)),
    missing_period_delivery_month = sum(is.na(period_delivery_month)),
    missing_in_index = sum(is.na(`In Index?`)),
    missing_classification = sum(is.na(`Index/Trade/Financial`)),
    missing_index_group = sum(is.na(`Index Group`)),
    missing_key_id = sum(is.na(KeyID)),
    outside_trade_cycle = sum(`Inside Trade Cycle?` == FALSE, na.rm = TRUE)
  )

onex_summary_check

onex_unmatched_rules <- onex_normalized %>%
  filter(
    is.na(`In Index?`) |
      is.na(`Index/Trade/Financial`) |
      is.na(`Index Group`) |
      is.na(KeyID)
  ) %>%
  distinct(
    Instrument,
    `Price Basis`,
    Location,
    Period,
    `In Index?`,
    `Index/Trade/Financial`,
    `Swap/Leg/Outright`,
    `Index Group`,
    KeyID
  ) %>%
  arrange(Instrument, `Price Basis`, Location)

onex_unmatched_rules





# =============================================================================
# OneX SW VWAP only
# =============================================================================



BBL_PER_M3 <- 6.28981077

onex_sw_vwap_input <- onex_normalized %>%
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

onex_sw_vwap <- onex_sw_vwap_input %>%
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

onex_sw_vwap
