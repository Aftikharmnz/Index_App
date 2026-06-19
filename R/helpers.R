# =============================================================================
# R/helpers.R
# Shared parsing / cleaning helpers used by all broker normalizers.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(readxl)
  library(readr)
  library(janitor)
  library(purrr)
  library(tibble)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

# Blank/whitespace -> NA character
blank_to_na <- function(x) {
  x <- str_squish(as.character(x))
  x[x == "" | str_to_upper(x) == "NA"] <- NA_character_
  x
}

# Upper-cased, squished key
ukey <- function(x) {
  x <- str_squish(as.character(x))
  x <- str_replace_all(x, "[‐‑‒–—―]", "-") # unicode dashes -> hyphen
  x <- str_to_upper(x)
  x[x == ""] <- NA_character_
  x
}

# Numeric, stripping commas / $ / spaces
clean_num <- function(x) {
  x <- str_replace_all(as.character(x), "[,$]", "")
  x <- str_squish(x)
  x[x == ""] <- NA_character_
  suppressWarnings(as.numeric(x))
}

# Boolean from common truthy/falsey tokens
parse_bool <- function(x) {
  x <- str_to_upper(str_squish(as.character(x)))
  case_when(
    x %in% c("TRUE", "T", "YES", "Y", "1") ~ TRUE,
    x %in% c("FALSE", "F", "NO", "N", "0") ~ FALSE,
    TRUE ~ NA
  )
}

MONTH_MAP <- c(JAN = 1, FEB = 2, MAR = 3, APR = 4, MAY = 5, JUN = 6,
               JUL = 7, AUG = 8, SEP = 9, OCT = 10, NOV = 11, DEC = 12)

# Parse a delivery-month label to first-of-month Date.
# Handles "Jul26", "JUL-26", "Jul 26", "Jul-2026", etc. (takes the FIRST month token).
parse_delivery_month <- function(x) {
  xx <- str_to_upper(str_squish(as.character(x)))
  m <- str_match(xx, "([A-Z]{3})[- ]?(\\d{2}|\\d{4})")
  mon <- unname(MONTH_MAP[m[, 2]])
  yr <- suppressWarnings(as.integer(m[, 3]))
  yr <- ifelse(is.na(yr), NA_integer_, ifelse(yr < 100, 2000L + yr, yr))
  out <- rep(as.Date(NA), length(xx))
  ok <- !is.na(mon) & !is.na(yr)
  out[ok] <- as.Date(sprintf("%04d-%02d-01", yr[ok], mon[ok]))
  out
}

# Add any missing columns (as NA character) so downstream selects never fail.
add_missing_cols <- function(df, cols) {
  for (col in cols) if (!col %in% names(df)) df[[col]] <- NA_character_
  df
}

# Robust single-file reader: returns a tibble (all text) or NULL on failure (with warning).
# This protects the pipeline from one corrupt download (e.g. Bad CRC-32) killing a broker.
safe_read_file <- function(file, prefer_sheet = NULL) {
  ext <- str_to_lower(tools::file_ext(file))
  out <- tryCatch({
    if (ext == "csv") {
      read_csv(file, col_types = cols(.default = col_character()), show_col_types = FALSE)
    } else {
      sheets <- excel_sheets(file)
      sheet <- if (!is.null(prefer_sheet) && prefer_sheet %in% sheets) prefer_sheet else sheets[1]
      read_excel(file, sheet = sheet, col_types = "text", .name_repair = "unique")
    }
  }, error = function(e) {
    warning(sprintf("Skipping unreadable file '%s': %s", basename(file), conditionMessage(e)),
            call. = FALSE)
    NULL
  })
  if (is.null(out)) return(NULL)
  out %>%
    clean_names() %>%
    mutate(source_file = basename(file),
           source_file_modified = file.info(file)$mtime)
}

# List broker data files (xlsx/xls/csv) in a folder.
list_broker_files <- function(folder) {
  if (!dir.exists(folder)) {
    warning(sprintf("Broker folder not found: %s", folder), call. = FALSE)
    return(character(0))
  }
  list.files(folder, pattern = "\\.(xlsx|xls|csv)$", full.names = TRUE, ignore.case = TRUE)
}

# Coerce any broker output to the exact shared schema (order + types).
coerce_schema <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    empty <- tibble(
      broker = character(), trade_id = character(),
      exec_datetime = as.POSIXct(character(), tz = LOCAL_TZ),
      exec_date = as.Date(character()), exec_hour = integer(),
      instrument = character(), grade = character(), index_group = character(),
      itf = character(), swap_leg_outright = character(), in_index = logical(),
      period = character(), delivery_month = as.Date(character()),
      price = numeric(), qty_m3 = numeric(), unit_raw = character(),
      location = character(), pipeline = character(), price_basis = character(),
      trade_cycle_start = as.Date(character()), trade_cycle_end = as.Date(character()),
      in_trade_cycle = logical(), source_file = character()
    )
    return(empty)
  }
  df %>%
    mutate(
      broker            = as.character(broker),
      trade_id          = as.character(trade_id),
      exec_datetime     = as.POSIXct(exec_datetime, tz = LOCAL_TZ),
      exec_date         = as.Date(exec_date),
      exec_hour         = as.integer(exec_hour),
      instrument        = as.character(instrument),
      grade             = as.character(grade),
      index_group       = as.character(index_group),
      itf               = as.character(itf),
      swap_leg_outright = as.character(swap_leg_outright),
      in_index          = as.logical(in_index),
      period            = as.character(period),
      delivery_month    = as.Date(delivery_month),
      price             = as.numeric(price),
      qty_m3            = as.numeric(qty_m3),
      unit_raw          = as.character(unit_raw),
      location          = as.character(location),
      pipeline          = as.character(pipeline),
      price_basis       = as.character(price_basis),
      trade_cycle_start = as.Date(trade_cycle_start),
      trade_cycle_end   = as.Date(trade_cycle_end),
      in_trade_cycle    = as.logical(in_trade_cycle),
      source_file       = as.character(source_file)
    ) %>%
    select(all_of(SCHEMA_COLS))
}

# Classify a reported/index-referenced row into its index type from the raw text.
# comm = all 4 brokers ; 1a = ICE+OneX ; Bi4 / XAPP = ICE published variants.
derive_index_type <- function(instrument, price_basis) {
  t <- str_to_upper(paste(coalesce(instrument, ""), coalesce(price_basis, "")))
  case_when(
    str_detect(t, "BI4") ~ "Bi4",
    str_detect(t, "XAPP") ~ "XAPP",
    str_detect(t, "COMM") ~ "comm",
    str_detect(t, "1A") ~ "1a",
    TRUE ~ "other"
  )
}

# Attach trade-cycle window + in/out flag using TC_Dates (joined on delivery_month).
attach_trade_cycle <- function(df, tc_dates) {
  df %>%
    left_join(tc_dates, by = "delivery_month") %>%
    mutate(
      in_trade_cycle = case_when(
        is.na(exec_date) | is.na(trade_cycle_start) | is.na(trade_cycle_end) ~ NA,
        exec_date >= trade_cycle_start & exec_date <= trade_cycle_end ~ TRUE,
        TRUE ~ FALSE
      )
    )
}
