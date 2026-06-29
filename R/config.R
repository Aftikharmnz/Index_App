# =============================================================================
# R/config.R
# Central configuration: paths, constants, broker list, index definitions.
# The data folder MUST live INSIDE the app folder (Price Index excel sheets/,
# with or without one extra nesting level). The search never walks outside the
# project, so a stray folder above the app can't shadow the real data.
# To force a location, set:
#   options(indexapp.data_dir = ".../folder that holds ICE,Modern,Neon,OneX")
#   options(indexapp.workbook = ".../broker_classification_validation.xlsx")
# =============================================================================

# ---- Locate the app directory (the folder holding app.R + R/) ----
.find_app_dir <- function() {
  d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  for (i in seq_len(8)) {
    if (file.exists(file.path(d, "app.R")) && dir.exists(file.path(d, "R"))) return(d)
    parent <- dirname(d)
    if (identical(parent, d)) break
    d <- parent
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}
APP_DIR <- getOption("indexapp.app_dir", default = .find_app_dir())

# A directory "is" the broker-data dir when it directly holds the broker folders.
.BROKER_DIRS <- c("ICE", "Modern", "Neon", "OneX")
.is_data_dir <- function(p) dir.exists(p) && any(dir.exists(file.path(p, .BROKER_DIRS)))

# ---- Locate the broker-data directory. Search ONLY inside the app folder.
#      Handles double-nested "Price Index excel sheets/Price Index excel sheets",
#      single-level "Price Index excel sheets", or brokers placed directly in
#      the app or in app/data. Never walks above APP_DIR. ----
.find_data_dir <- function() {
  opt <- getOption("indexapp.data_dir", default = NA_character_)
  if (!is.na(opt) && .is_data_dir(opt)) return(normalizePath(opt, winslash = "/"))
  pin   <- "Price Index excel sheets"
  bases <- c(APP_DIR, file.path(APP_DIR, "data"))
  cands <- unlist(lapply(bases, function(b) c(file.path(b, pin, pin), file.path(b, pin), b)))
  hit   <- Find(.is_data_dir, cands)
  if (!is.null(hit)) return(normalizePath(hit, winslash = "/"))
  file.path(APP_DIR, pin, pin)  # sensible default for messages
}
DATA_DIR      <- .find_data_dir()
INDEXAPP_ROOT <- APP_DIR  # the project IS the app folder — no outside roots

# ---- Locate the classification workbook. Search ONLY inside the app folder. ----
.find_workbook <- function() {
  opt <- getOption("indexapp.workbook", default = NA_character_)
  if (!is.na(opt) && file.exists(opt)) return(normalizePath(opt, winslash = "/"))
  cands <- c(
    file.path(APP_DIR, "data", "reference", "classification_validation",
              "broker_classification_validation.xlsx"),
    file.path(APP_DIR, "broker_classification_validation.xlsx"),
    file.path(DATA_DIR, "broker_classification_validation.xlsx"),
    file.path(dirname(DATA_DIR), "broker_classification_validation.xlsx")
  )
  hit <- Find(file.exists, cands)
  if (!is.null(hit)) return(normalizePath(hit, winslash = "/"))
  cands[1]
}
WORKBOOK <- .find_workbook()

# ---- Cache always lives inside the app's data folder (writable, travels w/ app) ----
CACHE_DIR  <- file.path(APP_DIR, "data", "processed", "cache")
CACHE_FILE <- file.path(CACHE_DIR, "combined_normalized.parquet")
QA_FILE    <- file.path(CACHE_DIR, "build_qa.rds")

# ---- Non-fatal heads-up if the broker data wasn't found anywhere ----
if (!.is_data_dir(DATA_DIR))
  warning("Broker data (folders ICE/Modern/Neon/OneX) not found near the app at '",
          APP_DIR, "'. Set options(indexapp.data_dir='<path to that folder>').",
          call. = FALSE)

# ---- Constants ----
BBL_PER_M3 <- 6.28981077
LOCAL_TZ   <- "America/Edmonton"

# ---- Brokers ----
BROKERS <- c("ICE", "Modern", "Neon", "OneX")
BROKER_FOLDER <- c(ICE = "ICE", Modern = "Modern", Neon = "Neon", OneX = "OneX")
BROKER_RULE_SHEET <- c(ICE = "ICE_Rules", Modern = "Modern_Rules",
                       Neon = "Marex_Rules", OneX = "OneX_Rules")

# ---- Index groups currently in scope (extensible: add "SYN","WCS","CONDENSATE") ----
ACTIVE_INDEX_GROUPS <- c("SW", "SYN")

# ---- Named index definitions: which brokers compose each published index ----
# comm = all four brokers; 1a = ICE + OneX (per user). Edit freely.
INDEX_DEFINITIONS <- list(
  comm = c("ICE", "Modern", "Neon", "OneX"),
  `1a`  = c("ICE", "OneX")
)

# ---- Shared normalized schema (every broker normalizer returns exactly these) ----
SCHEMA_COLS <- c(
  "broker", "trade_id", "exec_datetime", "exec_date", "exec_hour",
  "instrument", "grade", "index_group", "itf", "swap_leg_outright",
  "in_index", "period", "delivery_month", "price", "qty_m3", "unit_raw",
  "location", "pipeline", "price_basis",
  "trade_cycle_start", "trade_cycle_end", "in_trade_cycle", "source_file"
)
