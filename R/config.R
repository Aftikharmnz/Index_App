# =============================================================================
# R/config.R
# Central configuration: paths, constants, broker list, index definitions.
# Edit INDEXAPP_ROOT (or set options(indexapp.root=...)) if you move the project.
# =============================================================================

# ---- Locate the project root (the "appproject" folder that holds the data) ----
.find_root <- function() {
  explicit <- getOption("indexapp.root", default = NA_character_)
  if (!is.na(explicit) && dir.exists(explicit)) return(normalizePath(explicit, winslash = "/"))

  d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  for (i in seq_len(7)) {
    if (dir.exists(file.path(d, "Price Index excel sheets"))) return(d)
    parent <- dirname(d)
    if (identical(parent, d)) break
    d <- parent
  }
  # Fallback to the known location on this machine.
  "C:/Users/aftik/Documents/Projects/index_app_2/appproject"
}

INDEXAPP_ROOT <- .find_root()

# ---- Key paths ----
DATA_DIR <- file.path(INDEXAPP_ROOT, "Price Index excel sheets", "Price Index excel sheets")

WORKBOOK <- file.path(
  INDEXAPP_ROOT, "Index_App", "Index_App", "data", "reference",
  "classification_validation", "broker_classification_validation.xlsx"
)
if (!file.exists(WORKBOOK)) {
  WORKBOOK <- file.path(INDEXAPP_ROOT, "broker_classification_validation.xlsx")
}

CACHE_DIR <- file.path(INDEXAPP_ROOT, "Index_App", "Index_App", "data", "processed", "cache")
CACHE_FILE <- file.path(CACHE_DIR, "combined_normalized.parquet")
QA_FILE    <- file.path(CACHE_DIR, "build_qa.rds")

# ---- Constants ----
BBL_PER_M3 <- 6.28981077
LOCAL_TZ   <- "America/Edmonton"

# ---- Brokers ----
BROKERS <- c("ICE", "Modern", "Neon", "OneX")
BROKER_FOLDER <- c(ICE = "ICE", Modern = "Modern", Neon = "Neon", OneX = "OneX")
BROKER_RULE_SHEET <- c(ICE = "ICE_Rules", Modern = "Modern_Rules",
                       Neon = "Marex_Rules", OneX = "OneX_Rules")

# ---- Index groups currently in scope (extensible: add "SYN","WCS","CONDENSATE") ----
ACTIVE_INDEX_GROUPS <- c("SW")

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
