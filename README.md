# Canadian Crude Price Index — Sweet (SW)

Interactive Shiny app that reconstructs the **sweet crude (SW)** price indices from
day-to-day broker trades (ICE, Modern/Modcom, Neon/Marex, OneX), and shows how each
monthly index is priced in over its trade cycle.

* **comm index** = volume-weighted average of **all 4 brokers**' physical outright trades.
* **1a index** = volume-weighted average of **ICE + OneX** (configurable).
* Index value = VWAP of the differential to **WTI CMA** ($/bbl), m³-weighted, using only
  trades executed **inside the trade cycle** for that delivery month.

## How to run

R 4.5+ with: `shiny bslib plotly DT dplyr tidyr stringr lubridate readxl readr janitor purrr tibble arrow scales`.

```r
# from this folder (Index_App/Index_App)
shiny::runApp()
```

The app reads a cached dataset at `data/processed/cache/combined_normalized.parquet`.
Drop new broker files into `Price Index excel sheets/Price Index excel sheets/<Broker>/`
and click **"Rebuild from broker files"** in the sidebar (or delete the cache and reload).

## Tabs

| Tab | Shows |
|-----|-------|
| **Trade Cycle Monitor** | Pick a delivery month → KPI boxes, the **accumulated index developing through the cycle** (daily volume + cumulative VWAP), broker & grade contribution, volume by hour of day |
| **Indices Overview** | Monthly index history (comm vs 1a), downloadable table |
| **Index-Referenced Trades** | The `Index`-classified rows = trades executed **at** a published index (comm / 1a / Bi4 / XAPP). Price is flat to the index, so this shows **at-index volume & participation** by month, broker and grade, with a trade table |
| **Historical Analysis** | In-cycle vs out-of-cycle volume & average price; hour-of-day profile across all history |
| **Brokers & Grades** | Per-broker VWAP dispersion, broker volume share, grade volume mix over time |
| **Data & QA** | Coverage by broker, filterable/downloadable normalized trades |

## Code layout

```
app.R                      Shiny UI + server
R/config.R                 paths, constants, brokers, INDEX_DEFINITIONS (comm / 1a)
R/helpers.R                shared parsing/cleaning + safe file reader + shared schema
R/load_reference.R         reads classification workbook: TC_Dates + per-broker rules
R/normalize_{ice,modern,neon,onex}.R   each broker -> shared 23-column schema
R/build_dataset.R          binds brokers, flags component trades, caches parquet
R/indices.R                VWAP / index / contribution / historic analytics
R/plots.R                  plotly chart builders + formatters
```

## Extending to SYN / WCS / Condensate

1. Add the rules for the new products to `broker_classification_validation.xlsx`
   (set `Index_Group` to e.g. `SYN`, `WCS`, `CONDENSATE` and a `KeyID`/grade).
2. Add the group to `ACTIVE_INDEX_GROUPS` in `R/config.R`.
3. Rebuild. The "Index group" selector and all tabs pick it up automatically.

To change the **1a** broker set, edit `INDEX_DEFINITIONS$\`1a\`` in `R/config.R`.

## Notes / fixes applied vs. the original `scripts_02`

* Paths are now project-relative (auto-detected) instead of a hard-coded OneDrive path.
* File reads are wrapped so one bad file can't abort a broker's whole run.
* Trade-cycle flag standardized to `in_trade_cycle` across the pipeline.
* All four brokers share one schema; real `Location` is preserved (Edmonton/Kerrobert/…).
* ICE execution time uses **LOCAL DATETIME** (local Edmonton), not the UTC `DEAL TIME`.
