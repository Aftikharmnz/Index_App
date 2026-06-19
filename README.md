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

## Moving to another machine

Paths **auto-resolve** — there are no hard-coded user folders in `R/config.R`. The app
finds the broker data whether the `Price Index excel sheets` folder sits **inside** the
project (e.g. `Index_App/Index_App/Price Index excel sheets/…`) or **beside** it, with
single or double nesting. It locates the folder by looking for one that contains the
`ICE / Modern / Neon / OneX` subfolders. So to move machines:

1. Copy the whole project folder (with the data and `broker_classification_validation.xlsx`).
2. Install R 4.5+ and the packages above.
3. From the app folder run `shiny::runApp('.')` — that's it.

If your data lives somewhere unusual, force it in your R session before launching:

```r
options(indexapp.data_dir = "C:/path/to/folder that holds ICE,Modern,Neon,OneX")
options(indexapp.workbook = "C:/path/to/broker_classification_validation.xlsx")
```

> The cache and `build_qa.rds` always live under `data/processed/cache/` inside the app,
> so they travel with it. Re-run **"Rebuild from broker files"** once on the new machine
> if the data changed. The only file that still holds absolute paths is
> `.claude/launch.json` (the optional launcher) — edit its R-exe and app paths, or just
> use `shiny::runApp('.')`.

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

* Paths auto-resolve (data found inside **or** beside the project; no hard-coded user folder) — see "Moving to another machine".
* File reads are wrapped so one bad file can't abort a broker's whole run.
* Trade-cycle flag standardized to `in_trade_cycle` across the pipeline.
* All four brokers share one schema; real `Location` is preserved (Edmonton/Kerrobert/…).
* ICE execution time uses **LOCAL DATETIME** (local Edmonton), not the UTC `DEAL TIME`.
