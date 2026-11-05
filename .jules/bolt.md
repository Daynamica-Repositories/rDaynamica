## 2025-05-18 - Vectorizing multi-day date sequence generation in data.table
**Learning:** Using `lapply(split(dt, by = 'id'), ...)` to expand date sequences for each record creates thousands of single-row `data.table`s, causing severe allocation and partitioning overhead.
**Action:** Use `sequence(dt$days) - 1L` combined with `rep(dt$start_date, dt$days) + day_offsets` to generate date sequence tables in a single vectorized data.table operation.

## 2025-05-19 - Replacing high-cardinality dplyr group_by arrange slice with data.table order and unique
**Learning:** Using `dplyr::group_by(user_id, start_date, loc) %>% dplyr::arrange(desc(diff)) %>% dplyr::slice(1)` across large sequence datasets (>100k rows) creates tens of thousands of individual groups and takes ~18 seconds due to dplyr group bookkeeping overhead.
**Action:** Use `data.table::setorder(dt, user_id, start_date, loc, -diff, orig_row)` and `unique(dt, by = c("user_id", "start_date", "loc"))` to deduplicate and take the top record per composite key in milliseconds (~0.05s).
