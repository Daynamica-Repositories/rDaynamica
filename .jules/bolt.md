## 2025-05-18 - Vectorizing multi-day date sequence generation in data.table
**Learning:** Using `lapply(split(dt, by = 'id'), ...)` to expand date sequences for each record creates thousands of single-row `data.table`s, causing severe allocation and partitioning overhead.
**Action:** Use `sequence(dt$days) - 1L` combined with `rep(dt$start_date, dt$days) + day_offsets` to generate date sequence tables in a single vectorized data.table operation.

## 2025-05-19 - Replacing high-cardinality dplyr group_by arrange slice with data.table order and unique
**Learning:** Using `dplyr::group_by(user_id, start_date, loc) %>% dplyr::arrange(desc(diff)) %>% dplyr::slice(1)` across large sequence datasets (>100k rows) creates tens of thousands of individual groups and takes ~18 seconds due to dplyr group bookkeeping overhead.
**Action:** Use `data.table::setorder(dt, user_id, start_date, loc, -diff, orig_row)` and `unique(dt, by = c("user_id", "start_date", "loc"))` to deduplicate and take the top record per composite key in milliseconds (~0.05s).

## 2026-11-05 - Multi-pass .SD grouping and which.max on high-cardinality groups in data.table
**Learning:** Running `dt[, .I[which.max(...)], by = ...]` and `lapply(cols, function(col) dt[, .(paste(.SD[[1]], ...)), by = ..., .SDcols = col])` repeatedly across >20,000 groups in data.table incurs huge per-group overhead and requires multi-table `Reduce(merge, ...)`.
**Action:** Use `setorder` + `unique(dt, by = ...)` for selecting optimal records per group, combine all aggregated and collapsed columns into a single `by =` grouping pass, and merge the resulting tables directly.

## 2026-11-09 - Operating on unique string values for low-cardinality columns in survey data
**Learning:** Applying string regex functions like `gsub` and `trimws` across tens of thousands of rows for columns with low cardinality (such as `question_text`, which only has ~18 distinct values across >32,000 survey rows) creates massive CPU overhead (~0.16s per survey tidy call).
**Action:** Always compute string cleaning operations on `unique()` values first and map the cleaned strings back to all rows using vector indexing `cleaned[match(col, u_col)]` (~0.001s, >100x faster).
