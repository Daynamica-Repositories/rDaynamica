## 2025-05-18 - Vectorizing multi-day date sequence generation in data.table
**Learning:** Using `lapply(split(dt, by = 'id'), ...)` to expand date sequences for each record creates thousands of single-row `data.table`s, causing severe allocation and partitioning overhead.
**Action:** Use `sequence(dt$days) - 1L` combined with `rep(dt$start_date, dt$days) + day_offsets` to generate date sequence tables in a single vectorized data.table operation.
