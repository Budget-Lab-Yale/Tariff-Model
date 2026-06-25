# Tariff-Rate-Tracker Output Contract Request

Audience: a `tariff-rate-tracker` Claude/Codex instance.

Goal: change the tracker output contract so downstream consumers, especially
`Tariff-Model`, do not need to load or build a 194M-row `rate_timeseries` table
just to slice one revision/date or compute small rollups.

## Problem

The current tracker build is fast in the map step and slow in gather.

Each revision snapshot is independent and is already built in parallel:

- one `snapshot_<revision>.rds` per revision
- about 4.77M rows per revision
- about 1.16 GB in memory
- about 6 MB compressed on disk

The slow part is gathering all snapshots into one dense panel:

- read/decompress all snapshots
- bind into one table, about 194.5M rows
- sort the whole table
- add interval columns
- save `rate_timeseries.rds`
- optionally serialize the whole table again as parquet
- downstream then re-walks the giant table for daily aggregates and weighted ETRs

That dense stack is not conceptually required. Revisions do not need to see all
other revisions. Cross-revision information is tiny: revision order, validity
intervals, and optionally adjacent diffs.

## Requested Contract

Please publish a revision-partitioned output alongside, or eventually instead of,
the monolithic `rate_timeseries.rds`.

The core contract should let a downstream consumer do three things cheaply:

1. Enumerate available revisions and validity windows.
2. Load exactly one active revision snapshot for a requested date.
3. Load only the columns needed for aggregation.

## Preferred Output Layout

Use this layout for both `actual` and scenarios:

```text
<root>/<vintage>/actual/
  timeseries/
    manifest.csv
    manifest.parquet                 # optional but preferred
    snapshots/
      revision=<revision_id>/
        rates.parquet

<root>/<vintage>/scenarios/<scenario_name>/
  timeseries/
    manifest.csv
    manifest.parquet                 # optional but preferred
    snapshots/
      revision=<revision_id>/
        rates.parquet
```

Keep the old file during transition if needed:

```text
rate_timeseries.rds
rate_timeseries.parquet
```

But downstream should no longer be required to read it.

## `manifest` Schema

Required columns:

```text
revision          character
effective_date    Date
valid_from        Date
valid_until       Date
interval_end      character
path              character
n_rows            integer
```

Rules:

- `path` is relative to the `timeseries/` directory.
- `valid_from` and `valid_until` must define the active window for that revision.
- Declare date convention explicitly in `interval_end`.
- Preferred convention is `exclusive`, meaning `[valid_from, valid_until)`.
- If tracker keeps the current inclusive convention, set `interval_end = "inclusive"`.
- Final revision should extend to the configured series horizon, not `Sys.Date()`.

Example:

```csv
revision,effective_date,valid_from,valid_until,interval_end,path,n_rows
rev_20,2025-08-20,2025-08-20,2025-08-28,exclusive,snapshots/revision=rev_20/rates.parquet,4770000
```

## `rates.parquet` Schema

Required columns:

```text
hts10                 character
country               character
total_rate            double
base_rate             double
total_additional      double
rate_232              double
rate_301              double
rate_ieepa_recip      double
rate_ieepa_fent       double
rate_s122             double
rate_section_201      double
rate_other            double
usmca_eligible        logical
metal_share           double
```

Nice-to-have columns:

```text
revision              character
effective_date        Date
```

Those two are redundant with the manifest, but including them makes ad hoc
inspection easier. Do not include `valid_from` / `valid_until` on every row unless
there is a strong reason; they are revision metadata and belong in the manifest.

Column naming note:

- `country` is acceptable because that is what tracker uses today.
- `Tariff-Model` can normalize `country` to `cty_code`.
- `total_rate` is acceptable because `Tariff-Model` can normalize it to
  `rate_total`.

## Tariff-Model Consumer Semantics

`Tariff-Model` needs to support these operations:

1. Resolve the tracker bundle:

```text
root + vintage + actual/scenarios/<scenario> + timeseries
```

2. Read `manifest`.

3. For a requested date, identify the active revision:

```text
valid_from <= date < valid_until      # if interval_end == "exclusive"
valid_from <= date <= valid_until     # if interval_end == "inclusive"
```

4. Read only that revision's `rates.parquet`.

5. Select only necessary columns, usually:

```text
hts10, country, total_rate
```

or, for authority decomposition:

```text
hts10, country, total_rate, rate_232, rate_301, rate_ieepa_recip,
rate_ieepa_fent, rate_s122, rate_section_201, rate_other
```

This avoids materializing all revisions in memory.

## Strong Recommendation: Pre-Aggregate During Map

The fastest design is not just partitioned snapshots. The map task should also
write small aggregate artifacts for each revision, because downstream daily and
ETR outputs already operate one revision at a time.

Recommended per-revision outputs:

```text
aggregates/
  revision=<revision_id>/
    overall.csv
    by_country.csv
    by_authority.csv
    by_gtap.csv
```

These can be gathered by binding a few thousand rows instead of re-walking the
dense `hts10 x country` panel.

This is especially valuable for tracker-owned daily/ETR outputs. It is less
important for `Tariff-Model`, which mostly needs a date snapshot to roll rates
to GTAP/BEA using its own import weights.

## Optional Long-Term Contract: Change Log

A more aggressive output is an event/change log:

```text
rate_events.parquet
```

Schema:

```text
hts10
country
valid_from
valid_until
total_rate
base_rate
total_additional
rate_*
change_type       # add, update, delete/tombstone
```

This would store one row per changed rate interval instead of repeating every
unchanged `(hts10, country)` pair in every revision.

This is likely the right long-term storage model, but it is a larger contract
change. If implemented, it must also provide a tested reader that reconstructs
the full active snapshot for any date. Be explicit about tombstones or default
zero-rate behavior for dropped keys.

Do not make this the first step unless there is time to update every consumer.
The lower-risk first step is the manifest plus partitioned full snapshots.

## Implementation Notes

Replace the gather behavior that currently does this:

```text
read all snapshots -> rbindlist -> sort -> add intervals -> save one huge table
```

with this:

```text
compute manifest from revision_dates.csv
for each snapshot:
  read one snapshot
  enforce schema
  select stable output columns
  write snapshots/revision=<revision>/rates.parquet
  optionally write per-revision aggregates
  drop snapshot from memory
write manifest
```

There is no need to sort 194M rows globally. Ordering is already represented by
the manifest.

If parquet is unavailable, use one of these fallbacks:

- `rates.rds` under each revision partition
- `rates.csv.gz` under each revision partition

But parquet is preferred because downstream can project columns and filter
partitions without loading unnecessary data.

## Backward Compatibility

During rollout, publish both:

1. New partitioned contract:

```text
manifest.csv
snapshots/revision=<revision>/rates.parquet
```

2. Legacy dense contract:

```text
rate_timeseries.rds
rate_timeseries.parquet
```

Once `Tariff-Model` and other consumers are updated, the dense contract can
become optional or release-only.

## Acceptance Criteria

1. A downstream script can read `manifest.csv`, select one date, and load exactly
   one revision snapshot.

2. Peak gather memory should be approximately one snapshot plus writer overhead,
   not all snapshots.

3. No global sort of all snapshot rows is required.

4. Existing `snapshot_<revision>.rds` values and new `rates.parquet` values match
   for the required columns on at least two representative revisions.

5. `manifest` intervals are gapless and ordered.

6. Final revision uses the configured horizon end.

7. `Tariff-Model` can consume:

```text
actual/timeseries/manifest.csv
actual/timeseries/snapshots/revision=<revision>/rates.parquet
scenarios/<scenario>/timeseries/manifest.csv
scenarios/<scenario>/timeseries/snapshots/revision=<revision>/rates.parquet
```

## Suggested Message to Downstream Consumers

The tracker now publishes a revision-partitioned rate panel. Consumers should
read `manifest.csv` to resolve the active revision for a date, then read that
revision's `rates.parquet`. The legacy dense `rate_timeseries.rds` is retained
temporarily but should no longer be used for date slicing or aggregation.
