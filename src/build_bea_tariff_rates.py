#!/usr/bin/env python
# =============================================================================
# build_bea_tariff_rates.py
# =============================================================================
# Turns the tariff-rate-tracker rate panel into average tariff rates by BEA
# commodity over Apr-Dec 2025, then runs them through the import-content matrix
# (built by src/build_import_content_matrix.R) to get consumer price impact by
# CEX category.
#
# Method
#   - The tracker stores rates as step-function snapshots keyed by valid_from.
#     Each snapshot's rates.parquet is the FULL (hts10 x country) rate panel
#     valid from that date until the next snapshot.
#   - Window-average rate per (hts10, country) = sum_s total_rate_s * days_s,
#     where days_s = active days of snapshot s inside [2025-04-01, 2025-12-31].
#   - Aggregate hts10 x country -> BEA commodity by 2024 import-weighting
#     (hs10_bea_crosswalk.csv x import_weights_hs10_country). Import weights are
#     time-invariant, so time-averaging and import-weighting commute.
#   - Baseline = the 2025-01-01 snapshot, same aggregation. delta = avg - baseline
#     is the 2025 tariff escalation = the price shock fed to the matrix.
#
# Outputs (output/import_content_matrix/):
#   bea_commodity_tariff_rates_2025.csv
#   cex_price_impact_apr_dec_2025.csv
# =============================================================================

import os
import datetime as dt
import numpy as np
import pandas as pd

REPO = 'C:/Users/jar335/Documents/Repositories/Tariff-Model'
ROOT = 'C:/Users/jar335/Documents/Interfaces/model_data/tariff-rate-tracker/2026-06-04_2'
RATE_AGG = os.path.join(REPO, 'resources/rate_aggregation/2024')
OUT = os.path.join(REPO, 'output/import_content_matrix')

WINDOW_START = dt.date(2025, 4, 1)
WINDOW_END   = dt.date(2025, 12, 31)          # inclusive
WIN_END_EXCL = WINDOW_END + dt.timedelta(days=1)
BASELINE_DATE = '2025-01-01'

SCALING = 1 + 0.5 - 0.174                      # 1 + domestic_pricing - usd_offset = 1.326
RATE_COL = 'total_rate'                        # total effective ad-valorem rate (fractional)

# ---- 1. Base table: (hts10, country) -> bea_code + 2024 import value ---------
weights = pd.read_parquet(os.path.join(ROOT, 'weights/import_weights_hs10_country.parquet'),
                          columns=['hts10', 'country', 'imports'])
xw = pd.read_csv(os.path.join(RATE_AGG, 'hs10_bea_crosswalk.csv'), dtype=str) \
       .rename(columns={'hs10': 'hts10'})
base = weights.merge(xw, on='hts10', how='left')
unmatched_imp = base.loc[base['bea_code'].isna(), 'imports'].sum()
total_imp = base['imports'].sum()
print(f'Base rows: {len(base):,}   imports unmatched to BEA: '
      f'${unmatched_imp/1e9:.2f}B of ${total_imp/1e9:.1f}B '
      f'({100*unmatched_imp/total_imp:.2f}%)')
base = base.dropna(subset=['bea_code'])
base = base[base['imports'] > 0].copy()

# ---- 2. Snapshot day-weights over the window --------------------------------
snap_dir = os.path.join(ROOT, 'actual/snapshots')
dates = sorted(d.split('=')[1] for d in os.listdir(snap_dir) if d.startswith('valid_from='))
dord = [dt.date.fromisoformat(d) for d in dates]

day_weights = {}
for i, d in enumerate(dord):
    nxt = dord[i + 1] if i + 1 < len(dord) else dt.date(2100, 1, 1)
    a = max(d, WINDOW_START)
    b = min(nxt, WIN_END_EXCL)
    days = (b - a).days
    if days > 0:
        day_weights[dates[i]] = days
assert sum(day_weights.values()) == (WIN_END_EXCL - WINDOW_START).days
print(f'Window snapshots: {len(day_weights)}   total days: {sum(day_weights.values())}')

def bea_rate_for_snapshot(date_str):
    """Import-weighted total_rate by BEA commodity for one snapshot."""
    r = pd.read_parquet(os.path.join(snap_dir, f'valid_from={date_str}/rates.parquet'),
                        columns=['hts10', 'country', RATE_COL])
    m = base.merge(r, on=['hts10', 'country'], how='left')
    # HTS10 x country lines with imports but no rate row: treat as no tariff (0).
    m[RATE_COL] = m[RATE_COL].fillna(0.0)
    m['rw'] = m[RATE_COL] * m['imports']
    g = m.groupby('bea_code').agg(rw=('rw', 'sum'), imp=('imports', 'sum'))
    g['rate'] = g['rw'] / g['imp']
    return g['rate'], g['imp']

# ---- 3. Time-weighted average rate by BEA over the window -------------------
acc_rate = None
acc_imp = None
tot_days = sum(day_weights.values())
for date_str, days in day_weights.items():
    rate, imp = bea_rate_for_snapshot(date_str)
    acc_rate = rate * days if acc_rate is None else acc_rate.add(rate * days, fill_value=0)
    acc_imp = imp if acc_imp is None else acc_imp
    print(f'  {date_str}: {days:3d} days')
avg_rate = (acc_rate / tot_days).rename('avg_total_rate_apr_dec_2025')

# ---- 4. Baseline (2025-01-01) and delta -------------------------------------
base_rate, _ = bea_rate_for_snapshot(BASELINE_DATE)
base_rate = base_rate.rename('baseline_rate_2025_01_01')

desc = pd.read_csv(os.path.join(REPO, 'resources/io/bea_commodity_descriptions.csv')) \
         .rename(columns={'Commodity Code': 'bea_code', 'Description': 'description'})

rates = pd.concat([avg_rate, base_rate, acc_imp.rename('import_value_2024')], axis=1).reset_index()
rates = rates.merge(desc, on='bea_code', how='left')
rates['delta_rate'] = rates['avg_total_rate_apr_dec_2025'] - rates['baseline_rate_2025_01_01']
rates = rates[['bea_code', 'description', 'import_value_2024',
               'baseline_rate_2025_01_01', 'avg_total_rate_apr_dec_2025', 'delta_rate']] \
        .sort_values('delta_rate', ascending=False)
rates.to_csv(os.path.join(OUT, 'bea_commodity_tariff_rates_2025.csv'), index=False)

imp_wt_avg = np.average(rates['avg_total_rate_apr_dec_2025'], weights=rates['import_value_2024'])
imp_wt_delta = np.average(rates['delta_rate'], weights=rates['import_value_2024'])
print(f'\nImport-weighted avg total rate (Apr-Dec 2025): {imp_wt_avg*100:.2f}%')
print(f'Import-weighted baseline (2025-01-01):         '
      f'{np.average(rates["baseline_rate_2025_01_01"], weights=rates["import_value_2024"])*100:.2f}%')
print(f'Import-weighted delta (the 2025 shock):        {imp_wt_delta*100:.2f}%')

# ---- 5. Run the delta through the import-content matrix ----------------------
A = pd.read_csv(os.path.join(OUT, 'import_content_matrix_ces.csv'))
commod_cols = [c for c in A.columns if c not in ('cex_category', 'total_import_content')]
tau = rates.set_index('bea_code')['delta_rate'].reindex(commod_cols).fillna(0.0).values
price_impact_pp = SCALING * 100.0 * (A[commod_cols].values @ tau)
cex_out = pd.DataFrame({
    'cex_category': A['cex_category'],
    'total_import_content': A['total_import_content'],
    'price_impact_pp': price_impact_pp,
}).sort_values('price_impact_pp', ascending=False)
cex_out.to_csv(os.path.join(OUT, 'cex_price_impact_apr_dec_2025.csv'), index=False)

# Aggregate consumer price impact via native 76-PCE matrix + PCE purchaser value
A_pce = pd.read_csv(os.path.join(OUT, 'import_content_matrix_pce.csv'))
pce_cols = [c for c in A_pce.columns if c not in ('pce_category', 'total_import_content')]
tau_pce = pd.Series(rates.set_index('bea_code')['delta_rate']).reindex(pce_cols).fillna(0.0).values
bridge = pd.read_csv(os.path.join(REPO, 'resources/io/pce_bridge.csv'))
pv = bridge.groupby('pce_category')['purchasers_value'].sum()
pv = pv.reindex(A_pce['pce_category']).values
cat_impact = SCALING * 100.0 * (A_pce[pce_cols].values @ tau_pce)
agg_cpi = np.average(cat_impact, weights=pv)
print(f'\nAggregate consumer price-level impact (pre-substitution PE): {agg_cpi:.2f}%')

print('\n=== Consumer price impact by CEX category (pp), from 2025 delta ===')
print(cex_out.to_string(index=False))
print(f'\nWrote: {OUT}/bea_commodity_tariff_rates_2025.csv')
print(f'Wrote: {OUT}/cex_price_impact_apr_dec_2025.csv')
