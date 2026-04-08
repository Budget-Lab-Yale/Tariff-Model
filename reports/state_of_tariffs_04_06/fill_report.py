"""
Fill placeholders in state_of_tariffs.md with actual model output values.

Usage: python reports/state_of_tariffs_04_06/fill_report.py
"""

import csv
import os
import math

REPO = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
TEMP_DIR = os.path.join(REPO, 'output', '2026-04-06', 'results')
PERM_DIR = os.path.join(REPO, 'output', '2026-04-06_s122perm', 'results')
MD_PATH = os.path.join(REPO, 'reports', 'state_of_tariffs_04_06', 'state_of_tariffs.md')

def read_key_results(path):
    with open(path, newline='') as f:
        rows = list(csv.DictReader(f))
    return {r['metric']: float(r['value']) for r in rows}

def read_csv_dict(path):
    with open(path, newline='') as f:
        return list(csv.DictReader(f))

temp = read_key_results(os.path.join(TEMP_DIR, 'key_results.csv'))
perm = read_key_results(os.path.join(PERM_DIR, 'key_results.csv'))

temp_sectors = {r['sector']: float(r['output_change_pct']) for r in read_csv_dict(os.path.join(TEMP_DIR, 'sector_effects.csv'))}
perm_sectors = {r['sector']: float(r['output_change_pct']) for r in read_csv_dict(os.path.join(PERM_DIR, 'sector_effects.csv'))}

temp_dist = sorted(read_csv_dict(os.path.join(TEMP_DIR, 'distribution.csv')), key=lambda x: int(x['decile']))
perm_dist = sorted(read_csv_dict(os.path.join(PERM_DIR, 'distribution.csv')), key=lambda x: int(x['decile']))

# Helpers
def r1(v): return f'{v:.1f}'
def r2(v): return f'{v:.2f}'

# Long-run GDP in dollar terms (% of ~$28T 2025 GDP)
GDP_2025 = 28000  # approximate 2025 GDP in billions

# Build replacements
replacements = {
    # ETR levels (already in %)
    'PRESUB_ALLIN_TEMP': r1(temp['pre_sub_all_in_etr']),
    'POSTSUB_ALLIN_TEMP': r1(temp['pe_postsub_all_in_etr']),
    'PRESUB_ALLIN_PERM': r1(perm['pre_sub_all_in_etr']),
    'POSTSUB_ALLIN_PERM': r1(perm['pe_postsub_all_in_etr']),

    # ETR increase (pp)
    'PRESUB_ETR_INCR_PERM': r1(perm['pre_sub_etr_increase']),
    'POSTSUB_ETR_INCR_PERM': r1(perm['pe_postsub_etr_increase']),
    'PRESUB_ETR_INCR_TEMP': r1(temp['pre_sub_etr_increase']),
    'POSTSUB_ETR_INCR_TEMP': r1(temp['pe_postsub_etr_increase']),

    # Price effects (already in %)
    'PRESUB_PRICE_TEMP': r1(temp['pre_sub_price_increase']),
    'POSTSUB_PRICE_TEMP': r1(temp['pe_postsub_price_increase']),
    'PRESUB_PRICE_PERM': r1(perm['pre_sub_price_increase']),
    'POSTSUB_PRICE_PERM': r1(perm['pe_postsub_price_increase']),

    # Per-household costs
    'PRESUB_HH_TEMP': f'{temp["pre_sub_per_hh_cost"]:,.0f}',
    'POSTSUB_HH_TEMP': f'{temp["post_sub_per_hh_cost"]:,.0f}',
    'PRESUB_HH_PERM': f'{perm["pre_sub_per_hh_cost"]:,.0f}',
    'POSTSUB_HH_PERM': f'{perm["post_sub_per_hh_cost"]:,.0f}',

    # Long-run GDP (from sector_effects overall_gdp)
    'LR_GDP_TEMP': r2(abs(temp_sectors['overall_gdp'])),
    'LR_GDP_PERM': r2(abs(perm_sectors['overall_gdp'])),
    'LR_GDP_DOLLARS_TEMP': str(round(abs(temp_sectors['overall_gdp']) / 100 * GDP_2025)),

    # Sector effects
    'MFG_TEMP': r1(temp_sectors['manufacturing']),
    'CONST_TEMP': r1(abs(temp_sectors['construction'])),
    'MINING_TEMP': r1(abs(temp_sectors['mining'])),

    # Revenue (convert billions to trillions for display)
    'CONV_REV_TEMP': f'${temp["conventional_revenue_10yr"]/1000:.1f} trillion',
    'DYN_REV_TEMP': f'${temp["dynamic_revenue_10yr"]/1000:.1f} trillion',
    'CONV_REV_PERM': f'${perm["conventional_revenue_10yr"]/1000:.1f} trillion',
    'DYN_REV_PERM': f'${perm["dynamic_revenue_10yr"]/1000:.1f} trillion',
    'DYN_EFFECT_TEMP': f'${abs(temp["dynamic_effect_10yr"]):.0f} billion',

    # Distribution
    'DIST_D1_TEMP': r1(abs(float(temp_dist[0]['pct_of_income']))),
    'DIST_D10_TEMP': r1(abs(float(temp_dist[9]['pct_of_income']))),
    'DIST_D1_PERM': r1(abs(float(perm_dist[0]['pct_of_income']))),
    'DIST_D10_PERM': r1(abs(float(perm_dist[9]['pct_of_income']))),
    'DIST_D1_COST_TEMP': f'{abs(float(temp_dist[0]["cost_per_hh"])):,.0f}',
    'DIST_D10_COST_TEMP': f'{abs(float(temp_dist[9]["cost_per_hh"])):,.0f}',
    'DIST_D1_COST_PERM': f'{abs(float(perm_dist[0]["cost_per_hh"])):,.0f}',
    'DIST_D10_COST_PERM': f'{abs(float(perm_dist[9]["cost_per_hh"])):,.0f}',
}

# Compute D1 vs D10 ratio text
d1_temp = abs(float(temp_dist[0]['pct_of_income']))
d10_temp = abs(float(temp_dist[9]['pct_of_income']))
ratio = d1_temp / d10_temp
replacements['D1_VS_D10_TEMP'] = f'{ratio:.0f} times that of the top decile'

# Read and replace
with open(MD_PATH, 'r') as f:
    content = f.read()

for key, val in replacements.items():
    content = content.replace(key, val)

with open(MD_PATH, 'w') as f:
    f.write(content)

print('Filled placeholders in state_of_tariffs.md')
print()
print('Key numbers:')
print(f'  ETR (pre-sub, S122 perm):  {replacements["PRESUB_ALLIN_PERM"]}%')
print(f'  ETR (post-sub, S122 perm): {replacements["POSTSUB_ALLIN_PERM"]}%')
print(f'  ETR (pre-sub, expires):    {replacements["PRESUB_ALLIN_TEMP"]}%')
print(f'  ETR (post-sub, expires):   {replacements["POSTSUB_ALLIN_TEMP"]}%')
print(f'  Price (pre-sub, expires):  {replacements["PRESUB_PRICE_TEMP"]}%')
print(f'  Price (pre-sub, perm):     {replacements["PRESUB_PRICE_PERM"]}%')
print(f'  HH cost (pre-sub, exp):    ${replacements["PRESUB_HH_TEMP"]}')
print(f'  HH cost (pre-sub, perm):   ${replacements["PRESUB_HH_PERM"]}')
print(f'  Conv rev (expires):        {replacements["CONV_REV_TEMP"]}')
print(f'  Conv rev (perm):           {replacements["CONV_REV_PERM"]}')
print(f'  LR GDP (expires):          {replacements["LR_GDP_TEMP"]}%')
print(f'  LR GDP (perm):             {replacements["LR_GDP_PERM"]}%')
