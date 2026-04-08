"""Fact-check all report numbers against raw CSV data."""
import csv, os

REPO = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

def read_kr(path):
    with open(path) as f:
        return {r['metric']: float(r['value']) for r in csv.DictReader(f)}

def read_csv(path):
    with open(path) as f:
        return list(csv.DictReader(f))

T = read_kr(os.path.join(REPO, 'output/2026-04-06/results/key_results.csv'))
P = read_kr(os.path.join(REPO, 'output/2026-04-06_s122perm/results/key_results.csv'))

t_sec = {r['sector']: float(r['output_change_pct']) for r in read_csv(os.path.join(REPO, 'output/2026-04-06/results/sector_effects.csv'))}
p_sec = {r['sector']: float(r['output_change_pct']) for r in read_csv(os.path.join(REPO, 'output/2026-04-06_s122perm/results/sector_effects.csv'))}

t_dist = sorted(read_csv(os.path.join(REPO, 'output/2026-04-06/results/distribution.csv')), key=lambda x: int(x['decile']))
p_dist = sorted(read_csv(os.path.join(REPO, 'output/2026-04-06_s122perm/results/distribution.csv')), key=lambda x: int(x['decile']))

t_rev = read_csv(os.path.join(REPO, 'output/2026-04-06/results/revenue_by_year.csv'))
p_rev = read_csv(os.path.join(REPO, 'output/2026-04-06_s122perm/results/revenue_by_year.csv'))

t_dyn = read_csv(os.path.join(REPO, 'output/2026-04-06/results/dynamic_revenue_by_year.csv'))
p_dyn = read_csv(os.path.join(REPO, 'output/2026-04-06_s122perm/results/dynamic_revenue_by_year.csv'))

GDP = 28000

checks = []
def chk(label, raw, reported, ok):
    status = 'OK' if ok else '** FAIL **'
    checks.append((label, raw, reported, status))
    print(f'  {status:10s} {label:50s} raw={raw:>12.4f}  rpt={reported}')

print('=' * 80)
print('FACT CHECK: All report numbers vs raw model output')
print('=' * 80)

print('\n--- ETR Levels (%) ---')
chk('PERM pre-sub all-in ("stands at X%")', P['pre_sub_all_in_etr'], 12.2, abs(P['pre_sub_all_in_etr'] - 12.2) < 0.05)
chk('PERM post-sub all-in', P['pe_postsub_all_in_etr'], 10.5, abs(P['pe_postsub_all_in_etr'] - 10.5) < 0.05)
chk('TEMP pre-sub all-in ("fall to X%")', T['pre_sub_all_in_etr'], 9.7, abs(T['pre_sub_all_in_etr'] - 9.7) < 0.05)
chk('TEMP post-sub all-in', T['pe_postsub_all_in_etr'], 8.2, abs(T['pe_postsub_all_in_etr'] - 8.2) < 0.05)

print('\n--- ETR Increases (pp) ---')
chk('PERM pre-sub ETR increase', P['pre_sub_etr_increase'], 10.2, abs(P['pre_sub_etr_increase'] - 10.2) < 0.05)
chk('PERM post-sub ETR increase', P['pe_postsub_etr_increase'], 8.5, abs(P['pe_postsub_etr_increase'] - 8.5) < 0.05)
chk('TEMP pre-sub ETR increase', T['pre_sub_etr_increase'], 7.7, abs(T['pre_sub_etr_increase'] - 7.7) < 0.05)
chk('TEMP post-sub ETR increase', T['pe_postsub_etr_increase'], 6.0, abs(T['pe_postsub_etr_increase'] - 6.0) < 0.05)

print('\n--- Prices (%) ---')
chk('TEMP pre-sub price', T['pre_sub_price_increase'], 0.7, abs(T['pre_sub_price_increase'] - 0.7) < 0.05)
chk('TEMP post-sub price', T['pe_postsub_price_increase'], 0.5, abs(T['pe_postsub_price_increase'] - 0.5) < 0.05)
chk('PERM pre-sub price', P['pre_sub_price_increase'], 1.1, abs(P['pre_sub_price_increase'] - 1.1) < 0.05)
chk('PERM post-sub price', P['pe_postsub_price_increase'], 0.9, abs(P['pe_postsub_price_increase'] - 0.9) < 0.05)

print('\n--- Per-HH Costs ($) ---')
chk('TEMP pre-sub HH cost', T['pre_sub_per_hh_cost'], 939, abs(T['pre_sub_per_hh_cost'] - 939) < 1)
chk('TEMP post-sub HH cost', T['post_sub_per_hh_cost'], 761, abs(T['post_sub_per_hh_cost'] - 761) < 1)
chk('PERM pre-sub HH cost', P['pre_sub_per_hh_cost'], 1478, abs(P['pre_sub_per_hh_cost'] - 1478) < 1)
chk('PERM post-sub HH cost', P['post_sub_per_hh_cost'], 1208, abs(P['post_sub_per_hh_cost'] - 1208) < 1)

print('\n--- Long-Run GDP ---')
chk('TEMP LR GDP (%)', abs(t_sec['overall_gdp']), 0.11, abs(abs(t_sec['overall_gdp']) - 0.11) < 0.005)
chk('PERM LR GDP (%)', abs(p_sec['overall_gdp']), 0.18, abs(abs(p_sec['overall_gdp']) - 0.18) < 0.005)
lr_dollars = round(abs(t_sec['overall_gdp']) / 100 * GDP)
chk('TEMP LR GDP ($B equiv)', abs(t_sec['overall_gdp']) / 100 * GDP, 32, lr_dollars == 32)

print('\n--- Sectors (TEMP, %) ---')
chk('Manufacturing', t_sec['manufacturing'], 1.1, abs(t_sec['manufacturing'] - 1.1) < 0.05)
chk('Construction (abs)', abs(t_sec['construction']), 2.5, abs(abs(t_sec['construction']) - 2.5) < 0.05)
chk('Mining (abs)', abs(t_sec['mining']), 1.0, abs(abs(t_sec['mining']) - 1.0) < 0.05)

print('\n--- Revenue (10yr, billions -> trillions) ---')
# Verify CSV sums match key_results
t_conv_sum = sum(float(r['net_revenue']) for r in t_rev if r['fiscal_year'] != '2025')
p_conv_sum = sum(float(r['net_revenue']) for r in p_rev if r['fiscal_year'] != '2025')
t_dyn_sum = sum(float(r['dynamic_revenue']) for r in t_dyn if r['fiscal_year'] != '2025')
p_dyn_sum = sum(float(r['dynamic_revenue']) for r in p_dyn if r['fiscal_year'] != '2025')
t_eff_sum = sum(float(r['dynamic_effect']) for r in t_dyn if r['fiscal_year'] != '2025')
p_eff_sum = sum(float(r['dynamic_effect']) for r in p_dyn if r['fiscal_year'] != '2025')

chk('TEMP conv sum == key_results?', t_conv_sum, T['conventional_revenue_10yr'], abs(t_conv_sum - T['conventional_revenue_10yr']) < 1)
chk('PERM conv sum == key_results?', p_conv_sum, P['conventional_revenue_10yr'], abs(p_conv_sum - P['conventional_revenue_10yr']) < 1)
chk('TEMP dyn sum == key_results?', t_dyn_sum, T['dynamic_revenue_10yr'], abs(t_dyn_sum - T['dynamic_revenue_10yr']) < 1)
chk('PERM dyn sum == key_results?', p_dyn_sum, P['dynamic_revenue_10yr'], abs(p_dyn_sum - P['dynamic_revenue_10yr']) < 1)

chk('TEMP conv ($T)', T['conventional_revenue_10yr']/1000, 1.3, abs(T['conventional_revenue_10yr']/1000 - 1.3) < 0.05)
chk('TEMP dyn ($T)', T['dynamic_revenue_10yr']/1000, 1.2, abs(T['dynamic_revenue_10yr']/1000 - 1.2) < 0.05)
chk('PERM conv ($T)', P['conventional_revenue_10yr']/1000, 1.9, abs(P['conventional_revenue_10yr']/1000 - 1.9) < 0.05)
chk('PERM dyn ($T)', P['dynamic_revenue_10yr']/1000, 1.7, abs(P['dynamic_revenue_10yr']/1000 - 1.7) < 0.05)
chk('TEMP dyn effect ($B)', abs(T['dynamic_effect_10yr']), 106, abs(abs(T['dynamic_effect_10yr']) - 106) < 1)
chk('PERM dyn effect ($B)', abs(P['dynamic_effect_10yr']), 159, abs(abs(P['dynamic_effect_10yr']) - 159) < 1)

print('\n--- Revenue internal consistency: conv + effect = dyn ---')
t_check = T['conventional_revenue_10yr'] + T['dynamic_effect_10yr']
p_check = P['conventional_revenue_10yr'] + P['dynamic_effect_10yr']
chk('TEMP: conv + effect = dyn', t_check, T['dynamic_revenue_10yr'], abs(t_check - T['dynamic_revenue_10yr']) < 1)
chk('PERM: conv + effect = dyn', p_check, P['dynamic_revenue_10yr'], abs(p_check - P['dynamic_revenue_10yr']) < 1)

print('\n--- Distribution ---')
d1t = abs(float(t_dist[0]['pct_of_income']))
d10t = abs(float(t_dist[9]['pct_of_income']))
d1p = abs(float(p_dist[0]['pct_of_income']))
d10p = abs(float(p_dist[9]['pct_of_income']))
chk('TEMP D1 pct of income', d1t, 1.3, abs(d1t - 1.3) < 0.05)
chk('TEMP D10 pct of income', d10t, 0.4, abs(d10t - 0.4) < 0.05)
chk('PERM D1 pct of income', d1p, 2.1, abs(d1p - 2.1) < 0.05)
chk('PERM D10 pct of income', d10p, 0.7, abs(d10p - 0.7) < 0.05)
ratio = d1t / d10t
chk('D1/D10 ratio -> "3 times"', ratio, 3, round(ratio) == 3)

d1ct = abs(float(t_dist[0]['cost_per_hh']))
d10ct = abs(float(t_dist[9]['cost_per_hh']))
d1cp = abs(float(p_dist[0]['cost_per_hh']))
d10cp = abs(float(p_dist[9]['cost_per_hh']))
chk('TEMP D1 cost per HH', d1ct, 517, abs(d1ct - 517) < 1)
chk('TEMP D10 cost per HH', d10ct, 2175, abs(d10ct - 2175) < 1)
chk('PERM D1 cost per HH', d1cp, 813, abs(d1cp - 813) < 1)
chk('PERM D10 cost per HH', d10cp, 3424, abs(d10cp - 3424) < 1)

# Summary
mismatches = [c for c in checks if c[3] != 'OK']
print('\n' + '=' * 80)
print(f'Total checks: {len(checks)}')
print(f'Passed: {len(checks) - len(mismatches)}')
print(f'Failed: {len(mismatches)}')
if mismatches:
    print('\nFAILURES:')
    for label, raw, reported, status in mismatches:
        print(f'  {label}: raw={raw:.6f}, reported={reported}')
else:
    print('\nAll quantitative checks passed.')
print('=' * 80)
