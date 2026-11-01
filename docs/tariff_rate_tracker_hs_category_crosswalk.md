# Tariff-Rate-Tracker HS Category Crosswalk

This is the classification behind Tariff-Rate-Tracker's `daily_by_hs.csv`
output. It assigns each product's 10-digit HTS code to a dashboard product
category. The category is based on the underlying product code, rather than a
Chapter 99 tariff-modification provision.

Source: `Budget-Lab-Yale/tariff-rate-tracker` GitHub `master`, commit
`67fc53c` (`src/pipeline/09_daily_series.R`).

| Dashboard category | `category_code` | Product HTS rule |
|---|---|---|
| Agriculture & live animals | `ag_live_animals` | Chapters 01–15 |
| Processed foods, beverages & tobacco | `processed_foods` | Chapters 16–24 |
| Minerals & ores | `minerals_ores` | Chapters 25–26 |
| Energy (mineral fuels) | `energy` | Chapter 27 |
| Chemicals (ex-pharma) | `chemicals` | Chapters 28–29 and 31–38 |
| Pharmaceuticals | `pharmaceuticals` | Chapter 30 |
| Plastics & rubber | `plastics_rubber` | Chapters 39–40 |
| Leather & furskins | `leather` | Chapters 41–43 |
| Wood, paper & printing | `wood_paper` | Chapters 44–49 |
| Textiles, apparel & footwear | `textiles_apparel_footwear` | Chapters 50–67 |
| Stone, cement, ceramics & glass | `stone_glass` | Chapters 68–70 |
| Precious metals & stones | `precious_metals` | Chapter 71 |
| Iron & steel | `iron_steel` | Chapters 72–73 |
| Other base metals | `other_base_metals` | Chapters 74–75 and 78–83 |
| Aluminum | `aluminum` | Chapter 76 |
| Industrial machinery & computers | `machinery` | Chapter 84 |
| Electronics & electrical equipment | `electronics` | Chapter 85, except headings 8541 and 8542 |
| Semiconductors & electronic components | `semiconductors` | Headings 8541 and 8542 (overrides Chapter 85) |
| Other transport equipment | `other_transport` | Chapters 86 and 88–89 |
| Motor vehicles & parts | `motor_vehicles` | Chapter 87 |
| Precision instruments & optical | `instruments` | Chapters 90–92 |
| Miscellaneous manufactures | `misc_manufactures` | Chapters 93–97 |
| Special / unclassified | `unclassified` | Chapter 77, Chapters 98–99, or any unmatched/malformed code |

## Classification rules

1. The input is the product's 10-digit HTS code. Leading zeros are retained.
2. The first two digits determine the chapter category above.
3. The only heading-level exception is `8541` or `8542`, which is always
   categorized as semiconductors rather than the general Chapter 85 electronics
   category.
4. The heading exception takes precedence over the chapter assignment.
5. Category 99 tariff provisions do not determine the product category. A
   product receiving a Chapter 99 tariff is classified by its underlying product
   HTS code.
6. The classification is mutually exclusive: each product receives one category.
7. It is exhaustive: codes without a listed match go to `unclassified`, rather
   than being silently dropped.

## Suggested chart note

> Product groups are based on underlying HTS product codes. Most groups follow
> HS chapters; semiconductors are HTS headings 8541–8542 and are excluded from
> the broader electronics category. Chapter 99 tariff provisions are assigned
> to the underlying product group.
