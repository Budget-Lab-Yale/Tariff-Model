# Memo on the Price-Impact Overhaul

This note summarizes how the tariff model's price methodology changed, why we changed it, and what is still unresolved. It is meant to be readable by someone who knows the old model well, not as a technical appendix.

## 1. The old approach

The old model used a single aggregate formula for the effect of tariffs on the overall PCE price level:

\[
\text{price effect}
= (\text{ETR} \times -\text{USD offset} \times \text{goods share} \times \text{import share})
+ (\text{ETR} \times \text{goods share} \times \text{import share} \times (1 + \text{passthrough}))
\]

In code, that was implemented as:

- `price_passthrough = 0.50`
- `goods_share_pce = 0.31471`
- `import_share = 0.20570`
- `usd_offset = 0.174`

So, mechanically, the old model treated tariff price effects as a scalar function of the average tariff increase and a few fixed parameters. The "post-substitution" variant then adjusted the aggregate `goods_share` and `import_share` using two GTAP-derived scalars from NVPP data:

- `goods_adjustment = goods_share_post / goods_share_pre`
- `import_adjustment = import_share_post / import_share_pre`

That approach had real virtues. It was simple, transparent, fast, and easy to explain. It also produced a single number that moved in the right direction when the average tariff rate went up or down.

But its limitations became more serious as the scenarios became more complicated.

## 2. The main problems with the old approach

The biggest issue is that the old model compressed too much economics into one equation.

First, it relied on a single economy-wide import share. That share did not respond to changes in the tariff base in a granular way. A tariff package concentrated in autos, apparel, and electronics was treated too much like a tariff package with the same average ETR spread evenly across all goods. The old "post-substitution" fix helped somewhat, but only through two scalar adjustment factors. It still did not let import exposure vary commodity by commodity.

Second, the passthrough assumption was doing too much work. A `0.50` passthrough parameter means the direct tariff term is effectively multiplied by `1.5` before the exchange-rate offset is applied. That may have been a reasonable reduced-form shortcut when we were looking at a much narrower set of Trump 1.0-type tariffs. But as a single aggregate number it bundled together three distinct things — standard passthrough, the domestic supply-chain cost channel, and a domestic competitive-pricing response — without a clear structural interpretation. The new model unbundles the supply-chain channel and makes it explicit (Section 3); it *retains* a domestic-pricing markup of the same `0.50` magnitude, but now applied on top of that explicit structure rather than standing in for it. (See the scaling factor and the outstanding question below.)

Third, it was not really a consumer-price model. It produced an aggregate number, but it did not solve the basic communication problem reporters care about: how do tariff-induced changes in commodity prices map into consumer spending categories? Commodity definitions and consumption categories are not the same thing. Reporters ask about food, autos, appliances, clothing, and household budgets, not abstract import-weighted sectors.

Fourth, the old treatment of "long-run" product prices was internally inconsistent. Short-run product prices were built from `import_share × weighted_ETR` and then normalized to match the aggregate short-run effect. Long-run product prices were based on GTAP `ppa` and then normalized to match the aggregate post-substitution effect. In practice that meant the long-run aggregate was being "shared out" across products in proportion to GTAP commodity price changes, even though the aggregate itself came from a different reduced-form price equation. That is not wrong as a rough shortcut, but it is not a coherent price system.

## 3. What the new approach does

The new framework replaces the scalar formula with a commodity-level input-output model, and then maps those commodity results into consumer categories.

At a high level, the new pre-substitution calculation is:

\[
P_c
= \Big[\omega^M_c \tau_c
+ \omega^D_c (L_D' B_{MD}' \tau)_c \Big] \times (1 + \text{domestic pricing} - \text{USD offset})
\]

where:

- `tau` is the tariff shock by BEA commodity
- `omega_M` and `omega_D` are commodity-specific import and domestic shares
- `B_MD` captures imported-input cost exposure by industry
- `L_D` is the domestic Leontief structure that propagates those cost shocks through the supply chain
- the bracketed import + supply-chain effect is scaled by `(1 + domestic_pricing − usd_offset)`, where `domestic_pricing = 0.5` is the retained domestic competitive-pricing markup and `usd_offset = 0.174` is the exchange-rate offset. With the current parameters that net multiplier is `1.326`.

Note on the markup: the `0.5` here is not the same object as the old `0.50` passthrough, even though the magnitude matches. In the old model `1 + 0.5` multiplied a single reduced-form direct term and bundled the supply chain into it. Here the supply-chain channel is computed explicitly through `L_D` and `B_MD`, and the `0.5` is a separate, structurally narrower domestic-pricing markup applied to the whole commodity-level effect. It is a retained judgment-call parameter, not an elimination (see Section 7).

That gives us price effects at the BEA commodity level. We then map those commodity prices into NIPA PCE categories using the PCE bridge, which means the model now produces category-level consumer price effects rather than just an overall scalar.

The model now reports three price concepts:

- `pre_sub`: a partial-equilibrium, pre-substitution I-O result
- `pe_postsub`: a partial-equilibrium post-substitution result that updates tariff exposure and import shares using GTAP
- `ge`: a GTAP-based general-equilibrium consumer price concept, mapped back to BEA commodities and then to PCE categories

That is a cleaner setup conceptually. The "partial-equilibrium" and "general-equilibrium" numbers are now distinct objects instead of being blended together in an ad hoc way.

## 4. What is better about the new approach

The main gain is that exposure now lives where it should live: at the commodity level, not in one fixed aggregate import share.

That matters for at least three reasons.

One, the tariff base matters. A scenario that hits a narrow set of highly import-intensive goods now looks different from a broad scenario that hits many goods with different supply-chain structures. The model can finally reflect that.

Two, the new setup gives us an honest mapping from commodity prices to consumer prices. That is important analytically and important for communication. We can now talk about autos, apparel, appliances, food, and other PCE categories directly.

Three, the post-substitution concept is more disciplined. Instead of just shrinking one aggregate import share, the model updates both:

- the effective tariff on a commodity, through GTAP source-composition shifts
- the commodity's import share in absorption, through GTAP NVPP data

This is still only partial equilibrium, but it is a much more defensible partial equilibrium than before.

Finally, the new framework makes it possible to inspect the GE result rather than simply inherit it. We now have a GTAP GE decomposition that splits the GE consumer price effect into:

- an import-price component
- a domestic-price component
- a share-shift component
- a residual interaction term

That makes the GTAP output much easier to explain than it was before.

## 5. What the new approach costs us

The new method is better, but it is not free.

It is more complicated. The old model could be summarized in one formula and four parameters. The new model needs BEA use tables, a PCE bridge, a GTAP-BEA crosswalk, markup assumptions, and GTAP outputs. That is a real cost in maintainability and in explainability.

It also introduces more judgment calls. We now have to choose whether to use constant-dollar or constant-percentage markups in the I-O model, or average the two. That is a more intellectually honest problem than before, but it is still a problem.

And the GE result is harder to explain. In some scenarios, especially broad tariff regimes, the GTAP GE consumer-price effect can end up above the pre-substitution partial-equilibrium effect. That is economically coherent, but it is not intuitive to non-specialists. It takes work to explain that substitution can reduce import exposure while still raising the overall consumer price level if domestic prices rise enough.

Finally, the detail I-O path brings its own compromise. It uses richer commodity detail, but on older benchmark tables than the summary path. That is an improvement in one dimension and a step back in another.

## 6. Bottom line

My view is that the overhaul was worth it.

The old model was useful as a quick reduced-form approximation. But it was too aggregate, too parameter-driven, and too dependent on a passthrough assumption that was doing more conceptual work than it could really support. Most importantly, it did not produce consumer-category results in a coherent way.

The new framework is more complex, but it is much closer to the question we are actually trying to answer: how do tariff shocks move through production, through import substitution, and ultimately into the prices consumers pay?

## 7. Outstanding questions

- What exchange-rate offset should we use in the partial-equilibrium I-O model, if any?
- Is `domestic_pricing = 0.5` the right magnitude for the retained domestic competitive-pricing markup? It carries over the old passthrough magnitude and lifts every pre-sub and PE-post-sub price effect by a net `~33%` (the `1.326` multiplier); setting it to `0` would reduce the headline price effects accordingly.
- Should the baseline I-O result use constant-dollar passthrough, constant-percentage passthrough, or the average of the two?
- Does the current "post-substitution" PE result capture enough of the behavioral margins we care about, or is it still too narrow?
- When should we show GTAP's GE consumer-price result, given that it can be economically sensible but harder to explain?
- Should the headline number for external use be pre-sub, PE post-sub, GE, or some clearly labeled range?
- How much confidence should we place in the detail I-O path given the tradeoff between commodity detail and benchmark-year recency?
