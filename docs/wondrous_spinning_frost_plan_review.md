# Detailed Review: Principled 484(f) HTS Crosswalk Plan

**Plan reviewed:** `~/.claude/plans/wondrous-spinning-frost.md`  
**Repositories reviewed:** `tariff-rate-tracker` and `Tariff-Model`  
**Review date:** 2026-07-14  
**Review scope:** Correctness, data semantics, parser design, revision-aware weighting, cache validity, publishing, downstream model guards, testing, and rollout safety.

## Executive summary

The plan has a strong overall architecture:

- Replace heuristic prefix-only HTS suffix mapping with dated, explicit 484(f) transfers.
- Preserve the 2024 Laspeyres dollar base while using observed data only to determine composition within splits.
- Apply mappings one effective date at a time and simultaneously within each date.
- Build revision-aware weights for the tracker daily series while preserving a tip-vintage weight artifact for Tariff-Model.
- Commit the generated crosswalk so production builds do not depend on PDF extraction.
- Invalidate existing daily-part caches and perform a full rebuild and downstream rollout.

However, the plan should not be implemented unchanged. The review found several correctness blockers:

1. The 484(f) PDFs contain both HTS and Schedule B history tables, but the proposed parser does not distinguish them.
2. Same-date HTS code reuse invalidates not only flat transitive closure but also direct lookup of 2025 successor trade and 2024 anchors.
3. Country-specific split weights cannot be represented by the proposed country-agnostic audit schema.
4. The proposed Tariff-Model coverage guard measures panel cells absent from weights, which is the opposite direction from the diagnosed denominator failure.
5. The official transfer documents contain apparent internal inconsistencies, so strict two-sided validation needs a committed, auditable override mechanism.
6. Crosswalk timing should be keyed to the HTS identity of a snapshot, not assumed to equal the daily series' policy-retimed `valid_from` date.

There are also important omissions around `.json.gz` archive coverage, input-aware cache invalidation, manifest revision metadata, published provenance, GTAP category mapping, CI wiring, acceptance criteria, and the order in which `latest` is repointed.

The plan should be revised to address these issues before implementation begins.

---

## 1. What the plan gets right

Several central design decisions are appropriate and should be retained.

### 1.1 Dated propagation instead of flat transitive closure

Applying transfers in ascending effective-date order, while applying all edges within one date simultaneously, is the right conceptual model. It prevents a newly reused code at date `d` from being treated as though it were the old commodity that occupied the same numeric code immediately before `d`.

For example, the January 2026 pharmaceutical supplement contains same-date changes conceptually equivalent to:

```text
old identity at 3004.90.9206 -> new identity at 3004.90.9208
old identity at 3004.90.9208 -> new identity at 3004.90.9209
```

A sequential in-place update would incorrectly cascade the first flow through the second edge. A simultaneous update should instead:

1. Snapshot all masses at the beginning of the effective date.
2. Remove or replace all outgoing old identities from that snapshot.
3. Allocate their masses to the new identities.
4. Combine contributions only after all edges for the date have been evaluated.

This principle should remain central to the mapper.

### 1.2 Preserving the 2024 dollar base

Using 2025 trade only for within-family composition, while always allocating 2024 dollars, preserves the intended Laspeyres weighting concept. The mapper should never replace the 2024 base with 2025 totals.

The implementation should assert both overall and per-country conservation:

```text
sum(mapped imports) == sum(base imports)
sum(mapped imports by country) == sum(base imports by country)
```

The second assertion is worth making explicit because the proposed mapping preserves country and therefore should conserve each country's total exactly, subject only to floating-point tolerance.

### 1.3 Committed CSV as the production resource

Separating one-shot PDF extraction from the production pipeline is appropriate. Production should read a committed, validated CSV and should not require `pdftools`, network access, or the original PDF layout to remain stable.

The builder should still preserve strong lineage from the committed CSV back to the source PDFs through document hashes, source row identifiers, and any explicit corrections.

### 1.4 Revision-aware tracker weights and tip-vintage model weights

The tracker daily series needs weights keyed to each interval's HTS universe. Tariff-Model currently consumes a single tip-vintage weights file. Keeping those contracts distinct is reasonable:

- Tracker daily aggregation: interval-aware weights.
- Published model interface: one weights file keyed to the tip snapshot.

The daily outputs themselves can embed historical interval weighting without publishing dozens of per-interval weight files, provided the inputs and mapping version are fully fingerprinted.

### 1.5 Hard conservation and on-panel assertions

The existing validation in `src/io/build_panel_import_weights.R` is valuable and should be factored into a shared helper as proposed. It should be expanded rather than weakened.

---

## 2. Critical correctness findings

## 2.1 The parser must distinguish HTS from Schedule B

### Problem

The proposed parser grammar recognizes any line beginning with a ten-digit dotted code followed by an action and effective date. The 484(f) documents contain both:

- `HTS History` / `HTS Code` tables, relevant to U.S. import classification; and
- `Schedule B History` / `Schedule B Code` tables, relevant to exports.

Both systems use ten-digit numeric codes and the same action vocabulary. A row-level regex cannot distinguish them without retaining document-section context.

The July 2025 document, for example, contains separate HTS and Schedule B histories for Terbufos. The HTS predecessor and Schedule B predecessor are different, while one successor code overlaps. Parsing both into the import crosswalk would introduce a false import transfer.

### Consequence

Schedule B edges could:

- Move 2024 import dollars using an export-classification relationship.
- Create false many-to-one or many-to-many components.
- Cause apparent two-sided inconsistencies.
- Make archive-coverage validation fail in confusing ways.
- Produce a conserved but substantively incorrect result.

Conservation alone would not catch this error.

### Required plan change

The parser should maintain section state. Suggested states include:

```text
nomenclature = HTS
nomenclature = SCHEDULE_B
nomenclature = UNKNOWN
```

State transitions should be triggered by headings such as:

```text
HTS History
HTS Code
Schedule B History
Schedule B Code
```

Only `nomenclature == "HTS"` rows should enter the import crosswalk. Unknown-context rows should fail validation rather than being silently accepted.

### Suggested resource schema addition

Add:

```text
nomenclature
```

Even if the committed production CSV contains only HTS rows, retaining the field makes the exclusion explicit and testable.

### Required tests

- A fixture containing adjacent HTS and Schedule B tables.
- A shared successor code with different HTS and Schedule B predecessors.
- An assertion that Schedule B rows are parsed or classified for audit but excluded from the HTS transfer resource.
- An assertion that an action row encountered under unknown nomenclature fails loudly.

---

## 2.2 Same-date code reuse invalidates historical successor-share lookups

### Problem

The plan recognizes that same-date reuse requires simultaneous propagation, but it still proposes direct use of:

1. country-specific 2025 imports for successor codes;
2. all-country 2025 imports for successor codes; and
3. 2024 direct-code anchors.

Those lookups treat an HTS code string as a permanent commodity identity. That assumption is false when a code is reused.

For example, if numeric code `3004.90.9208` represented commodity A in 2025, was discontinued on 2026-02-01, and was simultaneously established for commodity B, then:

```text
2025 imports[3004.90.9208]
```

are observations for commodity A, not evidence about commodity B. The same problem applies to a 2024 anchor keyed only by `3004.90.9208`.

### Consequence

The mapper could confidently assign a large split weight using observed trade that belongs to the wrong commodity. This is more dangerous than an even fallback because it appears data-driven and may survive all conservation and panel-coverage checks.

### Required conceptual change

The mapper should treat a code identity as versioned, for example:

```text
(hts10, identity_effective_from)
```

rather than treating `hts10` alone as the node in the transfer graph.

At a minimum, define an eligibility rule for historical shares:

> Historical imports keyed to code `x` may be used for a successor identity at date `d` only if the code's pre-`d` identity is demonstrably the same identity as the successor.

An immediately implementable conservative rule is:

- If a destination code is also an outgoing old code on the same effective date, mark the destination as `same_date_reuse = TRUE`.
- Do not use raw 2024 or 2025 direct-code values for that new destination identity.
- Fall through to a semantically valid family-level or even allocation.

For changes effective in 2026, newly established successor identities generally have no valid 2025 direct history. A reused numeric code must not be mistaken for an exception.

### Suggested schema additions

Add fields such as:

```text
action_new
same_date_reuse
old_identity_id
new_identity_id
```

The committed CSV can remain edge-oriented, but the loader should derive stable identity IDs before propagation.

### Split-share eligibility metadata

The mapping audit should distinguish:

```text
country_2025_identity_valid
all_country_2025_identity_valid
anchor_2024_identity_valid
family_fallback
even_fallback
```

This is more informative than reporting only `country_2025`, because it shows whether the historical observation was semantically eligible.

### Required tests

- Old `A -> B` and old `B -> C` on the same date.
- Another old source splitting into newly reused `B` on that same date.
- Nonzero 2025 trade recorded under old identity `B`.
- Assertion that the old `B` trade is not used to allocate to newly reused `B`.
- Assertion that simultaneous propagation still routes old `A` to new `B`, not to `C`.
- A later-date reuse that creates an apparent string-level cycle but is valid chronologically.

### Cycle-validation clarification

The phrase "cycles fail loudly" should be narrowed. A global graph over raw code strings can contain apparent cycles because codes are reused in later years. Validation should reject impossible cycles within one effective-date transition or within the versioned identity graph, not any repeated string-level path across all dates.

---

## 2.3 The model-side coverage guard is reversed

### Problem

The plan proposes counting panel `(hts10, country)` rate cells absent from the weights universe. That metric does not measure the diagnosed denominator failure.

The diagnosed tracker failure is:

```text
weight dollars that fail to find a rate row
```

The proposed metric is:

```text
rate rows that have no positive import weight
```

Many panel cells legitimately have zero observed imports and are therefore intentionally absent from the published weights file. The tracker weights publication explicitly omits zero-import pairs.

In addition, current Tariff-Model `build_pair_frame()` no longer silently inner-joins the panel to weights. It drives from the weights universe, left-joins scenario and baseline rates, then applies prefix fallbacks and asserts coverage.

### Correct metric

For each scenario or baseline date, calculate exact weighted coverage before any fallback:

```text
exact_matched_imports = sum(imports where exact rate exists)
total_imports         = sum(imports)
exact_coverage_pct    = exact_matched_imports / total_imports
```

Then report import dollars filled at each fallback level:

```text
exact
HS8
HS6
HS4
HS2
country/global fallback
unresolved
```

The diagnostics should include:

- number of weighted pairs;
- import dollars;
- percentage of total import dollars;
- top HS2 chapters;
- top countries; and
- sample HTS10 codes.

### Recommended behavior

For a tip-vintage weights file explicitly keyed to the tip panel:

- Exact HTS10 coverage should be effectively 100%.
- Any prefix fallback should be treated as a build or interface-contract problem.
- Production should hard-fail rather than warn at an arbitrary 5% cell-count threshold.

For historical slices evaluated with tip weights, prefix fallback may be expected. Those cases should report value-weighted fallback usage and compare it to an explicit policy threshold.

### Secondary descriptive metric

Panel cells absent from weights can still be reported as descriptive information, but it should be labeled clearly as panel sparsity or zero-trade coverage. It must not be presented as the denominator-protection guard.

### Required model tests

- Many unweighted panel rows plus full weighted-pair rate coverage: guard passes.
- One high-value weighted pair missing from the rate panel: guard fails even if the missing row count is tiny.
- One low-value historical suffix filled at HS8: diagnostic records the correct dollars and tier.
- Tip `hts_vintage` with any prefix fallback: hard failure.

---

## 2.4 Country-specific allocation requires a country-specific audit

### Problem

The plan specifies country-specific 2025 successor shares as the first allocation tier, but retains the existing country-agnostic crosswalk contract and says `split_weight` sums to one per old code.

If Canada and Mexico have different observed successor compositions, the applied split weights differ by country. One row per `(old_hts10, new_hts10)` cannot represent both mappings.

### Required audit schema

The applied mapping audit should contain at least:

```text
base_hts10
cty_code
new_hts10
split_weight
method
share_source
path
as_of_date
hts_vintage
```

The normalization assertion becomes:

```text
sum(split_weight) == 1
for each (base_hts10, cty_code)
```

### Composed versus immediate edges

The audit should distinguish:

- immediate transfer edges used at one effective date; and
- the composed end-to-end map from the original 2024 code to the target interval code.

Suggested outputs:

```text
crosswalk_edges_applied       # detailed transition log
crosswalk_composed_by_country # actual mapping applied to base dollars
crosswalk_summary             # optional all-country compact publication
```

If only one file is retained, it must be the country-specific composed mapping because that is what is necessary to reproduce the output.

### Required tests

- Same old code and successor family for two countries with different shares.
- Split weights sum to one within each country.
- Country totals are conserved.
- The published summary, if present, is clearly identified as aggregated and is not used to reproduce mapping.

---

## 2.5 Strict two-sided validation needs explicit source-document overrides

### Problem

The proposed parser uses both discontinued/annotated rows and established rows as a consistency check. That is a good idea, but the official source text appears to contain internal discrepancies.

Examples observed in the locally extracted January 2026 document include apparent pairs such as:

```text
0710.80.9724 -> 0709.80.9725   # one side
0710.80.9724 -> 0710.80.9725   # established side / archive universe
```

and:

```text
2008.99.1000 -> 2008.19.1090   # one side
2008.99.1000 -> 2008.99.1090   # established side / archive universe
```

These should be independently confirmed against the rendered PDF, but they illustrate why parser correctness and source correctness must be treated separately.

### Consequence

A strict consistency check without an override workflow will either:

- make the builder unusable on official source errors; or
- tempt implementation code to silently choose one side.

Silent correction would undermine the purpose of a principled, auditable crosswalk.

### Required override design

Add a committed file such as:

```text
resources/hts10_484f_overrides.csv
```

Suggested schema:

```text
source_doc
source_row_id
old_hts10_as_printed
new_hts10_as_printed
corrected_old_hts10
corrected_new_hts10
reason
evidence
approved_by
```

The builder should:

1. Parse the source literally.
2. Run two-sided consistency and archive-universe checks.
3. Surface every discrepancy.
4. Apply only explicitly listed overrides.
5. Fail if an override no longer matches the source or is unused.
6. Include the override ID in the resulting transfer row.

### Required tests

- A known inconsistent two-sided fixture fails without an override.
- The same fixture succeeds with an exact override.
- An unused or stale override fails.
- The generated resource retains the source values and override identifier for audit.

---

## 2.6 Crosswalk timing should use HTS identity, not policy-retimed interval dates

### Problem

The proposed `imports_fn(revision, valid_from, panel_codes)` uses `valid_from` as the `as_of_date` for transfer application. In the tracker, `valid_from` is often derived from `policy_effective_date`, while the underlying snapshot is associated with an HTS revision and code universe.

Those dates are conceptually different:

- 484(f) effective date: when a statistical classification transition takes effect.
- HTS revision/release identity: the product-code universe present in a snapshot.
- Policy-effective date: when a tariff policy represented by that snapshot is placed on the daily timeline.

The current `config/revision_dates.csv` explicitly stores both raw and policy-effective dates, and many revisions are retimed for policy purposes.

### Consequence

If a revision is moved earlier or later on the policy timeline, using daily `valid_from` could apply a product-code transition before or after the snapshot actually contains its successor universe.

The final `panel_codes` residual fallback might still force all dollars on-panel, masking the timing error as successful coverage.

### Required plan change

Introduce an explicit HTS identity input, for example:

```text
imports_fn(revision, hts_as_of_date, valid_from, panel_codes)
```

or a metadata object:

```text
interval_context = list(
  revision = ...,
  hts_as_of_date = ...,
  policy_valid_from = ...,
  panel_codes = ...
)
```

Synthetic boundary snapshots should inherit `hts_as_of_date` and the code identity of their owning real revision.

The mapper should validate that the selected dated transfer state agrees with the target code universe:

- outgoing old codes expected to disappear are absent;
- expected successors are present;
- unexpected disagreements are reported before residual prefix mapping.

### Required tests

- Snapshot HTS date differs from policy `valid_from`.
- Synthetic revision shares its owner's HTS universe.
- Using the policy date would select the wrong mapping state; using `hts_as_of_date` produces exact target coverage.

---

## 3. High-priority design and implementation gaps

## 3.1 Archive discovery currently omits gzip-only revisions

The plan says validation will scan:

```text
data/hts_archives/*.json
```

The current tracker tree includes revisions available only as `.json.gz`, including the latest 2026 revisions. A literal `*.json` scan would omit those transitions, including the July 2026 change that the plan explicitly intends to validate.

### Required change

Use the repository's archive resolution logic or implement discovery for both:

```text
*.json
*.json.gz
```

Deduplicate revisions when both forms are present. Validation should report the exact ordered revision set it inspected.

### Required test

A fixture directory containing:

- one uncompressed-only revision;
- one compressed-only revision; and
- one revision in both formats.

The test should assert that all three logical revisions are inspected exactly once.

---

## 3.2 The coverage requirement may be broader than the legal source

The plan requires every code disappearing between every pair of consecutive cached HTS universes to appear in the 484(f) crosswalk.

That may be too broad. Not every HTS change is necessarily a 484(f) statistical transfer. Revisions may contain:

- tariff-policy changes;
- legislative changes;
- proclamation changes;
- technical corrections;
- editorial corrections;
- classification changes from another source; or
- Chapter 98/99 changes that are outside the ordinary import-weight universe.

### Required change

Define the validation universe precisely:

1. Restrict to ten-digit ordinary merchandise codes after the same Chapter 98/99 exclusions used by the weight base.
2. Associate each expected 484(f) document with the target HTS transition it is intended to explain.
3. Validate disappearing and established codes for those transitions.
4. Permit only explicit, documented exceptions for changes outside 484(f).

### Recommended reports

The builder should separately report:

```text
covered_disappearances
covered_establishments
uncovered_disappearances
uncovered_establishments
documented_non_484f_exceptions
source_edges_not_observed_in_archives
```

Both directions matter. A transfer document could contain a successor edge that does not appear in the cached target universe, which should be investigated rather than ignored.

---

## 3.3 A schema-version bump is not sufficient cache invalidation

### Current behavior

Daily-part validity currently depends mainly on:

- part existence;
- part modification time relative to the snapshot;
- revision;
- interval dates; and
- weighted versus unweighted mode.

### Problem

Bumping `schema_version` from 1 to 2 invalidates the existing cache once. It does not protect future v2 caches from changes to:

- the committed 484(f) CSV;
- the overrides file;
- the 2024 base weights;
- the 2025 share file;
- the mapping algorithm;
- the configured mapping method; or
- the HTS date assigned to a snapshot.

None of those inputs necessarily changes the snapshot file's modification time.

### Required cache fingerprint

Store a weight-build fingerprint in every daily part. For example:

```text
weight_metadata:
  method: 484f
  mapper_schema_version: 1
  base_sha256: ...
  shares_sha256: ...
  crosswalk_sha256: ...
  overrides_sha256: ...
  hts_as_of_date: ...
  target_code_hash: ...
```

`target_code_hash` can be the hash of the sorted unique panel code vector.

The gather validator should compute the expected fingerprint and reject any mismatch.

### Operational alternative

If hashing a large RDS in every array task is too expensive, compute the hashes once in the build orchestrator and pass them in a small build-context file. The validator must still compare stable content fingerprints rather than relying only on path or modification time.

### Required tests

- v1 part rejected.
- v2 part with matching fingerprint accepted.
- Same snapshot but changed crosswalk hash rejected.
- Same snapshot but changed shares hash rejected.
- Same files but changed mapper version rejected.
- Unweighted part remains valid without weight input hashes.

---

## 3.4 The proposed acceptance ratio is nearly tautological

The plan proposes acceptance based on:

```text
matched_imports_b / total_imports_b >= 0.99
```

But the mapping design sends `no_successor` and all off-panel residuals through a prefix cascade, while hard validation requires all output codes to be on-panel. After that process, high exact join coverage is expected by construction.

The ratio verifies the final mechanical join, but does not measure whether dollars were mapped using authoritative transfers or broad heuristic fallback.

### Required quality metrics

For every interval, report dollar shares by end-to-end method:

```text
identity
484f_exact
484f_with_country_2025_share
484f_with_all_country_2025_share
484f_with_valid_2024_anchor
484f_even
no_successor_prefix_HS8
no_successor_prefix_HS6
no_successor_prefix_HS4
no_successor_prefix_HS2
global_fallback
```

Also report whether any composed path includes a heuristic step.

### Recommended acceptance gates

Exact numeric thresholds should be selected after an A/B dry run, but production should at least require:

- overall and per-country dollar conservation within tolerance;
- 100% final target-code validity;
- zero global fallback;
- zero unknown-nomenclature rows;
- zero unapproved two-sided discrepancies;
- zero use of historical shares for same-date reused identities;
- a reviewed maximum dollar share using any prefix fallback;
- a reviewed maximum dollar share using even allocation; and
- direct reconciliation of daily ETR to an independent dot product.

The headline expectation of approximately 9% is a reasonableness check, not a correctness gate.

---

## 3.5 The manifest lacks the revision needed for the proposed assertion

The tracker stamps `hts_vintage` into the published weights file from the tip snapshot revision. However, the published manifest's per-snapshot records currently include dates, path, hash, size, and row count but omit `revision`.

Therefore Tariff-Model cannot compare the weight file's `hts_vintage` to a manifest tip revision without a tracker manifest change.

### Required tracker change

Add `revision` to every manifest snapshot record:

```json
{
  "revision": "2026_rev_11",
  "valid_from": "2026-07-01",
  "valid_until": "2026-12-31",
  "path": "actual/snapshots/valid_from=2026-07-01/rates.parquet",
  "sha256": "...",
  "size_bytes": 123,
  "n_rows": 456
}
```

### Required model assertion

Before dropping metadata columns from the raw weights:

1. Read the tracker vintage's `manifest.json`.
2. Select the actual-series snapshot with maximum `valid_from`.
3. Assert that manifest `revision` is present and nonempty.
4. Assert that raw weights contain exactly one non-NA `hts_vintage`.
5. Assert equality between the two values.
6. Include both values and paths in any failure message.

### Additional useful assertion

The manifest's `weights.path` should resolve to the same file that `resolve_weights_path()` selected. This detects stale alternate files within the weights directory.

---

## 3.6 Published provenance is incomplete

The plan says interval weights are reproducible from the base, crosswalk, and shares, but proposes publishing only the tip weight file and transfer CSV. The 2024 base and 2025 share artifact remain local ignored files.

### Consequence

The published bundle does not contain enough information to reproduce its historical daily weights, and a manifest field containing a local `split_shares` path would not help another consumer.

### Required provenance

At minimum, record:

```text
base:
  source_year
  value_type
  source_description
  sha256
  total_imports
  row_count

shares:
  source_year
  value_type
  source_description
  sha256
  total_imports
  row_count

crosswalk:
  path
  sha256
  row_count

overrides:
  path
  sha256
  row_count

mapper:
  method
  schema_version
```

### Preferred option

Publish a normalized split-share artifact sufficient to reproduce the share calculation, if its size and disclosure properties are acceptable. If it is not published, record its content hash and generation command and state explicitly that the bundle is provenance-complete but not self-contained for regeneration.

Source PDF SHA-256 values should also be recorded in the generated crosswalk or a sidecar manifest.

---

## 3.7 Tracker GTAP category mapping remains suffix-vintage-sensitive

The tracker daily category aggregation currently joins `resources/hs10_gtap_crosswalk.csv` by exact ten-digit code and labels missing mappings as `unmapped`.

Revision-aware mapping will move more weight onto newly established HTS10 codes. If the exact HS10 GTAP resource is stale, those codes will appear in the `unmapped` category even though GTAP assignment is stable at HS6.

Tariff-Model already uses an HS6-to-GTAP mapping on the principle that GTAP sectors are determined by the stable six-digit international classification.

### Required change

Choose one of:

1. Replace the tracker's exact HS10 category join with the stable HS6 mapping; or
2. Extend and validate the HS10 mapping for every interval code universe.

The HS6 approach is preferable because it removes recurring statistical-suffix maintenance.

### Required tests

- Newly established HS10 suffix absent from the old exact crosswalk still resolves through HS6.
- No positive-weight row is assigned to `unmapped` when its HS6 is known.
- Weighted category numerators aggregate back to the overall weighted numerator.
- Category denominators sum to total imports.

---

## 3.8 `--no-gtap` requires more plumbing than the plan lists

The current `build_import_weights()` function:

- requires the GTAP crosswalk to exist before parsing;
- always reports the crosswalk;
- always performs the join;
- drops unmapped rows;
- expects `gtap_code` in result summaries; and
- uses a default output filename containing `gtap`.

Adding `--no-gtap` should therefore update:

- CLI parsing and help;
- function signature;
- crosswalk existence validation;
- logging;
- result construction;
- result summary;
- default output naming;
- audit-file behavior; and
- tests for both modes.

The no-GTAP output should have an explicit schema:

```text
hs10, cty_code, imports
```

and the configured `split_share_imports` loader should assert exactly the required columns and collapse duplicates safely.

`load_local_paths()` defaults and documentation must be updated, not only `config/local_paths.yaml.example`.

---

## 3.9 Hard-fail behavior needs explicit weight-mode plumbing

The current publisher attempts the weights build inside a warning-producing `tryCatch`. The plan appropriately proposes hard failure for weighted production runs and an explicit unweighted escape hatch.

However, `publish_vintage.R` currently invokes `write_build_output()` without a reliable weight-mode field. The publisher cannot infer whether a missing weight artifact is authorized merely from the files it sees.

### Required change

Persist weight mode in build metadata and thread it through publication:

```text
weight_mode = required | unweighted
weight_method = 484f | prefix
```

The manifest should record both. Publication rules should be:

- `required`: any failure to build or publish weights aborts finalization.
- `unweighted`: do not attempt a weighted artifact; publish `weights.present = false` with an explicit reason.

Avoid inferring unweighted mode from a missing file because that converts accidental omissions into authorized behavior.

---

## 4. Parser and resource design recommendations

## 4.1 Recommended transfer-resource schema

The proposed resource schema is too small for the required audit and identity semantics. A more robust committed edge resource would be:

```text
old_hts10
new_hts10
effective_date
change_type
partial
action_old
action_new
nomenclature
same_date_reuse
source_doc
source_doc_sha256
source_page
source_row_id
override_id
```

Notes:

- `new_hts10` may be blank only for a deliberate `no_successor` record.
- `nomenclature` must be `HTS` for production rows.
- `action_old` and `action_new` preserve both sides of the transfer assertion.
- `source_row_id` provides a stable identifier for overrides and golden tests.
- `same_date_reuse` may be derived by the builder rather than stored, but it should appear in diagnostics.
- `partial` should be edge-specific and should not be interpreted as permission to lose part of the source dollar mass.

## 4.2 Normalize HTS codes at ingestion

The parser should normalize dotted codes to ten-digit strings only after preserving the literal source value:

```text
source_code = 3004.90.9208
normalized_code = 3004909208
```

Validation should reject:

- wrong digit counts;
- nonnumeric normalized codes;
- accidental spaces within codes unless covered by a known normalization rule;
- self-edges within one versioned transition; and
- blank destinations except explicit `no_successor` rows.

## 4.3 Preserve both sides before deduplication

Discontinued and established rows often express the same edge from opposite directions. The parser should first emit assertions such as:

```text
edge_assertion(old, new, side = "old_action")
edge_assertion(old, new, side = "new_action")
```

Then reconcile them into a canonical edge. This makes it possible to distinguish:

- confirmed on both sides;
- present only on the old side;
- present only on the new side;
- inconsistent and overridden; and
- inconsistent and unresolved.

That reconciliation status should be available in builder diagnostics.

## 4.4 Manual overrides should be data, not parser conditionals

Document-specific corrections must not appear as hard-coded `if source_doc == ...` branches inside parsing code. Keeping corrections in a committed override table makes them reviewable, testable, and removable if a corrected source PDF is later published.

## 4.5 Source-document checksums

The builder should maintain a small source manifest:

```text
source_doc, sha256, expected_pages, expected_effective_dates
```

This protects against silently replacing a PDF under the same filename.

---

## 5. Mapping algorithm recommendations

## 5.1 Normalize and validate inputs

Before mapping:

1. Normalize base columns to `hs10`, `cty_code`, and numeric `imports`.
2. Collapse duplicate `(hs10, cty_code)` rows.
3. Drop nonpositive rows under the existing contract.
4. Record overall and country totals.
5. Normalize the target code set.
6. Validate the transfer resource and share data.
7. Establish `hts_as_of_date` and target revision identity.

## 5.2 Apply each effective date transactionally

For each transfer date `d <= hts_as_of_date`:

1. Snapshot the complete pre-date state.
2. Build the outgoing edge family for every old identity.
3. Determine split shares using only semantically eligible evidence.
4. Generate destination contributions from the pre-date snapshot.
5. Preserve unaffected code identities.
6. Replace outgoing identities and add all destination contributions simultaneously.
7. Collapse duplicate `(identity, country)` rows.
8. Assert overall and per-country conservation.
9. Record the transition audit.

Do not mutate one source row at a time in a way that permits newly created mass to be consumed by another edge on the same date.

## 5.3 Recommended split-share ladder

The user-selected hierarchy can be retained with semantic eligibility added:

1. Country-specific 2025 successor imports, only for valid successor identities.
2. All-country 2025 successor imports, only for valid successor identities.
3. Valid 2024 successor anchors, excluding reused or redefined identities.
4. A documented family-level proxy, if one can be defined without using the wrong identity.
5. Even allocation.

Each family should record why a higher tier was unavailable.

## 5.4 Handling zero observations

The implementation should distinguish:

- successor identity did not exist in the observation year;
- successor existed but had zero observed imports for one country;
- successor existed and the entire family had zero imports; and
- numeric code existed but represented another identity.

These cases should not all collapse to a generic zero.

## 5.5 Residual prefix fallback

Residual prefix mapping can remain as a conservation-preserving last resort, but it should be constrained:

- Apply only after all dated transfers.
- Record the exact prefix level and dollars.
- Never use whole-panel fallback in production without an explicit failure or approval.
- Treat a large residual share as a failed crosswalk, even if final coverage is 100%.
- Keep `no_successor` distinct from an unexpectedly uncovered source code.

An unexpectedly uncovered source should not be mislabeled as an official deletion without successor.

## 5.6 Validation after final mapping

Required assertions:

```text
all output hts10 in target panel codes
no duplicate (hts10, cty_code)
no NA keys
no NA imports
all imports > 0
overall total conserved
each country total conserved
each composed split sums to one by (base_hts10, cty_code)
no ineligible historical share used for a reused identity
```

---

## 6. Daily-series integration recommendations

## 6.1 Prefer an explicit weight-provider interface

Instead of loosely accepting either a tibble or callable through one argument, define a clear contract. For example:

```r
build_daily_aggregates(
  ts,
  imports = NULL,
  imports_fn = NULL,
  policy_params = NULL,
  stacking_method = "mutual_exclusion"
)
```

Assert that callers do not supply both `imports` and `imports_fn`.

The callback should return both weights and metadata:

```r
list(
  weights = tibble(...),
  stats = list(...),
  fingerprint = list(...)
)
```

## 6.2 Denominators become interval-specific objects

When interval weights are used, compute these per interval:

- total imports;
- country totals;
- GTAP-sector totals; and
- Budget Lab HS-category totals.

Overall and country totals should remain invariant because product mapping preserves country. Category totals may change if the category mapping itself is suffix-sensitive; moving GTAP to HS6 should eliminate that problem for GTAP sectors.

## 6.3 Avoid accidental closure over the wrong denominator

The current aggregation helper closures read `total_imports`, `country_total_imp`, `sector_total_imports`, and `category_total_imports` from outer scope. Moving weight construction into the revision loop requires either:

- passing an interval-specific weight context into each helper; or
- defining the helpers inside an interval-local scope.

Passing an explicit context is clearer and easier to test:

```r
weight_context <- list(
  weights = ...,
  total_imports = ...,
  country_totals = ...,
  sector_totals = ...,
  category_totals = ...
)
```

## 6.4 Combined, streaming, array, and alternative paths must agree

The plan correctly identifies multiple callers, but testing should cover all logical modes:

- combined in-memory timeseries;
- per-revision array part writer;
- cached-part gather;
- streaming snapshot aggregation;
- backfill-by-HS;
- alternative/scenario snapshot aggregation; and
- unweighted smoke mode.

At least one synthetic fixture should compare combined versus per-part outputs field-for-field.

---

## 7. Publishing and manifest recommendations

## 7.1 Tip weight build must pass all required mapping context

The publisher should explicitly pass:

```text
method = 484f
crosswalk_path
overrides_path
shares_path
hts_as_of_date
hts_vintage
panel_codes
```

Do not rely on `Sys.Date()` or an implicit default for `as_of_date`. The selected date and revision should come from the tip snapshot's HTS metadata.

## 7.2 Preserve output interface while making provenance richer

The main Parquet schema can remain:

```text
hts10
country
imports
import_value_year
hts_vintage
```

Additional provenance belongs in the manifest and audit files unless a downstream consumer needs it row-by-row.

## 7.3 Publish mapping resources under unambiguous names

Suggested files:

```text
weights/import_weights_hs10_country.parquet
weights/import_weights_hs10_country.csv.gz
weights/hts10_484f_transfers.csv
weights/hts10_484f_overrides.csv
weights/hts10_mapping_summary.csv
```

If the detailed country-specific composed map is too large to publish by default, record its hash and retain it as a build artifact. The manifest should not imply that a country-agnostic edge table is sufficient to reproduce country-specific allocation.

## 7.4 Stage and verify before moving `latest`

Publication should remain additive until all acceptance checks pass. A failed hard weight build may leave a partial named vintage directory, but it must not update `latest` or produce a misleading complete manifest.

---

## 8. Test plan revisions

The original test list is a useful start but should be expanded and wired into CI.

## 8.1 Parser tests

Required cases:

- Wrapped rows.
- Leading whitespace before codes.
- `transferred to` and `transferred from`.
- Parenthesis-spacing variants.
- `Discontinued`, `Deleted`, `Renumbered`, `Established`, and `Annotated`.
- `(pt.)` edge annotations.
- Comma-separated, `and`-separated, and wrapped successor lists.
- January 2026 `Renumbered As:` supplement table.
- HTS versus Schedule B context.
- Noise, headings, and page markers.
- Literal two-sided mismatch.
- Required explicit override.
- Unused override failure.
- Unknown nomenclature failure.

## 8.2 Resource validation tests

- Both `.json` and `.json.gz` archive discovery.
- Deduplication when both forms exist.
- Every expected disappearance covered.
- Every expected establishment covered or explicitly classified.
- Edge destination appears in the expected target universe.
- No unapproved source inconsistency.
- No self-edge within a versioned transition.
- Dates belong to the document's allowed dates.
- Source hashes match.
- Schedule B rows excluded.

## 8.3 Mapping tests

- Identity mapping.
- One-to-one rename.
- One-to-many split using each share tier.
- Many-to-one merge.
- Many-to-many component.
- Two-date chain composition.
- Same-date string reuse.
- Reused successor with misleading 2025 imports.
- Reused successor with misleading 2024 anchor.
- `hts_as_of_date` gating.
- Policy date differing from HTS date.
- Official `no_successor` residual.
- Unexpected uncovered code distinguished from `no_successor`.
- Each prefix fallback level.
- Prohibition or failure of whole-panel fallback.
- Overall and per-country conservation.
- Country-specific split audit normalization.
- Prefix-method regression compatibility.

## 8.4 Daily aggregation tests

- Two intervals around a code transition.
- Equal rates before and after yield continuous weighted ETR.
- Different rates yield the independently computed expected change.
- Exact matched imports equal total after valid mapping.
- Method-tier statistics expose heuristic fallback.
- Combined and per-part results are equal.
- Cache fingerprint mismatch rejects stale parts.
- Unweighted behavior remains unchanged.
- GTAP category rollups reconcile to overall.
- HS-category rollups reconcile to overall.

## 8.5 Tariff-Model tests

- Correct direction of weighted coverage diagnostics.
- High-value missing weight-to-rate mapping fails.
- Many zero-import panel cells do not cause a false alarm.
- Tip revision equals weight `hts_vintage`.
- Missing manifest revision fails with an actionable message.
- Multiple weight `hts_vintage` values fail.
- Manifest weight path and resolved weight path agree.
- Direct tip weighted rate equals pre-GTAP statutory aggregation.

## 8.6 CI wiring

The current tracker CI does not invoke `tests/test_panel_import_weights.R`, and that file imports `arrow`, which is not currently installed in CI.

Recommended options:

1. Split pure mapping tests from Arrow round-trip tests and run the pure suite in every CI job; or
2. Add Arrow to CI and run the complete suite.

Whichever option is chosen, CI must explicitly invoke:

```text
test_484f_parser
test_484f_coverage
test_panel_import_weights or its pure replacement
daily interval-weight fixture
```

Tariff-Model should receive its own targeted test script for the new guards, even if it does not currently have a GitHub Actions workflow.

---

## 9. Revised acceptance criteria

## 9.1 Crosswalk build acceptance

- All expected source PDFs present with matching SHA-256.
- Parser produces zero unknown-nomenclature production rows.
- Schedule B rows excluded from the HTS resource.
- All two-sided discrepancies either resolved by exact documented overrides or fail.
- Zero unused overrides.
- All scoped archive disappearances and establishments accounted for.
- Both compressed and uncompressed archives included.
- No impossible same-date cycles in the versioned identity graph.

## 9.2 Weight-map acceptance by interval

- Overall dollar conservation within `1e-9` relative tolerance.
- Per-country dollar conservation within the same tolerance.
- Every output code belongs to the target interval panel.
- No duplicate `(hts10, cty_code)` keys.
- No NA or nonpositive weight rows.
- Every composed country-specific split sums to one.
- Zero historical-share use for invalid/reused successor identities.
- Zero global fallback.
- Prefix and even-fallback dollar shares below reviewed thresholds.

## 9.3 Daily-series acceptance

- `matched_imports_b / total_imports_b` is effectively one for each interval.
- Independent dot product of interval weights and interval rates equals published `weighted_etr` within numerical tolerance.
- Country and category rollups reconcile to overall.
- Early-2025 movement is explainable by documented mapping differences.
- End-2026 movement is explainable by documented recovered dollars, not merely proximity to a target headline.
- Combined and per-part fixture tests agree.

## 9.4 Published vintage acceptance

- Manifest contains revision for every snapshot.
- Tip weight `hts_vintage` equals actual tip snapshot revision.
- Manifest hashes match published weights and mapping resources.
- Manifest records weight method, source years, hashes, mapper version, and weight mode.
- Verification is run against the named vintage before `latest` is updated.

## 9.5 Cross-repository acceptance

Compare like with like:

- Tariff-Model actual scenario versus tracker actual series.
- Tariff-Model `new_301` scenario versus tracker `new_301` series, if both are published.
- Exact same calendar date.
- Date away from policy expiry boundaries unless boundary behavior is the subject of the test.
- Statutory, pre-substitution, non-eta-adjusted model metric versus tracker statutory weighted level.

The direct algebraic comparison should be performed before relying on a tolerance such as 0.05 percentage point. Any residual should be decomposed into country universe, GTAP aggregation, date/scenario selection, and numeric rounding.

---

## 10. Revised rollout runbook

The original runbook should be reordered as follows.

### Phase 1: Source acquisition and parser design

1. Copy the four scratchpad PDFs into stable storage immediately; the `/tmp` paths are ephemeral.
2. Acquire the July 2024 document from the authoritative source.
3. Record SHA-256 and page count for all source documents.
4. Implement the context-aware parser with HTS/Schedule B distinction.
5. Parse both sides of transfer assertions without silently reconciling them.
6. Create documented overrides for confirmed source-document inconsistencies.
7. Add parser golden fixtures and tests.

### Phase 2: Crosswalk generation and validation

8. Discover both `.json` and `.json.gz` archive revisions.
9. Define exactly which revision transitions are expected to be explained by each 484(f) document.
10. Generate the committed transfer CSV.
11. Run two-sided, archive-universe, source-hash, nomenclature, and override validation.
12. Require zero unexplained scoped orphans.
13. Commit PDFs, source manifest, parser, overrides, generated CSV, and tests together.

This resolves the contradiction in the original runbook where step 1 commits the CSV before step 3 generates it.

### Phase 3: Share data and mapper

14. Implement and test `--no-gtap` completely.
15. Build the 2025 HS10-by-country share file.
16. Record its SHA-256, row count, total, year, and value type.
17. Implement versioned-identity mapping and share eligibility rules.
18. Implement country-specific composed audits.
19. Add conservation, fallback, reuse, and timing tests.
20. Run latest-tip A/B comparison against prefix mapping.
21. Review dollar shares by method and establish production thresholds.

### Phase 4: Daily integration and cache contract

22. Add the explicit interval weight-provider interface.
23. Thread HTS identity separately from policy interval dates.
24. Update array, gather, streaming, backfill, alternative, and monolithic callers.
25. Add input fingerprints to daily-part metadata.
26. Bump the daily-part schema version.
27. Add combined-versus-parts parity tests.

### Phase 5: Publishing and model guards

28. Add snapshot `revision` to tracker manifest records.
29. Add complete weights provenance and hashes.
30. Plumb `weight_mode` and weight method into final publication.
31. Make weighted publication fail hard.
32. Implement the correctly directed, value-weighted Tariff-Model coverage guard.
33. Implement manifest-tip versus weight-`hts_vintage` assertion.
34. Add Tariff-Model tests.

### Phase 6: Full staged rebuild

35. Land the existing `eta-diagnostic-export` work before scenario-vintage changes.
36. Submit the full tracker array rebuild.
37. Gather with fingerprint validation.
38. Run tracker verification and all new acceptance reports.
39. Publish the new named vintage with `latest` unchanged.
40. Independently recompute tip and selected historical daily ETRs from weights and rates.
41. Review method/share-source dollar tables for every interval.

### Phase 7: Downstream model rollout

42. Merge the Tariff-Model guard work.
43. Update the four scenario vintage pins in a separate commit.
44. Rerun the actual and `new_301` model scenarios.
45. Compare each model result to the corresponding tracker series on the same date.
46. Refresh dashboard outputs only after reconciliation passes.

### Phase 8: Production cutover

47. Re-run final named-vintage verification.
48. Repoint tracker `latest` only after every acceptance gate succeeds.
49. Verify downstream consumers resolve the intended named vintage or new `latest` target.
50. Preserve the A/B reports, method-tier summaries, manifests, and job IDs as rollout artifacts.

---

## 11. File-by-file amendments to the original plan

## tariff-rate-tracker

### `data/484f/`

Add:

- five source PDFs;
- a source manifest with hashes and expected metadata; and
- documentation that the documents contain both HTS and Schedule B content.

### `tools/build_484f_crosswalk.R`

Add:

- context-aware nomenclature state;
- literal two-sided assertion parsing;
- source page/row IDs;
- explicit override application;
- `.json` and `.json.gz` archive discovery;
- scoped transition validation;
- source hash validation; and
- failure on unused overrides.

### `resources/hts10_484f_transfers.csv`

Expand schema to preserve:

- old and new actions;
- nomenclature;
- source row/page;
- source hash; and
- override identity.

### `resources/hts10_484f_overrides.csv`

Add as a new committed, human-reviewed resource.

### `src/io/build_import_weights.R`

Implement complete `--no-gtap` mode, including CLI defaults, validation, output schema, and tests.

### `src/model/policy_params.R`

Add `split_share_imports` to `load_local_paths()` defaults and documentation. Validate configured paths and keep weight mode explicit.

### `src/io/build_panel_import_weights.R`

Add:

- versioned identity semantics;
- semantically eligible share selection;
- country-specific audit output;
- per-country conservation;
- explicit HTS as-of context;
- richer method/fallback statistics; and
- input fingerprints.

### `src/pipeline/09_daily_series.R`

Add:

- explicit `imports_fn` contract;
- interval-local weight contexts;
- fingerprinted cache metadata;
- schema-version validation;
- HS6 GTAP resolution or an equivalent suffix-stable category mapping; and
- direct reconciliation diagnostics.

### `scripts/build_revision.R`

Build the interval weight context using the snapshot's HTS identity and include its fingerprint in the daily part.

### `scripts/build_gather.R`

Validate expected fingerprints, not only snapshot timestamps, dates, and weight mode.

### `src/io/write_output.R`

Add:

- hard-fail behavior keyed to explicit weight mode;
- explicit tip `hts_as_of_date` and revision inputs;
- snapshot revision in the manifest;
- complete source hashes and mapper provenance; and
- published overrides and mapping summary resources.

### `.github/workflows/ci.yml`

Run all new pure parser, resource, and mapping tests. Decide explicitly how the Arrow round-trip test is handled.

## Tariff-Model

### `src/00a_prepare_rate_inputs.R`

Replace the proposed panel-to-weight cell-count guard with:

- weights-to-rates exact dollar coverage;
- fallback-tier dollar diagnostics;
- strict tip coverage behavior; and
- raw weight `hts_vintage` versus manifest tip revision assertion.

### `src/read_rate_panel.R`

Consider adding a manifest resolver/reader alongside the existing panel, snapshot, and weights path helpers so manifest validation is centralized.

### Tests

Add focused tests for coverage direction, high-value missing mappings, manifest revision matching, and multiple/NA `hts_vintage` values.

### Scenario configuration

Keep scenario vintage updates in a separate commit after existing uncommitted scenario and dashboard-guide work is landed.

---

## 12. Recommended decisions to lock before coding

The following decisions should be made explicit in the revised plan:

1. **Schedule B policy:** Parse and classify but exclude, or skip its sections entirely. Recommended: classify and exclude so tests prove the distinction.
2. **Source-error policy:** Require committed overrides. Recommended: yes, with unused-override failure.
3. **Code identity model:** Raw strings plus reuse flags, or fully versioned nodes. Recommended: versioned nodes internally, flat codes only at input/output boundaries.
4. **2026 share policy:** Whether all newly established 2026 identities automatically bypass direct 2025 shares. Recommended: yes unless continuity is positively established.
5. **Fallback production thresholds:** Set after A/B diagnostics, but zero global fallback should be fixed now.
6. **Audit publication:** Publish detailed country-specific map or retain it as a hashed build artifact. Recommended: retain/publish enough to reproduce actual country-specific allocation.
7. **HTS timing source:** Add explicit `hts_as_of_date` to snapshot/build metadata. Recommended: do not reuse policy `valid_from` implicitly.
8. **GTAP mapping:** Update exact HS10 resource or move tracker categories to HS6. Recommended: move to stable HS6.
9. **Bundle reproducibility:** Self-contained versus hash-provenance only. Recommended: publish normalized share inputs if practical; otherwise clearly document non-self-contained reproducibility.
10. **Model coverage behavior:** Warn or fail. Recommended: hard-fail exact tip-vintage weighted coverage failures; report controlled historical fallback.

---

## Final assessment

The plan's main direction should be retained: authoritative dated transfers, committed generated data, revision-aware tracker weighting, a stable tip-vintage model interface, and a staged full rollout.

The most important corrections are semantic rather than mechanical. The implementation must distinguish HTS from Schedule B, distinguish a numeric code from a commodity identity, prevent old trade under a reused code from becoming false successor evidence, and measure coverage in import dollars from weights to rates. Without those corrections, the pipeline could conserve every dollar, achieve 100% final joins, and still allocate material value to the wrong commodities.

Once those issues, cache fingerprints, manifest metadata, CI wiring, and staged acceptance are added, the proposal will be substantially safer and genuinely auditable.
