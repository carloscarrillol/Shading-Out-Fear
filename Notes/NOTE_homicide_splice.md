# Methodological note: splicing SESNSP homicide series (2011–2020)

## Problem

SESNSP reports municipal crime incidence under two distinct methodologies:

- **1997–2017 methodology**: municipal-level breakdown available from 2011 onward.
  Single category `HOMICIDIOS` in the `modalidad` column, with no distinction between
  intentional (doloso) and negligent (culposo) homicide.
- **2015–2025 methodology** (CNSP/38/15 instrument): harmonizes categories with the
  National Technical Classification of Common-Jurisdiction Crimes, expanding from 66 to
  98 types/subtypes/modalities. Distinguishes `Homicidio doloso` and `Homicidio culposo`
  as subtypes under `tipo == "Homicidio"`.

Both series overlap in 2015–2017, published in parallel during the transition.

## Anchor category decision

**Total homicide (doloso + culposo combined)** was used as the crime variable, rather
than a broader category (robbery, property crimes, etc.), because:

1. It is the category least likely to be affected by administrative reclassification
   between methodologies (the underlying act doesn't change definition, unlike figures
   such as "despojo" or "abuso de confianza," which were redefined).
2. It is the standard anchor category in the literature on violence in Mexico for long
   panels.

To make it comparable with the old series (which does not distinguish intent), the new
series was collapsed by summing `Homicidio doloso` + `Homicidio culposo` into a single
municipality-year total.

## Splicing method

1. Computed the 2015–2017 overlap (years in which both methodologies are available) at
   the municipality level.
2. Computed a **national-level aggregate splicing factor** (not a municipality-level
   factor):

   ```
   factor = sum(homicides_new_methodology) / sum(homicides_old_methodology)
   ```

   The national aggregate was used instead of a per-municipality factor to avoid noise
   or undefined values (zero variance, non-computable correlation) introduced by
   municipalities with very low or near-constant homicide counts.

3. Splice quality was verified via:
   - National aggregate correlation between both series over the overlap period.
   - Distribution of correlations at the individual-municipality level.

### Verification results (2015–2017 overlap, n = 6,297 municipality-years)

| Metric | Value |
|---|---|
| Splicing factor | 0.969 |
| National aggregate correlation | 0.962 |
| Per-municipality correlation — median | 1.000 |
| Per-municipality correlation — mean | 0.981 |
| Per-municipality correlation — minimum | -1.000 |
| Municipalities with non-computable correlation (NA) | 719 |

A factor close to 1 combined with a high aggregate correlation confirms that the
methodological change had minimal impact on homicide counts specifically (unlike
categories more prone to reclassification). Cases of low, negative, or non-computable
correlation are concentrated in low-homicide-count municipalities (near-constant
zero series during the overlap window), do not reflect a systematic splicing failure,
and do not affect the aggregate factor, since it was computed volume-weighted rather
than as a simple average of municipality-level factors.

## Application

The factor was applied to rescale 2011–2014 (only available under the old methodology)
to the scale of the methodology in effect since 2015:

```
adjusted_homicides(municipality, year < 2015) = old_homicides(municipality, year) × factor
```

2015–2020 are kept unadjusted (original new methodology).

## Limitations and pending robustness checks

- The splicing factor is a single national value applied uniformly to all
  municipalities and years 2011–2014; it does not capture possible regional or temporal
  heterogeneity in how the two methodologies diverge.
- The main econometric specification should be run in two versions: (a) the full
  spliced panel 2011–2020, and (b) 2015–2020 only (single methodology, unadjusted) — as
  a robustness check. If results do not change in sign or significance between the two,
  the splice is not driving the result.
- This splice was performed for homicide only. If other crime categories are
  incorporated into the analysis (robbery, kidnapping, etc.), the overlap/factor
  exercise must be repeated for each — the homicide factor is not generalizable to other
  categories, which had a higher probability of reclassification between methodologies.

## Output file

`data/panel_homicidios_mx_2011_2020.csv` — municipality × year panel, total homicides
(doloso + culposo), spliced and scale-comparable across the full 2011–2020 period.
