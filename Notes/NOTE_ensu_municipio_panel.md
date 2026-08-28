# Methodological note: building the ENSU municipality panel (2013–2020)

## Problem

ENSU microdata files (`ENSU_VIV_[mm][aa]`) do not use a consistent column naming
scheme for geographic identifiers across the 2013–2020 period. Four different naming
schemes were found across the 28 available quarters (55 VIV files total, including
ECOSEP):

| Scheme | Columns | Period observed |
|---|---|---|
| 1 | `ENT` + `MPIO` | 2013–2015 |
| 2 | `CVE_ENT` + `CVE_MUN` (separate) | 2016–2020 (most files) |
| 3 | `ENT` + `MUN` | A subset of 2016 quarters |
| 4 | `CVE_MUN` (already combined, 5 digits) | Fallback, not needed in this dataset |

Additionally, ECOSEP (2010–2013 H1, the predecessor survey) only reports geography at
the **state** level (`ENT`), with no municipality-level field at all. ECOSEP files were
identified automatically (file names matching `segpub_*`) and excluded from the
municipality panel construction — they remain useful only as a potential state-level
robustness source for 2010–2013 H1, outside the main 2013–2020 window.

A second, more serious risk was identified and ruled out: the `CD` field (ENSU's
internal "city of interest" code) is **not time-invariant** — it was renumbered as
INEGI expanded survey coverage from 32 cities (2013) to 91 cities (2024). For example,
Morelia is `CD == 15` in a 2013 file but `CD == 29` in the 2019 data dictionary. Using
`CD` as a panel key would have silently merged unrelated cities across years. For this
reason, `CD` was abandoned as a geographic key in favor of directly reconstructing the
5-digit INEGI municipality code (`cve_mun` = state code + municipality code), which is
stable over time by construction.

## Method

1. All `.dbf` files under the project's ENSU/ECOSEP folder were inventoried
   automatically (`list.files(..., recursive = TRUE)`), classified by survey (`ENSU` vs
   `ECOSEP`) via filename pattern, and by file type (`VIV`/`CB`/`CS`) via filename
   substring.
2. Each `VIV` file was tagged `"ciudad"` (has a `CD` field, i.e. ENSU with
   city-of-interest design) or `"estado"` (no `CD` field, i.e. ECOSEP) — this cleanly
   separates the two surveys without relying on folder structure.
3. A single function (`construir_cve_mun()`) attempts, in order, all four column-naming
   schemes above and builds `cve_mun` as `str_pad(ENT, 2, "0")` +
   `str_pad(MPIO/MUN/CVE_MUN, 3, "0")`. If none of the schemes match a given file, the
   function does not fail the pipeline — it returns an empty result and emits a warning
   with the file's actual column names, so mismatches can be diagnosed and patched
   incrementally rather than causing a hard crash partway through 28 files.
4. Verification: Morelia's known INEGI code (`16053`) was confirmed present in the
   output for a file where the scheme was known, validating the construction logic
   before applying it to the full set.

## Result

- **28 valid ENSU quarters** (2013–2020) contributed to the panel; 0 files failed to
  resolve to a `cve_mun` after all four schemes were implemented.
- **62 municipalities** are present in *every* one of the 28 quarters (balanced panel).
- **198 municipalities** appear in *at least one* quarter (union / unbalanced panel).
- Morelia (`16053`) is present in both sets.

## Recommendation for downstream use

- Use the **balanced panel (62 municipalities)** as the main specification for
  perception-based outcomes, since it guarantees no missing quarters per unit.
- Use the **union panel (198 municipalities)** as a robustness check — `plm` handles
  unbalanced panels natively, so this is a low-cost way to confirm results aren't driven
  by the balance restriction.
- `cve_mun` is directly compatible with the keys already used in the NDVI (MODIS/GEE)
  and SESNSP homicide panels, allowing a direct `inner_join` across all three sources
  without needing a separate city-to-municipality crosswalk.

## Output files

- `data/municipios_balanceados_ensu.csv` / `.rds` — 62 municipalities, balanced panel.
- `data/municipios_union_ensu.csv` / `.rds` — 198 municipalities, union panel.
