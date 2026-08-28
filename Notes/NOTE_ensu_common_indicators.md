# Methodological note: homologating common insecurity indicators (ENSU 2013–2020)

## Problem

ENSU's questionnaire was redesigned at least once during the 2013–2020 window (2013–2015
"old" design, vs. 2016–2020 "new" design), with different column names for every
variable. Before pooling any variable into a single panel, each candidate had to be
checked against the *actual question wording* in both eras — matching column names alone
was not sufficient, since prior steps in this project already produced false matches
based on superficially similar naming (see the `CD` code-renumbering issue documented
separately).

The 2013 and 2020 questionnaires (official INEGI PDFs) were compared item by item to
determine which concepts are genuinely asked the same way across the full period.

## Variables confirmed common to all years (2013–2020)

| Concept | 2013 column(s) | 2016–2020 column(s) | Response scale |
|---|---|---|---|
| General insecurity in the city | `P1` | `BP1_1` | 1=safe, 2=unsafe, 9=DK/NA |
| Expects crime to worsen (next 12 months) | `P2` | `BP1_3` | 1=improve, 2=stay same-good, 3=stay same-bad, 4=worsen, 9=DK/NA |
| Witnessed antisocial behavior nearby (6 core items: vandalism, public drinking, robbery, gangs, drug dealing, gunfire) | `P3_1`–`P3_6` | `BP1_4_1`–`BP1_4_6` | 1=yes, 2=no, 9=DK/NA (per item) |
| Changed habits out of fear (5 items: carrying valuables, walking at night, visiting relatives, letting children out, other) | `P4_1`–`P4_5` | `BP1_5_1`–`BP1_5_5` | 1=yes, 2=no, 3=N/A, 9=DK/NA (per item) |

2020 expanded the "witnessed behavior" item to 8 sub-items (added illegal fuel
theft/"huachicol" and illegal power taps). Only the original 6 sub-items, present in
every wave, are used to keep the indicator comparable across the full period.

## Variable explicitly excluded: police effectiveness

An earlier pass assumed `P5` (2013, single-item police effectiveness rating) mapped to a
single column (`BP2_1`) in 2016–2020. This was wrong: `BP2_1` in the newer design is
actually a yes/no screening question for a different module (personal conflicts), not
police effectiveness. From 2016 onward, police-effectiveness is asked **per authority**
(municipal, state, federal, army, and — from 2019 — national guard), with the number of
authorities changing over time as the National Guard was created in 2019. There is no
clean single-column equivalent to 2013's combined "state and municipal police" question,
so this indicator was **not** included in the homologated panel rather than force an
inconsistent mapping.

## Geography

- 2016–2020 `CB` files carry `CVE_ENT`/`CVE_MUN` (or the unprefixed `ENT`/`MUN` variant
  seen in some 2016 files) directly, so `cve_mun` is built in place.
- 2013–2015 `CB` files carry no geography of their own. Each respondent record is
  matched to its corresponding `VIV` file (same survey quarter, identified by parent
  folder) via household-level keys (`ENT`+`FOL`+`CON`+`V_SEL`+`N_HOG`), inheriting
  `cve_mun` from there. This reuses the same `agregar_cve_mun()` geography-resolution
  logic (4 naming schemes) already validated for the municipality panel.

## Aggregation

All four indicators are aggregated to municipality-year, with non-valid responses
(9=DK/NA, and 3=N/A where applicable) excluded from each variable's own denominator
rather than treated as missing-safe or missing-unsafe:

- `prop_inseguro` — share reporting the city is unsafe.
- `prop_espera_empeorar` — share expecting crime to worsen in the next 12 months.
- `prop_atestiguo_alguno` / `indice_atestiguo` — share who witnessed at least one of the
  6 core antisocial behaviors nearby; average count witnessed (0–6 scale).
- `prop_cambio_habito` / `indice_habito` — share who changed at least one of the 5
  behaviors out of fear; average count changed (0–5 scale).

Each metric's underlying valid-response count (`n_valido_*`) is retained in the output
so that low-reliability municipality-year cells (few valid responses) can be flagged or
weighted in downstream analysis.

## Output files

- `data/panel_indicadores_raw_persona.csv` — respondent-level records with all four
  indicators plus `cve_mun`, `ano`, `mes`.
- `data/panel_inseguridad_municipio_ano.csv` — municipality-year panel, ready to merge
  with the NDVI and homicide panels via `cve_mun` + `ano`.
