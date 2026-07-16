# CAO 2026 — EDA Shiny app

A scrollable, report-style Shiny app for exploring
`data/clean/cao-2026-clean.csv`.

## Run

From the project root:

```r
shiny::runApp("analysis/eda-shiny")
```

Requires: `shiny`, `bslib`, `ggplot2`, `dplyr`, `tidyr`, `forcats`, `scales`,
`patchwork`, `DT`, `readr` (all already installed on this machine).

## Measurement levels (this drives everything)

Almost nothing in this instrument is truly continuous — most items are ordinal
Likert scales, so the app classifies every variable and picks measurement-
appropriate plots and statistics:

- **continuous / interval** (14): `yob` (age), `job_tenure`, `time_use_1..6`
  (% of time), and the 0–10 `ideology` / `anchor_ideo_*` scales (treated as
  interval). *The only items that get a scatter + line of best fit.*
- **ordinal** (45): the 4–5 point Likert items (`issue_*`, `incivility_*`,
  `legitimacy_*`, `strongmayor_effect_*`, `innovation_barriers_*`, …).
- **categorical** (13): coded nominal factors — `gender`, `race`, `prov_terr`,
  the comma-coded multi-selects, and the numeric-coded nominals `educ_valuable`
  (which credential) and `job_longest` (which sector).

## Sections

1. **Spearman correlation heatmap** — **Spearman's ρ** (rank correlation, right
   for ordinal data) across all continuous + ordinal items, optionally reordered
   by hierarchical clustering. The overview for spotting pairs worth examining.
2. **Bivariate comparison** — three pickers: **X variable**, **Y variable**, and
   **plot type**. Plot type defaults to **Auto**, which chooses the plot from the
   two variables' measurement levels, but you can force any of: scatter +
   lm/loess, box + violin, cross-tab heatmap, or 100 % stacked bars (with a
   guard message when a forced type doesn't fit the data). Scatter also has
   **colour-by** (categorical, per-group lm lines) and a **jitter** toggle. The
   association statistic under the plot always follows the measurement levels:
   - continuous × continuous → **Pearson r** (Auto → scatter + lm/loess)
   - continuous × ordinal → **Spearman ρ** (Auto → box + violin by level)
   - ordinal × ordinal → **Spearman ρ** (Auto → cross-tab heatmap, cells =
     counts shaded by row-conditional %)
   - numeric × categorical → **η² / ANOVA p** (Auto → box + violin)
   - categorical × categorical → **Cramér's V / χ² p** (Auto → 100 % stacked bars)

   A line of best fit therefore only appears on a Scatter of two continuous /
   interval items — the only place it is meaningful.
3. **Single-variable distribution** — histogram (continuous, with a **bins
   slider**) or bar chart (ordinal / categorical), plus summary statistics.
4. **Variable dictionary** — searchable table of every variable, its
   measurement level, and its question wording.

## Design notes / data handling

- **`-99` is recoded to `NA`.** It is a "don't know / refused" sentinel present
  in 26 numeric items (e.g. `ideology` runs `-99..10`); left in, it would wreck
  every correlation and line of best fit.
- **Variable classification** (`app.R`, top): `admin_drop` removes process
  metadata, free-text, and derived columns; `continuous_vars` and
  `categorical_vars` are curated lists; everything else numeric falls through to
  `ordinal_vars`. Adjust these three vectors to re-scope. In particular, whether
  the 0–10 scales count as interval (currently in `continuous_vars`) or ordinal
  is a judgement call you can flip.
- **`labels.csv`** maps each variable to its survey question wording. It is
  generated from the raw Qualtrics header rows (question text only, no
  respondent data). Regenerate with `scratchpad/gen_labels.R` if the instrument
  changes.

## Possible extensions

- Missingness overview (per-item response counts / a missingness heatmap).
- Weighting or subgroup filters (e.g. restrict to a province or `strong_mayor`).
- Downloadable filtered data / plot export.
- Reliability (Cronbach's α) for multi-item scales (incivility, legitimacy).
