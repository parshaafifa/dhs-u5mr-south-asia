# DHS Under-Five Mortality Spatial Analysis — South Asia

Spatial and statistical analysis of under-five mortality (U5MR) across Bangladesh, Nepal, Pakistan, and India using Demographic and Health Survey (DHS) data.

## Overview

This project estimates and maps under-five mortality at the admin-1 (division/province/state) level for four South Asian countries, examines spatial clustering patterns, and models individual/household-level determinants of child mortality risk using a pooled, weighted analysis.

## Data Sources

| Country | Survey | Year |
|---|---|---|
| Bangladesh | BDHS | 2022 |
| Nepal | NDHS | 2022 |
| Pakistan | PDHS | 2017–18 |
| India | NFHS-5 | 2019–21 |

Raw DHS microdata (Births Recode, GPS cluster files) are **not included** in this repository, as DHS data is licensed and cannot be redistributed. Researchers can request access at [dhsprogram.com](https://dhsprogram.com). Administrative boundary shapefiles were sourced from [GADM](https://gadm.org).

**Note:** survey years differ by country, reflecting the most recent nationally representative DHS/NFHS round available for each at the time of analysis. This is a limitation of cross-country comparability discussed further in the write-up.

## Methodology

1. **U5MR estimation** — direct estimation from birth histories (synthetic cohort method) at the DHS cluster level.
2. **Spatial linkage** — cluster GPS coordinates joined to admin-1 boundaries (GADM) to aggregate mortality estimates regionally.
3. **Choropleth mapping** — regional U5MR visualized per country.
4. **Spatial autocorrelation** — Global Moran's I and Local Indicators of Spatial Association (LISA) to identify significant mortality clusters (hotspots/coldspots).
5. **Determinants analysis** — two complementary weighted regression approaches on pooled individual-level data across all four countries:
   - `weighted_multilevel_model_glmer.R` — multilevel (mixed-effects) logistic regression with a random intercept per DHS cluster, DHS sample weight applied as a prior weight. **Primary model.**
   - `svyglm_model.R` — design-based weighted logistic regression (survey-adjusted standard errors, no random effect). Used as a robustness check.

## Repository Contents

```
dhs_u5mr_pipeline_optimized.R      # Main pipeline: U5MR calc, spatial join, choropleth, Moran's I/LISA (per country)
weighted_multilevel_model_glmer.R  # Primary determinants model (weighted multilevel logistic regression)
svyglm_model.R                     # Secondary determinants model (design-based weighted logistic regression)
outputs/
  choropleth_<country>.png         # U5MR choropleth maps
  region_u5mr_<country>.csv        # Region-level U5MR summary
  multilevel_model_results.csv     # Odds ratios from the primary (glmer) model
  svyglm_model_results.csv         # Odds ratios from the secondary (svyglm) model
```

## Covariates Included in Determinants Models

Maternal education, household wealth quintile, urban/rural residence, birth order, preceding birth interval, child sex, and country.

## Requirements

R (≥ 4.5) with packages: `haven`, `dplyr`, `sf`, `spdep`, `tmap`, `rmapshaper`, `lme4`, `broom.mixed`, `survey`.

## Limitations

- Survey years are not synchronized across countries (see table above).
- Weighted multilevel estimation uses DHS sample weight as a simple prior weight within `glmer`, rather than full survey-design-adjusted multilevel estimation — a common simplification in comparable published DHS multi-country studies, adopted here after computational constraints ruled out fully design-based multilevel packages (e.g., WeMix) at this sample scale.
- GADM administrative boundaries may differ from official government boundaries in some disputed border areas.

## Author

Kotha — Department of Statistics and Data Science, Jahangirnagar University, Bangladesh
Supervised by Md Moyazzem Hossain Sobuj
