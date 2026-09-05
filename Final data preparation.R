# ================================================================
# POOLED DHS UNDER-5 MORTALITY
# FINAL DATA PREPARATION SCRIPT (REGRESSION-READY)
#
# Countries:
#   Bangladesh 2022
#   Nepal 2022
#   Pakistan 2017-18
#   India 2019-21
#
# This is the FINAL version of the data preparation pipeline.
# All diagnostic issues identified during the review process have
# been resolved and are documented below. Running this script
# produces `analysis_data`, saved as an .rds file, ready to be
# used in a regression model.
#
# ----------------------------------------------------------------
# DOCUMENTED DECISIONS (for methods section / reproducibility)
# ----------------------------------------------------------------
#
# 1. OUTCOME DEFINITION (died_under5)
#    A child is coded as:
#      1 = died before 60 completed months (b5==0, b7<60)
#      0 = survived to at least 60 months (b5==1 & age>=60,
#          OR b5==0 & b7>=60)
#      NA = still alive and under 60 months at interview
#           (outcome not yet resolved — genuinely censored)
#
#    IMPORTANT FIX: an earlier draft of this script additionally
#    required `age_at_interview >= 60` ("full_followup") before a
#    case could enter the analytic sample. This was incorrect and
#    has been REMOVED. `age_at_interview` reflects time since
#    birth regardless of vital status, so a child who died at, say,
#    8 months but was born only 20 months before the survey was
#    being wrongly excluded from the analytic sample even though
#    their death is a fully observed event. Removing this filter
#    recovered several thousand real deaths across all four
#    countries (largest impact: Pakistan, +593 deaths; India,
#    +8,539 deaths) and shifted crude mortality upward by
#    0.7-1.7 percentage points per country. Only `!is.na(died_under5)`
#    is used to define analytic eligibility on the outcome side.
#
# 2. INDIA b11 (PRECEDING BIRTH INTERVAL) SPECIAL CODES
#    27 India records have b11 == 997, which DHS's own value
#    label confirms as "inconsistent" (verified directly against
#    the raw .dta label: attr(b11, "labels") -> 997 = inconsistent).
#    These are NOT real birth intervals. birth_interval_cat's
#    case_when logic requires b11 < 900 for a valid interval
#    category, so all 27 cases correctly fall through to NA and
#    are excluded via the birth_interval_cat missingness filter.
#    No further action needed; documented here for transparency.
#
# 3. PAKISTAN ZERO-WEIGHT OBSERVATIONS (AJK & GILGIT-BALTISTAN)
#    9,058 Pakistan records have v005 (and therefore weight_raw)
#    equal to 0, concentrated in a contiguous block of 103 PSUs
#    (v001 471-580). This is confirmed (DHS Program user forum,
#    response from DHS staff, 2024) to correspond to Azad Jammu &
#    Kashmir (AJK) and Gilgit-Baltistan (GB) -- two regions
#    surveyed in the 2017-18 PDHS but deliberately given
#    zero/standard weight because they are reported separately
#    from Pakistan's national totals for political reasons. The
#    correct weight for these two regions is a DIFFERENT variable
#    (sv005, not collected in this pipeline) which is normalized
#    separately and CANNOT be pooled with the rest of Pakistan or
#    with the other three countries.
#
#    DECISION: AJK and GB are excluded from the analytic sample via
#    `weight_raw > 0`, consistent with DHS's own convention for
#    Pakistan national estimates. This should be stated explicitly
#    in the methods section, e.g.:
#      "Pakistan's analytic sample excludes Azad Jammu & Kashmir
#       and Gilgit-Baltistan, consistent with DHS convention, as
#       these regions carry a separate, non-comparable sampling
#       weight (sv005) and are excluded from PDHS national
#       estimates (DHS Program, pers. comm., 2024)."
#    This removes 718 deaths and 8,340 survivors from Pakistan's
#    pool prior to the missing-covariate filters below.
#
# 4. v022 / v023 / v106 LABEL WARNINGS ON POOLING
#    dplyr::bind_rows() raises warnings that value LABELS for
#    v022/v023/v106 conflict across countries and keeps only the
#    first-loaded country's labels. This affects LABEL TEXT ONLY.
#    Verified: each country's v106 was independently checked
#    against its own original label attributes before pooling and
#    all match the expected 0/1/2/3 = no education/primary/
#    secondary/higher scheme. All derived variables (mat_edu,
#    wealth, residence, child_sex, birth_order_cat,
#    birth_interval_cat) are built from the underlying NUMERIC
#    codes via case_when(), never from label text, so they are
#    unaffected by this warning. Do not trust printed v022/v023
#    label text on the pooled object; refer back to
#    country_data[[country]]$v022 if a raw label is ever needed.
#
# ----------------------------------------------------------------
# EXCLUSION SUMMARY (original pooled N -> analytic N)
# ----------------------------------------------------------------
#   Original pooled N ......... 1,417,080
#   Excluded, no valid outcome . (children alive & <60 months —
#                                 genuinely censored, not an error)
#   Excluded, AJK/GB (Pakistan)  9,058 (718 deaths, 8,340 survivors)
#   Excluded, missing covariate  small residual (birth interval /
#                                 birth order missingness)
#   -----------------------------------------------------------
#   See Section "PROPOSED FINAL ANALYTIC SAMPLE" below for the
#   exact, reproducible counts from this run.
# ================================================================


# ================================================================
# 0. PACKAGES
# ================================================================

required_packages <- c("haven", "dplyr", "purrr", "tidyr")

for (p in required_packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

library(haven)
library(dplyr)
library(purrr)
library(tidyr)


# ================================================================
# 1. FILE PATHS
# ================================================================

countries <- list(

  bangladesh =
    "C:/Users/User/Downloads/DHS DATASETS/BD_2022_DHS_08312026_2337_222099/BDBR81DT/BDBR81FL.DTA",

  nepal =
    "C:/Users/User/Downloads/DHS DATASETS/NP_2022_DHS_08312026_2342_222099/NPBR82DT/NPBR82FL.DTA",

  pakistan =
    "C:/Users/User/Downloads/DHS DATASETS/PK_2017-18_DHS_09012026_013_222099/PKBR71DT/PKBR71FL.DTA",

  india =
    "C:/Users/User/Downloads/DHS DATASETS/IABR7EDT/IABR7EFL.DTA"
)

cat("\nFile existence check:\n")
print(sapply(countries, file.exists))


# ================================================================
# 2. REQUIRED VARIABLES
# ================================================================

required_vars <- c(
  "v001",   # cluster (PSU)
  "v002",   # household
  "v003",   # respondent/mother line number
  "v005",   # sampling weight
  "v008",   # date of interview, century-month-code (CMC)
  "v022",   # sample strata
  "v023",   # strata alternative
  "v025",   # urban/rural
  "v106",   # maternal education
  "v190",   # wealth quintile
  "b3",     # child's date of birth, CMC
  "b4",     # child's sex
  "b5",     # child alive/dead
  "b7",     # age at death (months)
  "b11",    # preceding birth interval (months)
  "bord"    # birth order
)


# ================================================================
# 3. READ DATA
# ================================================================

read_country <- function(path, country_name) {
  cat("\nReading:", country_name, "\n")
  x <- read_dta(path, col_select = all_of(required_vars))
  x$country <- country_name
  x
}

country_data <- map2(countries, names(countries), read_country)

sample_sizes <- map2_dfr(
  country_data, names(country_data),
  ~ data.frame(country = .y, n = nrow(.x))
)

cat("\nSample size by country:\n")
print(sample_sizes)
cat("\nTOTAL POOLED SAMPLE:", sum(sample_sizes$n), "\n")


# ================================================================
# 4. HARMONIZE VARIABLES
# ================================================================

harmonize_country <- function(x) {

  x %>%
    mutate(

      # Age at interview, months (time since birth; independent
      # of vital status -- used only for descriptive purposes,
      # NOT as an eligibility filter -- see documented decision #1)
      age_at_interview = v008 - b3,

      # ----------------------------------------------------------
      # OUTCOME: under-five death
      #   1 = died before 60 completed months (fully observed)
      #   0 = survived to at least 60 months (fully observed)
      #   NA = alive and still under 60 months (censored -- the
      #        only legitimate reason to exclude a case on the
      #        outcome side)
      # ----------------------------------------------------------
      died_under5 = case_when(
        b5 == 0 & !is.na(b7) & b7 < 60 & age_at_interview >= 0 ~ 1L,
        b5 == 1 & age_at_interview >= 60 ~ 0L,
        b5 == 0 & !is.na(b7) & b7 >= 60 & age_at_interview >= 60 ~ 0L,
        TRUE ~ NA_integer_
      ),

      # Maternal education (0/1/2/3 confirmed identical across all
      # four countries' raw label attributes -- see decision #4)
      mat_edu = case_when(
        v106 == 0 ~ "No education",
        v106 == 1 ~ "Primary",
        v106 == 2 ~ "Secondary",
        v106 == 3 ~ "Higher",
        TRUE ~ NA_character_
      ),
      mat_edu = factor(
        mat_edu,
        levels = c("No education", "Primary", "Secondary", "Higher")
      ),

      # Wealth quintile
      wealth = case_when(
        v190 == 1 ~ "Poorest",
        v190 == 2 ~ "Poorer",
        v190 == 3 ~ "Middle",
        v190 == 4 ~ "Richer",
        v190 == 5 ~ "Richest",
        TRUE ~ NA_character_
      ),
      wealth = factor(
        wealth,
        levels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")
      ),

      # Residence
      residence = case_when(
        v025 == 1 ~ "Urban",
        v025 == 2 ~ "Rural",
        TRUE ~ NA_character_
      ),
      residence = factor(residence, levels = c("Urban", "Rural")),

      # Child sex
      child_sex = case_when(
        b4 == 1 ~ "Male",
        b4 == 2 ~ "Female",
        TRUE ~ NA_character_
      ),
      child_sex = factor(child_sex, levels = c("Male", "Female")),

      # Birth order (categorical)
      birth_order = bord,
      birth_order_cat = case_when(
        bord == 1 ~ "1",
        bord == 2 ~ "2",
        bord == 3 ~ "3",
        bord == 4 ~ "4",
        bord >= 5 ~ "5+",
        TRUE ~ NA_character_
      ),
      birth_order_cat = factor(
        birth_order_cat, levels = c("1", "2", "3", "4", "5+")
      ),

      # ----------------------------------------------------------
      # Birth interval category
      #   First births: b11 structurally not applicable ->
      #                 kept as their own category, not missing.
      #   b11 >= 900: DHS special/inconsistent codes (see decision
      #               #2) -- explicitly excluded via b11 < 900.
      # ----------------------------------------------------------
      birth_interval_cat = case_when(
        bord == 1 ~ "First birth",
        bord > 1 & !is.na(b11) & b11 >= 0  & b11 < 24  ~ "<24 months",
        bord > 1 & !is.na(b11) & b11 >= 24 & b11 < 36  ~ "24-35 months",
        bord > 1 & !is.na(b11) & b11 >= 36 & b11 < 48  ~ "36-47 months",
        bord > 1 & !is.na(b11) & b11 >= 48 & b11 < 900 ~ "48+ months",
        TRUE ~ NA_character_
      ),
      birth_interval_cat = factor(
        birth_interval_cat,
        levels = c("First birth", "<24 months", "24-35 months",
                   "36-47 months", "48+ months")
      ),

      # Identifiers (country-prefixed to guarantee uniqueness
      # across the pooled dataset)
      cluster_id   = paste(country, v001, sep = "_"),
      household_id = paste(country, v001, v002, sep = "_"),
      mother_id    = paste(country, v001, v002, v003, sep = "_"),
      strata_id    = paste(country, v022, sep = "_"),

      # ----------------------------------------------------------
      # Raw DHS sampling weight
      #   Pakistan: this is 0 for AJK & Gilgit-Baltistan by DHS
      #   design (see decision #3) -- NOT a data error.
      # ----------------------------------------------------------
      weight_raw = v005 / 1000000
    )
}

pooled_data <- map_dfr(country_data, harmonize_country)

pooled_data <- pooled_data %>%
  group_by(country) %>%
  mutate(weight_norm = weight_raw / mean(weight_raw, na.rm = TRUE)) %>%
  ungroup()

cat("\nHarmonized pooled dataset created. Rows:", nrow(pooled_data), "\n")


# ================================================================
# 5. FINAL ANALYTIC SAMPLE
#
#    Eligibility criteria (all documented above):
#      - valid, non-censored outcome
#      - non-missing covariates
#      - positive sampling weight (excludes Pakistan's AJK/GB,
#        which require a separate, non-comparable weight variable)
# ================================================================

analysis_data <- pooled_data %>%
  filter(
    !is.na(died_under5),
    !is.na(mat_edu),
    !is.na(wealth),
    !is.na(residence),
    !is.na(child_sex),
    !is.na(birth_order_cat),
    !is.na(birth_interval_cat),
    !is.na(weight_raw),
    weight_raw > 0
  )

cat("\n============================================================\n")
cat("PROPOSED FINAL ANALYTIC SAMPLE\n")
cat("============================================================\n")

cat("\nOriginal pooled N:", nrow(pooled_data), "\n")
cat("Final analysis N:", nrow(analysis_data), "\n")
cat("Excluded:", nrow(pooled_data) - nrow(analysis_data), "\n")
cat(
  "Percent retained:",
  round(100 * nrow(analysis_data) / nrow(pooled_data), 2), "%\n"
)

analysis_country_summary <- analysis_data %>%
  group_by(country) %>%
  summarise(
    N = n(),
    deaths = sum(died_under5 == 1),
    survivors = sum(died_under5 == 0),
    mortality_percent = 100 * deaths / N,
    PSUs = n_distinct(cluster_id),
    mothers = n_distinct(mother_id),
    .groups = "drop"
  )

cat("\nFinal analytic sample by country:\n")
print(analysis_country_summary)

weighted_mortality_check <- analysis_data %>%
  group_by(country) %>%
  summarise(
    unweighted_N = n(),
    unweighted_deaths = sum(died_under5 == 1),
    unweighted_mortality = 100 * unweighted_deaths / unweighted_N,
    weighted_deaths = sum(weight_norm * (died_under5 == 1)),
    weighted_total = sum(weight_norm),
    weighted_mortality = 100 * weighted_deaths / weighted_total,
    .groups = "drop"
  )

cat("\nWeighted vs. unweighted mortality check:\n")
print(weighted_mortality_check)


# ================================================================
# 6. FINAL VALIDATION (should all show [OK])
# ================================================================

cat("\n============================================================\n")
cat("FINAL VALIDATION\n")
cat("============================================================\n")

final_variables <- c(
  "died_under5", "mat_edu", "wealth", "residence", "child_sex",
  "birth_order_cat", "birth_interval_cat", "weight_raw",
  "cluster_id", "mother_id"
)

final_missing <- analysis_data %>%
  summarise(across(all_of(final_variables), ~ sum(is.na(.))))

total_final_missing <- sum(final_missing)

cat(
  if (total_final_missing == 0) "[OK] " else "[WARNING] ",
  "Missing values in final analysis variables:", total_final_missing, "\n"
)

duplicate_check <- analysis_data %>%
  count(country, v001, v002, v003, bord, name = "n_duplicate") %>%
  filter(n_duplicate > 1)

cat(
  if (nrow(duplicate_check) == 0) "[OK] " else "[WARNING] ",
  "Duplicate child identifiers:", nrow(duplicate_check), "\n"
)

negative_weights <- sum(analysis_data$weight_raw < 0, na.rm = TRUE)
cat(
  if (negative_weights == 0) "[OK] " else "[CRITICAL] ",
  "Negative weights:", negative_weights, "\n"
)

zero_weights_remaining <- sum(analysis_data$weight_raw == 0, na.rm = TRUE)
cat(
  if (zero_weights_remaining == 0) "[OK] " else "[CRITICAL] ",
  "Zero weights remaining in analytic sample:", zero_weights_remaining, "\n"
)

cat("\nFinal dataset structure:\n")
cat("Rows:", nrow(analysis_data), "| Columns:", ncol(analysis_data), "\n")
print(names(analysis_data))


# ================================================================
# 7. SAVE OUTPUTS
# ================================================================

output_dir <- "C:/Users/User/Downloads/outputs"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

write.csv(
  analysis_country_summary,
  file.path(output_dir, "analysis_country_summary.csv"),
  row.names = FALSE
)

write.csv(
  weighted_mortality_check,
  file.path(output_dir, "weighted_mortality_check.csv"),
  row.names = FALSE
)

saveRDS(
  analysis_data,
  file.path(output_dir, "pooled_dhs_analysis_data.rds")
)

cat("\nFiles saved to:", output_dir, "\n")

cat("\n============================================================\n")
cat("DATA PREPARATION COMPLETE — analysis_data IS REGRESSION-READY\n")
cat("============================================================\n")
cat("\nLoad it in a new session with:\n")
cat("  analysis_data <- readRDS(\"", 
    file.path(output_dir, "pooled_dhs_analysis_data.rds"), "\")\n", sep = "")