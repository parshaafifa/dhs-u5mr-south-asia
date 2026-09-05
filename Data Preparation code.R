# ================================================================
# POOLED DHS UNDER-5 MORTALITY
# FULL DATA PREPARATION + DIAGNOSTIC SCRIPT
#
# Countries:
#   Bangladesh 2022
#   Nepal 2022
#   Pakistan 2017-18
#   India 2019-21
#
# IMPORTANT:
# This script DOES NOT fit the final regression model.
# It prepares and diagnoses the data only.
#
# Run the entire script and send me ALL console output.
# ================================================================


# ================================================================
# 0. PACKAGES
# ================================================================

required_packages <- c(
  "haven",
  "dplyr",
  "purrr",
  "tidyr"
)

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


cat("\n")
cat("============================================================\n")
cat("1. FILE EXISTENCE CHECK\n")
cat("============================================================\n")

print(sapply(countries, file.exists))


# ================================================================
# 2. REQUIRED VARIABLES
# ================================================================

required_vars <- c(
  "v001",   # cluster
  "v002",   # household
  "v003",   # respondent/mother line number
  "v005",   # sampling weight
  "v008",   # date of interview in CMC
  "v022",   # sample strata
  "v023",   # strata alternative
  "v025",   # urban/rural
  "v106",   # education
  "v190",   # wealth
  "b3",     # date of birth in CMC
  "b4",     # sex
  "b5",     # alive/dead
  "b7",     # age at death
  "b11",    # preceding birth interval
  "bord"    # birth order
)


# ================================================================
# 3. FUNCTION TO CHECK VARIABLES
# ================================================================

check_variables <- function(path, country_name) {

  cat("\n------------------------------------------------------------\n")
  cat("Country:", country_name, "\n")
  cat("------------------------------------------------------------\n")

  x <- read_dta(path, col_select = any_of(required_vars))

  available <- names(x)

  missing_vars <- setdiff(required_vars, available)

  if (length(missing_vars) == 0) {

    cat("ALL REQUIRED VARIABLES PRESENT\n")

  } else {

    cat("MISSING VARIABLES:\n")
    print(missing_vars)

  }

  cat("Total columns:", ncol(x), "\n")
  cat("Total rows:", nrow(x), "\n")

  rm(x)
  gc()
}


for (cn in names(countries)) {
  check_variables(
    countries[[cn]],
    cn
  )
}


# ================================================================
# 4. READ ONLY REQUIRED VARIABLES
# ================================================================

read_country <- function(path, country_name) {

  cat("\nReading:", country_name, "\n")

  x <- read_dta(
    path,
    col_select = all_of(required_vars)
  )

  x$country <- country_name

  x
}


country_data <- map2(
  countries,
  names(countries),
  read_country
)


# ================================================================
# 5. SAMPLE SIZE BY COUNTRY
# ================================================================

cat("\n")
cat("============================================================\n")
cat("2. SAMPLE SIZE\n")
cat("============================================================\n")

sample_sizes <- map2_dfr(
  country_data,
  names(country_data),
  ~ data.frame(
    country = .y,
    n = nrow(.x)
  )
)

print(sample_sizes)

cat("\nTOTAL POOLED SAMPLE:", sum(sample_sizes$n), "\n")


# ================================================================
# 6. BASIC VARIABLE STRUCTURE
# ================================================================

cat("\n")
cat("============================================================\n")
cat("3. VARIABLE STRUCTURE / CLASSES\n")
cat("============================================================\n")

for (cn in names(country_data)) {

  cat("\n---", cn, "---\n")

  x <- country_data[[cn]]

  print(
    data.frame(
      variable = names(x),
      class = sapply(x, function(z) paste(class(z), collapse = "/")),
      stringsAsFactors = FALSE
    )
  )
}


# ================================================================
# 7. RAW VALUE DISTRIBUTIONS
# ================================================================

cat("\n")
cat("============================================================\n")
cat("4. RAW VALUE DISTRIBUTIONS\n")
cat("============================================================\n")


# -------------------------
# EDUCATION
# -------------------------

cat("\n### MATERNAL EDUCATION: v106 ###\n")

for (cn in names(country_data)) {

  cat("\n---", cn, "---\n")

  print(
    table(
      country_data[[cn]]$v106,
      useNA = "ifany"
    )
  )
}


# -------------------------
# RESIDENCE
# -------------------------

cat("\n### RESIDENCE: v025 ###\n")

for (cn in names(country_data)) {

  cat("\n---", cn, "---\n")

  print(
    table(
      country_data[[cn]]$v025,
      useNA = "ifany"
    )
  )
}


# -------------------------
# WEALTH
# -------------------------

cat("\n### WEALTH: v190 ###\n")

for (cn in names(country_data)) {

  cat("\n---", cn, "---\n")

  print(
    table(
      country_data[[cn]]$v190,
      useNA = "ifany"
    )
  )
}


# -------------------------
# CHILD SEX
# -------------------------

cat("\n### CHILD SEX: b4 ###\n")

for (cn in names(country_data)) {

  cat("\n---", cn, "---\n")

  print(
    table(
      country_data[[cn]]$b4,
      useNA = "ifany"
    )
  )
}


# -------------------------
# CHILD STATUS
# -------------------------

cat("\n### CHILD STATUS: b5 ###\n")

for (cn in names(country_data)) {

  cat("\n---", cn, "---\n")

  print(
    table(
      country_data[[cn]]$b5,
      useNA = "ifany"
    )
  )
}


# ================================================================
# 8. LABEL CHECKS
# ================================================================

cat("\n")
cat("============================================================\n")
cat("5. LABEL CHECKS\n")
cat("============================================================\n")

for (cn in names(country_data)) {

  cat("\n############################################################\n")
  cat("COUNTRY:", cn, "\n")
  cat("############################################################\n")

  x <- country_data[[cn]]

  cat("\nv106 labels:\n")
  print(attr(x$v106, "labels"))

  cat("\nv190 labels:\n")
  print(attr(x$v190, "labels"))

  cat("\nv025 labels:\n")
  print(attr(x$v025, "labels"))

  cat("\nv022 labels:\n")
  print(attr(x$v022, "labels"))

  cat("\nv023 labels:\n")
  print(attr(x$v023, "labels"))

  cat("\nb4 labels:\n")
  print(attr(x$b4, "labels"))

  cat("\nb5 labels:\n")
  print(attr(x$b5, "labels"))
}


# ================================================================
# 9. CREATE HARMONIZED VARIABLES
# ================================================================

cat("\n")
cat("============================================================\n")
cat("6. HARMONIZED VARIABLES\n")
cat("============================================================\n")


harmonize_country <- function(x) {

  x %>%

    mutate(

      # ----------------------------------------------------------
      # AGE AT INTERVIEW
      # ----------------------------------------------------------

      age_at_interview =
        v008 - b3,


      # ----------------------------------------------------------
      # FULL FIVE-YEAR FOLLOW-UP
      # ----------------------------------------------------------

      full_followup =
        age_at_interview >= 60,


      # ----------------------------------------------------------
      # UNDER-FIVE DEATH
      #
      # 1 = death before 60 completed months
      # 0 = survived to at least 60 months
      #
      # Living children <60 months are NOT coded as survivors.
      # ----------------------------------------------------------

      died_under5 = case_when(

        b5 == 0 &
          !is.na(b7) &
          b7 < 60 &
          age_at_interview >= 0
          ~ 1L,

        b5 == 1 &
          age_at_interview >= 60
          ~ 0L,

        b5 == 0 &
          !is.na(b7) &
          b7 >= 60 &
          age_at_interview >= 60
          ~ 0L,

        TRUE
          ~ NA_integer_
      ),


      # ----------------------------------------------------------
      # MATERNAL EDUCATION
      # ----------------------------------------------------------

      mat_edu = case_when(

        v106 == 0 ~ "No education",

        v106 == 1 ~ "Primary",

        v106 == 2 ~ "Secondary",

        v106 == 3 ~ "Higher",

        TRUE ~ NA_character_
      ),

      mat_edu = factor(
        mat_edu,
        levels = c(
          "No education",
          "Primary",
          "Secondary",
          "Higher"
        )
      ),


      # ----------------------------------------------------------
      # WEALTH
      # ----------------------------------------------------------

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
        levels = c(
          "Poorest",
          "Poorer",
          "Middle",
          "Richer",
          "Richest"
        )
      ),


      # ----------------------------------------------------------
      # RESIDENCE
      # ----------------------------------------------------------

      residence = case_when(

        v025 == 1 ~ "Urban",

        v025 == 2 ~ "Rural",

        TRUE ~ NA_character_
      ),

      residence = factor(
        residence,
        levels = c(
          "Urban",
          "Rural"
        )
      ),


      # ----------------------------------------------------------
      # CHILD SEX
      # ----------------------------------------------------------

      child_sex = case_when(

        b4 == 1 ~ "Male",

        b4 == 2 ~ "Female",

        TRUE ~ NA_character_
      ),

      child_sex = factor(
        child_sex,
        levels = c(
          "Male",
          "Female"
        )
      ),


      # ----------------------------------------------------------
      # BIRTH ORDER
      # ----------------------------------------------------------

      birth_order = bord,


      # Categorical birth order
      birth_order_cat = case_when(

        bord == 1 ~ "1",

        bord == 2 ~ "2",

        bord == 3 ~ "3",

        bord == 4 ~ "4",

        bord >= 5 ~ "5+",

        TRUE ~ NA_character_
      ),

      birth_order_cat = factor(
        birth_order_cat,
        levels = c(
          "1",
          "2",
          "3",
          "4",
          "5+"
        )
      ),


      # ----------------------------------------------------------
      # BIRTH INTERVAL
      #
      # FIRST BIRTH:
      # b11 is structurally not applicable.
      #
      # We preserve first births as a separate category.
      # ----------------------------------------------------------

      birth_interval_cat = case_when(

        bord == 1
          ~ "First birth",

        bord > 1 &
          !is.na(b11) &
          b11 >= 0 &
          b11 < 24
          ~ "<24 months",

        bord > 1 &
          !is.na(b11) &
          b11 >= 24 &
          b11 < 36
          ~ "24-35 months",

        bord > 1 &
          !is.na(b11) &
          b11 >= 36 &
          b11 < 48
          ~ "36-47 months",

        bord > 1 &
          !is.na(b11) &
          b11 >= 48 &
          b11 < 900
          ~ "48+ months",

        TRUE
          ~ NA_character_
      ),

      birth_interval_cat = factor(
        birth_interval_cat,
        levels = c(
          "First birth",
          "<24 months",
          "24-35 months",
          "36-47 months",
          "48+ months"
        )
      ),


      # ----------------------------------------------------------
      # UNIQUE PSU ID
      # ----------------------------------------------------------

      cluster_id = paste(
        country,
        v001,
        sep = "_"
      ),


      # ----------------------------------------------------------
      # UNIQUE HOUSEHOLD ID
      # ----------------------------------------------------------

      household_id = paste(
        country,
        v001,
        v002,
        sep = "_"
      ),


      # ----------------------------------------------------------
      # UNIQUE MOTHER ID
      # ----------------------------------------------------------

      mother_id = paste(
        country,
        v001,
        v002,
        v003,
        sep = "_"
      ),


      # ----------------------------------------------------------
      # UNIQUE STRATUM ID
      # ----------------------------------------------------------

      strata_id = paste(
        country,
        v022,
        sep = "_"
      ),


      # ----------------------------------------------------------
      # RAW DHS WEIGHT
      # ----------------------------------------------------------

      weight_raw =
        v005 / 1000000
    )
}


pooled_data <- map_dfr(
  country_data,
  harmonize_country
)


# ================================================================
# 10. HARMONIZED VARIABLE CHECK
# ================================================================

cat("\n")
cat("============================================================\n")
cat("7. HARMONIZED VARIABLE CHECK\n")
cat("============================================================\n")

cat("\nMaternal education:\n")
print(table(
  pooled_data$country,
  pooled_data$mat_edu,
  useNA = "ifany"
))

cat("\nWealth:\n")
print(table(
  pooled_data$country,
  pooled_data$wealth,
  useNA = "ifany"
))

cat("\nResidence:\n")
print(table(
  pooled_data$country,
  pooled_data$residence,
  useNA = "ifany"
))

cat("\nChild sex:\n")
print(table(
  pooled_data$country,
  pooled_data$child_sex,
  useNA = "ifany"
))

cat("\nBirth order category:\n")
print(table(
  pooled_data$country,
  pooled_data$birth_order_cat,
  useNA = "ifany"
))

cat("\nBirth interval category:\n")
print(table(
  pooled_data$country,
  pooled_data$birth_interval_cat,
  useNA = "ifany"
))


# ================================================================
# 11. AGE AT INTERVIEW
# ================================================================

cat("\n")
cat("============================================================\n")
cat("8. AGE AT INTERVIEW\n")
cat("============================================================\n")

age_summary <- pooled_data %>%
  group_by(country) %>%
  summarise(

    N = n(),

    min_age_months =
      min(age_at_interview, na.rm = TRUE),

    q1_age_months =
      quantile(
        age_at_interview,
        0.25,
        na.rm = TRUE
      ),

    median_age_months =
      median(
        age_at_interview,
        na.rm = TRUE
      ),

    mean_age_months =
      mean(
        age_at_interview,
        na.rm = TRUE
      ),

    max_age_months =
      max(
        age_at_interview,
        na.rm = TRUE
      ),

    age_negative =
      sum(
        age_at_interview < 0,
        na.rm = TRUE
      ),

    age_lt60 =
      sum(
        age_at_interview < 60,
        na.rm = TRUE
      ),

    age_ge60 =
      sum(
        age_at_interview >= 60,
        na.rm = TRUE
      ),

    .groups = "drop"
  )

print(age_summary)


# ================================================================
# 12. OUTCOME DIAGNOSTICS
# ================================================================

cat("\n")
cat("============================================================\n")
cat("9. UNDER-FIVE OUTCOME DIAGNOSTICS\n")
cat("============================================================\n")


# Child status by country
cat("\nB5 by country:\n")

print(
  table(
    pooled_data$country,
    pooled_data$b5,
    useNA = "ifany"
  )
)


# Deaths by age at death
cat("\nDeaths by age at death:\n")

death_summary <- pooled_data %>%
  filter(b5 == 0) %>%
  group_by(country) %>%
  summarise(

    deaths_total = n(),

    b7_missing =
      sum(is.na(b7)),

    b7_lt60 =
      sum(
        !is.na(b7) &
        b7 < 60
      ),

    b7_ge60 =
      sum(
        !is.na(b7) &
        b7 >= 60
      ),

    .groups = "drop"
  )

print(death_summary)


# Outcome distribution
cat("\nFinal outcome distribution:\n")

print(
  table(
    pooled_data$country,
    pooled_data$died_under5,
    useNA = "ifany"
  )
)


# Outcome percentages
cat("\nOutcome percentages among non-missing outcomes:\n")

outcome_summary <- pooled_data %>%
  group_by(country) %>%
  summarise(

    N_total = n(),

    outcome_nonmissing =
      sum(!is.na(died_under5)),

    deaths_under5 =
      sum(
        died_under5 == 1,
        na.rm = TRUE
      ),

    survived_to5 =
      sum(
        died_under5 == 0,
        na.rm = TRUE
      ),

    mortality_percent =
      100 *
      deaths_under5 /
      outcome_nonmissing,

    .groups = "drop"
  )

print(outcome_summary)


# ================================================================
# 13. IMPORTANT OUTCOME CROSS-CHECK
# ================================================================

cat("\n")
cat("============================================================\n")
cat("10. OUTCOME CROSS-CHECK\n")
cat("============================================================\n")

cat(
  "\nCases where a child is alive but <60 months:\n"
)

print(
  pooled_data %>%
    filter(
      b5 == 1,
      age_at_interview < 60
    ) %>%
    count(country)
)


cat(
  "\nCases where child died before 60 months:\n"
)

print(
  pooled_data %>%
    filter(
      b5 == 0,
      !is.na(b7),
      b7 < 60
    ) %>%
    count(country)
)


cat(
  "\nDeaths at >=60 months:\n"
)

print(
  pooled_data %>%
    filter(
      b5 == 0,
      !is.na(b7),
      b7 >= 60
    ) %>%
    count(country)
)


# ================================================================
# 14. BIRTH INTERVAL DIAGNOSTICS
# ================================================================

cat("\n")
cat("============================================================\n")
cat("11. BIRTH INTERVAL DIAGNOSTICS\n")
cat("============================================================\n")


birth_interval_summary <- pooled_data %>%
  group_by(country) %>%
  summarise(

    total = n(),

    b11_missing =
      sum(is.na(b11)),

    b11_zero =
      sum(b11 == 0, na.rm = TRUE),

    b11_negative =
      sum(b11 < 0, na.rm = TRUE),

    b11_lt24 =
      sum(
        !is.na(b11) &
        b11 >= 0 &
        b11 < 24
      ),

    b11_24_35 =
      sum(
        !is.na(b11) &
        b11 >= 24 &
        b11 < 36
      ),

    b11_36_47 =
      sum(
        !is.na(b11) &
        b11 >= 36 &
        b11 < 48
      ),

    b11_ge48 =
      sum(
        !is.na(b11) &
        b11 >= 48 &
        b11 < 900
      ),

    b11_ge900 =
      sum(
        !is.na(b11) &
        b11 >= 900
      ),

    .groups = "drop"
  )

print(birth_interval_summary)


# ------------------------------------------------
# B11 MISSING BY BIRTH ORDER
# ------------------------------------------------

cat("\nB11 missingness by birth order:\n")

print(
  pooled_data %>%
    group_by(country, bord) %>%
    summarise(
      N = n(),
      b11_missing = sum(is.na(b11)),
      .groups = "drop"
    ) %>%
    filter(
      b11_missing > 0
    )
)


# ================================================================
# 15. INDIA 997 INVESTIGATION
# ================================================================

cat("\n")
cat("============================================================\n")
cat("12. INDIA B11 >=900 INVESTIGATION\n")
cat("============================================================\n")

india_special_b11 <- pooled_data %>%
  filter(
    country == "india",
    !is.na(b11),
    b11 >= 900
  ) %>%
  select(
    v001,
    v002,
    v003,
    v008,
    b3,
    b11,
    bord,
    b5,
    age_at_interview
  )

cat("\nNumber of India b11 >=900:\n")
print(nrow(india_special_b11))

cat("\nFrequency:\n")
print(
  table(
    india_special_b11$b11,
    useNA = "ifany"
  )
)

cat("\nFirst 30 cases:\n")
print(
  head(
    india_special_b11,
    30
  )
)


# ================================================================
# 16. BIRTH ORDER DIAGNOSTICS
# ================================================================

cat("\n")
cat("============================================================\n")
cat("13. BIRTH ORDER\n")
cat("============================================================\n")

birth_order_summary <- pooled_data %>%
  group_by(country) %>%
  summarise(

    min_birth_order =
      min(bord, na.rm = TRUE),

    q1 =
      quantile(
        bord,
        0.25,
        na.rm = TRUE
      ),

    median =
      median(
        bord,
        na.rm = TRUE
      ),

    mean =
      mean(
        bord,
        na.rm = TRUE
      ),

    max_birth_order =
      max(bord, na.rm = TRUE),

    missing =
      sum(is.na(bord)),

    .groups = "drop"
  )

print(birth_order_summary)


# ================================================================
# 17. WEIGHT DIAGNOSTICS
# ================================================================

cat("\n")
cat("============================================================\n")
cat("14. DHS WEIGHT DIAGNOSTICS\n")
cat("============================================================\n")

weight_summary <- pooled_data %>%
  group_by(country) %>%
  summarise(

    N = n(),

    min_weight =
      min(weight_raw, na.rm = TRUE),

    q1_weight =
      quantile(
        weight_raw,
        0.25,
        na.rm = TRUE
      ),

    median_weight =
      median(
        weight_raw,
        na.rm = TRUE
      ),

    mean_weight =
      mean(
        weight_raw,
        na.rm = TRUE
      ),

    q3_weight =
      quantile(
        weight_raw,
        0.75,
        na.rm = TRUE
      ),

    max_weight =
      max(
        weight_raw,
        na.rm = TRUE
      ),

    missing =
      sum(is.na(weight_raw)),

    zero =
      sum(weight_raw == 0, na.rm = TRUE),

    negative =
      sum(weight_raw < 0, na.rm = TRUE),

    .groups = "drop"
  )

print(weight_summary)


# ------------------------------------------------
# ZERO-WEIGHT CASES
# ------------------------------------------------

cat("\nZero-weight cases by country:\n")

print(
  pooled_data %>%
    filter(
      weight_raw == 0
    ) %>%
    count(country)
)


# ------------------------------------------------
# EXTREME WEIGHTS
# ------------------------------------------------

cat("\nTop 20 largest weights:\n")

print(
  pooled_data %>%
    arrange(
      desc(weight_raw)
    ) %>%
    select(
      country,
      v001,
      v002,
      v003,
      weight_raw
    ) %>%
    head(20)
)


# ================================================================
# 18. WEIGHT NORMALIZATION
# ================================================================

cat("\n")
cat("============================================================\n")
cat("15. WITHIN-COUNTRY WEIGHT NORMALIZATION\n")
cat("============================================================\n")

pooled_data <- pooled_data %>%

  group_by(country) %>%

  mutate(
    weight_norm =
      weight_raw /
      mean(
        weight_raw,
        na.rm = TRUE
      )
  ) %>%

  ungroup()


weight_norm_summary <- pooled_data %>%

  group_by(country) %>%

  summarise(

    mean_raw =
      mean(
        weight_raw,
        na.rm = TRUE
      ),

    mean_normalized =
      mean(
        weight_norm,
        na.rm = TRUE
      ),

    min_normalized =
      min(
        weight_norm,
        na.rm = TRUE
      ),

    max_normalized =
      max(
        weight_norm,
        na.rm = TRUE
      ),

    .groups = "drop"
  )

print(weight_norm_summary)


# ================================================================
# 19. PSU / HOUSEHOLD / MOTHER STRUCTURE
# ================================================================

cat("\n")
cat("============================================================\n")
cat("16. CLUSTER / HOUSEHOLD / MOTHER STRUCTURE\n")
cat("============================================================\n")

cluster_summary <- pooled_data %>%

  group_by(country) %>%

  summarise(

    PSUs =
      n_distinct(cluster_id),

    households =
      n_distinct(household_id),

    mothers =
      n_distinct(mother_id),

    observations =
      n(),

    .groups = "drop"
  )

print(cluster_summary)


# ------------------------------------------------
# CHILDREN PER MOTHER
# ------------------------------------------------

cat("\nChildren per mother:\n")

mother_structure <- pooled_data %>%

  group_by(
    country,
    mother_id
  ) %>%

  summarise(
    children = n(),
    .groups = "drop"
  ) %>%

  group_by(country) %>%

  summarise(

    mothers =
      n(),

    mothers_one_child =
      sum(children == 1),

    mothers_multiple_children =
      sum(children > 1),

    max_children =
      max(children),

    mean_children =
      mean(children),

    .groups = "drop"
  )

print(mother_structure)


# ================================================================
# 20. DUPLICATE CHECK
# ================================================================

cat("\n")
cat("============================================================\n")
cat("17. DUPLICATE CHECK\n")
cat("============================================================\n")


# Same country + cluster + household + mother + birth order
# should normally identify a child.

duplicate_check <- pooled_data %>%

  count(
    country,
    v001,
    v002,
    v003,
    bord,
    name = "n_duplicate"
  ) %>%

  filter(
    n_duplicate > 1
  )

cat(
  "\nDuplicate child identifiers:",
  nrow(duplicate_check),
  "\n"
)

if (nrow(duplicate_check) > 0) {
  print(
    head(
      duplicate_check,
      20
    )
  )
}


# ================================================================
# 21. STRATA DIAGNOSTICS
# ================================================================

cat("\n")
cat("============================================================\n")
cat("18. STRATA\n")
cat("============================================================\n")

strata_summary <- pooled_data %>%

  group_by(country) %>%

  summarise(

    unique_v022 =
      n_distinct(v022),

    unique_v023 =
      n_distinct(v023),

    unique_strata_id =
      n_distinct(strata_id),

    .groups = "drop"
  )

print(strata_summary)


cat("\nCountry-specific v022 distributions:\n")

for (cn in names(country_data)) {

  cat("\n---", cn, "---\n")

  print(
    table(
      country_data[[cn]]$v022,
      useNA = "ifany"
    )
  )
}


# ================================================================
# 22. MISSING DATA
# ================================================================

cat("\n")
cat("============================================================\n")
cat("19. MISSING DATA\n")
cat("============================================================\n")


missing_summary <- pooled_data %>%

  group_by(country) %>%

  summarise(

    N = n(),

    missing_outcome =
      sum(is.na(died_under5)),

    missing_education =
      sum(is.na(mat_edu)),

    missing_wealth =
      sum(is.na(wealth)),

    missing_residence =
      sum(is.na(residence)),

    missing_sex =
      sum(is.na(child_sex)),

    missing_birth_order =
      sum(is.na(birth_order_cat)),

    missing_birth_interval =
      sum(is.na(birth_interval_cat)),

    missing_weight =
      sum(is.na(weight_raw)),

    zero_weight =
      sum(weight_raw == 0, na.rm = TRUE),

    .groups = "drop"
  )

print(missing_summary)


# ================================================================
# 23. PROPOSED FINAL ANALYTIC SAMPLE
# ================================================================

cat("\n")
cat("============================================================\n")
cat("20. PROPOSED FINAL ANALYTIC SAMPLE\n")
cat("============================================================\n")


analysis_data <- pooled_data %>%

  filter(

    # valid outcome
    !is.na(died_under5),

    # explanatory variables
    !is.na(mat_edu),
    !is.na(wealth),
    !is.na(residence),
    !is.na(child_sex),
    !is.na(birth_order_cat),
    !is.na(birth_interval_cat),

    # valid positive weight
    !is.na(weight_raw),
    weight_raw > 0

  )


cat(
  "\nOriginal pooled N:",
  nrow(pooled_data),
  "\n"
)

cat(
  "Proposed analysis N:",
  nrow(analysis_data),
  "\n"
)

cat(
  "Excluded:",
  nrow(pooled_data) -
    nrow(analysis_data),
  "\n"
)

cat(
  "Percent retained:",
  round(
    100 *
      nrow(analysis_data) /
      nrow(pooled_data),
    2
  ),
  "%\n"
)


# ================================================================
# 24. ANALYTIC SAMPLE BY COUNTRY
# ================================================================

cat("\n")
cat("============================================================\n")
cat("21. ANALYTIC SAMPLE BY COUNTRY\n")
cat("============================================================\n")

analysis_country_summary <- analysis_data %>%

  group_by(country) %>%

  summarise(

    N = n(),

    deaths =
      sum(
        died_under5 == 1
      ),

    survivors =
      sum(
        died_under5 == 0
      ),

    mortality_percent =
      100 *
      deaths /
      N,

    PSUs =
      n_distinct(cluster_id),

    mothers =
      n_distinct(mother_id),

    .groups = "drop"
  )

print(analysis_country_summary)


# ================================================================
# 25. ANALYTIC SAMPLE CHARACTERISTICS
# ================================================================

cat("\n")
cat("============================================================\n")
cat("22. ANALYTIC SAMPLE CHARACTERISTICS\n")
cat("============================================================\n")


cat("\nMaternal education:\n")

print(
  prop.table(
    table(
      analysis_data$country,
      analysis_data$mat_edu
    ),
    margin = 1
  ) * 100
)


cat("\nWealth:\n")

print(
  prop.table(
    table(
      analysis_data$country,
      analysis_data$wealth
    ),
    margin = 1
  ) * 100
)


cat("\nResidence:\n")

print(
  prop.table(
    table(
      analysis_data$country,
      analysis_data$residence
    ),
    margin = 1
  ) * 100
)


cat("\nChild sex:\n")

print(
  prop.table(
    table(
      analysis_data$country,
      analysis_data$child_sex
    ),
    margin = 1
  ) * 100
)


cat("\nBirth order:\n")

print(
  prop.table(
    table(
      analysis_data$country,
      analysis_data$birth_order_cat
    ),
    margin = 1
  ) * 100
)


cat("\nBirth interval:\n")

print(
  prop.table(
    table(
      analysis_data$country,
      analysis_data$birth_interval_cat
    ),
    margin = 1
  ) * 100
)


# ================================================================
# 26. OUTCOME BY MAIN PREDICTORS
# ================================================================

cat("\n")
cat("============================================================\n")
cat("23. OUTCOME BY MAIN PREDICTORS\n")
cat("============================================================\n")


cat("\nUnder-five mortality by education:\n")

print(
  analysis_data %>%

    group_by(
      country,
      mat_edu
    ) %>%

    summarise(

      N = n(),

      deaths =
        sum(
          died_under5 == 1
        ),

      mortality_percent =
        100 *
        deaths /
        N,

      .groups = "drop"
    )
)


cat("\nUnder-five mortality by wealth:\n")

print(
  analysis_data %>%

    group_by(
      country,
      wealth
    ) %>%

    summarise(

      N = n(),

      deaths =
        sum(
          died_under5 == 1
        ),

      mortality_percent =
        100 *
        deaths /
        N,

      .groups = "drop"
    )
)


cat("\nUnder-five mortality by residence:\n")

print(
  analysis_data %>%

    group_by(
      country,
      residence
    ) %>%

    summarise(

      N = n(),

      deaths =
        sum(
          died_under5 == 1
        ),

      mortality_percent =
        100 *
        deaths /
        N,

      .groups = "drop"
    )
)


cat("\nUnder-five mortality by sex:\n")

print(
  analysis_data %>%

    group_by(
      country,
      child_sex
    ) %>%

    summarise(

      N = n(),

      deaths =
        sum(
          died_under5 == 1
        ),

      mortality_percent =
        100 *
        deaths /
        N,

      .groups = "drop"
    )
)


# ================================================================
# 27. WEIGHTED DESCRIPTIVE CHECK
# ================================================================

cat("\n")
cat("============================================================\n")
cat("24. WEIGHTED DESCRIPTIVE CHECK\n")
cat("============================================================\n")


weighted_mortality <- analysis_data %>%

  group_by(country) %>%

  summarise(

    unweighted_N = n(),

    unweighted_deaths =
      sum(died_under5 == 1),

    unweighted_mortality =
      100 *
      unweighted_deaths /
      unweighted_N,

    weighted_deaths =
      sum(
        weight_norm *
          (died_under5 == 1)
      ),

    weighted_total =
      sum(weight_norm),

    weighted_mortality =
      100 *
      weighted_deaths /
      weighted_total,

    .groups = "drop"
  )

print(weighted_mortality)


# ================================================================
# 28. FINAL COMPLETE-CASE CHECK
# ================================================================

cat("\n")
cat("============================================================\n")
cat("25. FINAL COMPLETE-CASE CHECK\n")
cat("============================================================\n")


final_variables <- c(
  "died_under5",
  "mat_edu",
  "wealth",
  "residence",
  "child_sex",
  "birth_order_cat",
  "birth_interval_cat",
  "weight_raw",
  "cluster_id",
  "mother_id"
)


final_missing <- analysis_data %>%

  summarise(

    across(
      all_of(final_variables),
      ~ sum(is.na(.))
    )
  )

print(final_missing)


# ================================================================
# 29. FINAL STRUCTURE
# ================================================================

cat("\n")
cat("============================================================\n")
cat("26. FINAL DATA STRUCTURE\n")
cat("============================================================\n")

cat("\nRows:", nrow(analysis_data), "\n")
cat("Columns:", ncol(analysis_data), "\n")

cat("\nVariables:\n")
print(names(analysis_data))


# ================================================================
# 30. AUTOMATIC RED-FLAG CHECK
# ================================================================

cat("\n")
cat("============================================================\n")
cat("27. AUTOMATIC RED-FLAG CHECK\n")
cat("============================================================\n")


# ------------------------------------------------
# FLAG 1 — FILES
# ------------------------------------------------

if (all(sapply(countries, file.exists))) {

  cat("\n[OK] All four DHS files found.\n")

} else {

  cat("\n[WARNING] One or more DHS files missing.\n")
}


# ------------------------------------------------
# FLAG 2 — NEGATIVE AGE
# ------------------------------------------------

negative_age <- sum(
  pooled_data$age_at_interview < 0,
  na.rm = TRUE
)

if (negative_age == 0) {

  cat("[OK] No negative child ages.\n")

} else {

  cat(
    "[WARNING] Negative child ages:",
    negative_age,
    "\n"
  )
}


# ------------------------------------------------
# FLAG 3 — INDIA SPECIAL B11
# ------------------------------------------------

india_997 <- sum(
  pooled_data$country == "india" &
  pooled_data$b11 >= 900,
  na.rm = TRUE
)

if (india_997 == 0) {

  cat("[OK] No India b11 >=900.\n")

} else {

  cat(
    "[WARNING] India b11 >=900 cases:",
    india_997,
    "\n"
  )
}


# ------------------------------------------------
# FLAG 4 — ZERO WEIGHTS
# ------------------------------------------------

zero_weights <- pooled_data %>%
  filter(weight_raw == 0) %>%
  nrow()

if (zero_weights == 0) {

  cat("[OK] No zero weights.\n")

} else {

  cat(
    "[WARNING] Zero-weight observations:",
    zero_weights,
    "\n"
  )
}


# ------------------------------------------------
# FLAG 5 — NEGATIVE WEIGHTS
# ------------------------------------------------

negative_weights <- pooled_data %>%
  filter(weight_raw < 0) %>%
  nrow()

if (negative_weights == 0) {

  cat("[OK] No negative weights.\n")

} else {

  cat(
    "[CRITICAL] Negative weights:",
    negative_weights,
    "\n"
  )
}


# ------------------------------------------------
# FLAG 6 — DUPLICATES
# ------------------------------------------------

if (nrow(duplicate_check) == 0) {

  cat("[OK] No duplicate child identifiers.\n")

} else {

  cat(
    "[WARNING] Duplicate child identifiers:",
    nrow(duplicate_check),
    "\n"
  )
}


# ------------------------------------------------
# FLAG 7 — MISSING FINAL VARIABLES
# ------------------------------------------------

total_final_missing <-
  sum(
    final_missing
  )

if (total_final_missing == 0) {

  cat("[OK] No missing values in final analysis variables.\n")

} else {

  cat(
    "[WARNING] Missing values remain in final variables:",
    total_final_missing,
    "\n"
  )
}


# ------------------------------------------------
# FLAG 8 — OUTCOME
# ------------------------------------------------

outcome_missing <-
  sum(
    is.na(
      analysis_data$died_under5
    )
  )

if (outcome_missing == 0) {

  cat("[OK] No missing outcome in proposed analytic sample.\n")

} else {

  cat(
    "[WARNING] Missing outcome:",
    outcome_missing,
    "\n"
  )
}


# ================================================================
# 31. SAVE DIAGNOSTIC DATA
# ================================================================

cat("\n")
cat("============================================================\n")
cat("28. SAVING DIAGNOSTIC DATA\n")
cat("============================================================\n")


output_dir <-
  "C:/Users/User/Downloads/outputs"

dir.create(
  output_dir,
  showWarnings = FALSE,
  recursive = TRUE
)


write.csv(
  analysis_country_summary,
  file.path(
    output_dir,
    "analysis_country_summary.csv"
  ),
  row.names = FALSE
)


write.csv(
  weight_summary,
  file.path(
    output_dir,
    "weight_summary.csv"
  ),
  row.names = FALSE
)


write.csv(
  birth_interval_summary,
  file.path(
    output_dir,
    "birth_interval_summary.csv"
  ),
  row.names = FALSE
)


write.csv(
  outcome_summary,
  file.path(
    output_dir,
    "outcome_summary.csv"
  ),
  row.names = FALSE
)


write.csv(
  missing_summary,
  file.path(
    output_dir,
    "missing_summary.csv"
  ),
  row.names = FALSE
)


# Save the prepared data as RDS
saveRDS(
  analysis_data,
  file.path(
    output_dir,
    "pooled_dhs_analysis_data.rds"
  )
)


cat("\nFiles saved to:\n")
cat(output_dir, "\n")


# ================================================================
# 32. FINAL MESSAGE
# ================================================================

cat("\n")
cat("============================================================\n")
cat("DIAGNOSTIC SCRIPT FINISHED\n")
cat("============================================================\n")

cat("\nIMPORTANT:\n")
cat("DO NOT RUN THE REGRESSION MODEL YET.\n")
cat("Send me the complete console output from this script.\n")
cat("I will inspect the data preparation before we fit the model.\n")

cat("\n============================================================\n")