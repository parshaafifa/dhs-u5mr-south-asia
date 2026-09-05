library(haven)
library(dplyr)

# ============================================================
# 1. CORRECT DHS FILE PATHS
# ============================================================

countries <- list(
  bangladesh = "C:/Users/User/Downloads/DHS DATASETS/BD_2022_DHS_08312026_2337_222099/BDBR81DT/BDBR81FL.DTA",
  nepal      = "C:/Users/User/Downloads/DHS DATASETS/NP_2022_DHS_08312026_2342_222099/NPBR82DT/NPBR82FL.DTA",
  pakistan   = "C:/Users/User/Downloads/DHS DATASETS/PK_2017-18_DHS_09012026_013_222099/PKBR71DT/PKBR71FL.DTA",
  india      = "C:/Users/User/Downloads/DHS DATASETS/IABR7EDT/IABR7EFL.DTA"
)

# Check paths
cat("\n================ FILE CHECK ================\n")
print(file.exists(unlist(countries)))


# ============================================================
# 2. VARIABLES NEEDED FOR FULL DIAGNOSTIC CHECK
# ============================================================

vars_needed <- c(
  "v001",   # cluster / PSU
  "v002",   # household number
  "v003",   # respondent line number
  "v005",   # DHS sampling weight
  "v008",   # date of interview in CMC
  "v022",   # sampling strata
  "v023",   # strata alternative
  "v025",   # urban/rural
  "v106",   # maternal education
  "v190",   # wealth index
  "b3",     # date of birth in CMC
  "b4",     # sex of child
  "b5",     # child alive/dead
  "b7",     # age at death in months
  "b11",    # preceding birth interval
  "bord"    # birth order
)


# ============================================================
# 3. READ DATA COUNTRY BY COUNTRY
# ============================================================

all_data <- list()

for (cn in names(countries)) {
  
  cat("\n\n====================================================\n")
  cat("READING:", toupper(cn), "\n")
  cat("====================================================\n")
  
  dat <- read_dta(
    countries[[cn]],
    col_select = all_of(vars_needed)
  )
  
  dat$country <- cn
  
  all_data[[cn]] <- dat
  
  cat("Rows:", nrow(dat), "\n")
  cat("Columns:", ncol(dat), "\n")
}


# ============================================================
# 4. BASIC SAMPLE SIZE CHECK
# ============================================================

cat("\n\n================ SAMPLE SIZE ================\n")

sample_sizes <- sapply(all_data, nrow)

print(sample_sizes)


# ============================================================
# 5. MATERNAL EDUCATION HARMONIZATION CHECK
# ============================================================

cat("\n\n================ MATERNAL EDUCATION ================\n")

for (cn in names(all_data)) {
  
  cat("\n---", toupper(cn), "---\n")
  
  x <- all_data[[cn]]
  
  print(table(x$v106, useNA = "ifany"))
  
  print(
    table(
      haven::as_factor(x$v106),
      useNA = "ifany"
    )
  )
  
  cat("\nValue labels:\n")
  print(attr(x$v106, "labels"))
}


# ============================================================
# 6. CHECK OTHER CATEGORICAL VARIABLES
# ============================================================

cat("\n\n================ RESIDENCE ================\n")

for (cn in names(all_data)) {
  cat("\n---", toupper(cn), "---\n")
  print(table(all_data[[cn]]$v025, useNA = "ifany"))
  print(table(haven::as_factor(all_data[[cn]]$v025), useNA = "ifany"))
}


cat("\n\n================ WEALTH ================\n")

for (cn in names(all_data)) {
  cat("\n---", toupper(cn), "---\n")
  print(table(all_data[[cn]]$v190, useNA = "ifany"))
  print(table(haven::as_factor(all_data[[cn]]$v190), useNA = "ifany"))
}


cat("\n\n================ CHILD SEX ================\n")

for (cn in names(all_data)) {
  cat("\n---", toupper(cn), "---\n")
  print(table(all_data[[cn]]$b4, useNA = "ifany"))
  print(table(haven::as_factor(all_data[[cn]]$b4), useNA = "ifany"))
}


# ============================================================
# 7. OUTCOME CHECK: b5 = ALIVE/DEAD
# ============================================================

cat("\n\n================ CHILD STATUS (b5) ================\n")

for (cn in names(all_data)) {
  
  cat("\n---", toupper(cn), "---\n")
  
  print(table(all_data[[cn]]$b5, useNA = "ifany"))
  
  print(
    table(
      haven::as_factor(all_data[[cn]]$b5),
      useNA = "ifany"
    )
  )
}


# ============================================================
# 8. AGE AT DEATH (b7) CHECK
# ============================================================

cat("\n\n================ AGE AT DEATH (b7) ================\n")

for (cn in names(all_data)) {
  
  cat("\n---", toupper(cn), "---\n")
  
  x <- all_data[[cn]]
  
  cat("Missing b7:\n")
  print(sum(is.na(x$b7)))
  
  cat("Summary of b7:\n")
  print(summary(x$b7))
  
  cat("Deaths with b7 < 60 months:\n")
  print(
    sum(
      x$b5 == 0 &
      !is.na(x$b7) &
      x$b7 < 60,
      na.rm = TRUE
    )
  )
  
  cat("Deaths with missing b7:\n")
  print(
    sum(
      x$b5 == 0 &
      is.na(x$b7),
      na.rm = TRUE
    )
  )
  
  cat("Deaths with b7 >= 60:\n")
  print(
    sum(
      x$b5 == 0 &
      !is.na(x$b7) &
      x$b7 >= 60,
      na.rm = TRUE
    )
  )
}


# ============================================================
# 9. CHECK LIVING CHILDREN AND AGE AT INTERVIEW
# ============================================================

cat("\n\n================ AGE AT INTERVIEW ================\n")

for (cn in names(all_data)) {
  
  cat("\n---", toupper(cn), "---\n")
  
  x <- all_data[[cn]]
  
  x <- x %>%
    mutate(
      age_interview_months = v008 - b3
    )
  
  cat("Summary of age at interview:\n")
  print(summary(x$age_interview_months))
  
  cat("\nLiving children younger than 60 months:\n")
  print(
    sum(
      x$b5 == 1 &
      x$age_interview_months < 60,
      na.rm = TRUE
    )
  )
  
  cat("Living children >= 60 months:\n")
  print(
    sum(
      x$b5 == 1 &
      x$age_interview_months >= 60,
      na.rm = TRUE
    )
  )
  
  cat("Negative ages:\n")
  print(
    sum(
      x$age_interview_months < 0,
      na.rm = TRUE
    )
  )
}


# ============================================================
# 10. BIRTH INTERVAL (b11) CHECK
# ============================================================

cat("\n\n================ BIRTH INTERVAL (b11) ================\n")

for (cn in names(all_data)) {
  
  cat("\n---", toupper(cn), "---\n")
  
  x <- all_data[[cn]]
  
  cat("Summary:\n")
  print(summary(x$b11))
  
  cat("\nFrequency of lowest values:\n")
  print(
    table(
      x$b11[x$b11 <= 24],
      useNA = "ifany"
    )
  )
  
  cat("\nMissing:\n")
  print(sum(is.na(x$b11)))
  
  cat("\nBirths with b11 = 0:\n")
  print(sum(x$b11 == 0, na.rm = TRUE))
}


# ============================================================
# 11. BIRTH ORDER CHECK
# ============================================================

cat("\n\n================ BIRTH ORDER (bord) ================\n")

for (cn in names(all_data)) {
  
  cat("\n---", toupper(cn), "---\n")
  
  x <- all_data[[cn]]
  
  print(summary(x$bord))
  
  cat("Minimum birth order:\n")
  print(min(x$bord, na.rm = TRUE))
  
  cat("Maximum birth order:\n")
  print(max(x$bord, na.rm = TRUE))
  
  cat("Missing:\n")
  print(sum(is.na(x$bord)))
}


# ============================================================
# 12. DHS WEIGHT CHECK
# ============================================================

cat("\n\n================ DHS WEIGHTS ================\n")

for (cn in names(all_data)) {
  
  cat("\n---", toupper(cn), "---\n")
  
  x <- all_data[[cn]]
  
  weight <- x$v005 / 1000000
  
  print(summary(weight))
  
  cat("Missing:", sum(is.na(weight)), "\n")
  cat("Zero:", sum(weight == 0, na.rm = TRUE), "\n")
  cat("Negative:", sum(weight < 0, na.rm = TRUE), "\n")
  
  cat("Mean:", mean(weight, na.rm = TRUE), "\n")
  cat("Median:", median(weight, na.rm = TRUE), "\n")
}


# ============================================================
# 13. PSU / CLUSTER CHECK
# ============================================================

cat("\n\n================ PSU / CLUSTER ================\n")

for (cn in names(all_data)) {
  
  cat("\n---", toupper(cn), "---\n")
  
  x <- all_data[[cn]]
  
  cat("Number of PSUs:", n_distinct(x$v001), "\n")
  
  cat("Number of households:", 
      n_distinct(paste(x$v001, x$v002)), 
      "\n")
  
  cat("Number of mothers/respondents:",
      n_distinct(paste(x$v001, x$v002, x$v003)),
      "\n")
}


# ============================================================
# 14. STRATA CHECK
# ============================================================

cat("\n\n================ STRATA ================\n")

for (cn in names(all_data)) {
  
  cat("\n---", toupper(cn), "---\n")
  
  x <- all_data[[cn]]
  
  cat("Unique v022 strata:", n_distinct(x$v022), "\n")
  cat("Unique v023 strata:", n_distinct(x$v023), "\n")
  
  cat("\nFirst few v022 values:\n")
  print(head(unique(x$v022), 20))
  
  cat("\nFirst few v023 values:\n")
  print(head(unique(x$v023), 20))
}


# ============================================================
# 15. REPEATED CHILDREN PER MOTHER
# ============================================================

cat("\n\n================ CHILDREN PER MOTHER ================\n")

for (cn in names(all_data)) {
  
  cat("\n---", toupper(cn), "---\n")
  
  x <- all_data[[cn]]
  
  mother_id <- paste(x$v001, x$v002, x$v003, sep = "_")
  
  children_per_mother <- table(mother_id)
  
  cat("Number of mothers:", length(children_per_mother), "\n")
  
  cat("Mothers with >1 child:",
      sum(children_per_mother > 1),
      "\n")
  
  cat("Maximum children per mother:",
      max(children_per_mother),
      "\n")
}


# ============================================================
# 16. MISSING DATA CHECK FOR ALL MODEL VARIABLES
# ============================================================

cat("\n\n================ MISSING DATA ================\n")

for (cn in names(all_data)) {
  
  cat("\n---", toupper(cn), "---\n")
  
  x <- all_data[[cn]]
  
  missing_table <- data.frame(
    variable = names(x),
    missing_n = sapply(x, function(z) sum(is.na(z))),
    missing_percent =
      sapply(x, function(z) mean(is.na(z)) * 100)
  )
  
  print(missing_table)
}


# ============================================================
# 17. CURRENT OUTCOME DEFINITION VS CORRECTED VERSION
# ============================================================

cat("\n\n================ OUTCOME DEFINITION CHECK ================\n")

for (cn in names(all_data)) {
  
  cat("\n---", toupper(cn), "---\n")
  
  x <- all_data[[cn]]
  
  # YOUR ORIGINAL DEFINITION
  original <- ifelse(
    x$b5 == 0 &
      !is.na(x$b7) &
      x$b7 < 60,
    1,
    0
  )
  
  # SAFER DEFINITION
  corrected <- case_when(
    x$b5 == 0 & !is.na(x$b7) & x$b7 < 60 ~ 1L,
    x$b5 == 1 ~ 0L,
    TRUE ~ NA_integer_
  )
  
  cat("\nOriginal outcome:\n")
  print(table(original, useNA = "ifany"))
  
  cat("\nCorrected outcome:\n")
  print(table(corrected, useNA = "ifany"))
  
  cat("\nDifference between definitions:\n")
  print(
    table(
      original,
      corrected,
      useNA = "ifany"
    )
  )
}


# ============================================================
# 18. CREATE POOLED DIAGNOSTIC DATASET
# ============================================================

diagnostic_data <- bind_rows(all_data)


# ============================================================
# 19. CHECK COUNTRY-SPECIFIC OUTCOME
# ============================================================

cat("\n\n================ COUNTRY × OUTCOME ================\n")

diagnostic_data <- diagnostic_data %>%
  mutate(
    died_under5_corrected = case_when(
      b5 == 0 & !is.na(b7) & b7 < 60 ~ 1L,
      b5 == 1 ~ 0L,
      TRUE ~ NA_integer_
    )
  )

print(
  table(
    diagnostic_data$country,
    diagnostic_data$died_under5_corrected,
    useNA = "ifany"
  )
)


# ============================================================
# 20. COUNTRY × EDUCATION
# ============================================================

cat("\n\n================ COUNTRY × EDUCATION ================\n")

diagnostic_data <- diagnostic_data %>%
  mutate(
    mat_edu = case_when(
      v106 == 0 ~ "no education",
      v106 == 1 ~ "primary",
      v106 == 2 ~ "secondary",
      v106 == 3 ~ "higher",
      TRUE ~ NA_character_
    ),
    
    mat_edu = factor(
      mat_edu,
      levels = c(
        "no education",
        "primary",
        "secondary",
        "higher"
      )
    )
  )

print(
  table(
    diagnostic_data$country,
    diagnostic_data$mat_edu,
    useNA = "ifany"
  )
)


# ============================================================
# 21. CHECK COMPLETE CASES
# ============================================================

cat("\n\n================ COMPLETE CASE CHECK ================\n")

analysis_variables <- c(
  "died_under5_corrected",
  "mat_edu",
  "v190",
  "v025",
  "bord",
  "b11",
  "b4",
  "v005"
)

complete_cases <- complete.cases(
  diagnostic_data[, analysis_variables]
)

cat("Total pooled observations:", nrow(diagnostic_data), "\n")
cat("Complete cases:", sum(complete_cases), "\n")
cat("Excluded:", sum(!complete_cases), "\n")
cat(
  "Percent complete:",
  mean(complete_cases) * 100,
  "%\n"
)


# ============================================================
# 22. COUNTRY-SPECIFIC COMPLETE CASES
# ============================================================

cat("\n\n================ COUNTRY-SPECIFIC COMPLETE CASES ================\n")

country_complete <- diagnostic_data %>%
  mutate(
    complete = complete.cases(
      across(all_of(analysis_variables))
    )
  ) %>%
  group_by(country) %>%
  summarise(
    total = n(),
    complete = sum(complete),
    excluded = sum(!complete),
    complete_percent = mean(complete) * 100
  )

print(country_complete)


# ============================================================
# 23. FINAL SUMMARY
# ============================================================

cat("\n\n====================================================\n")
cat("DIAGNOSTIC CHECK FINISHED\n")
cat("====================================================\n")

cat("\nIMPORTANT: NO MULTILEVEL MODEL WAS FIT.\n")
cat("No glmer() was run.\n")
cat("No long model computation was performed.\n")