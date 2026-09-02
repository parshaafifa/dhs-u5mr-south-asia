## =========================================================
## POOLED MULTILEVEL LOGISTIC REGRESSION
## Run this AFTER all 4 countries have finished in the main pipeline.
## This only reads BR files (needed for individual-level covariates) -
## no shapefiles involved, so it's much lighter on memory.
## =========================================================

library(haven)
library(dplyr)
library(survey)   # design-based weighted regression - much lighter than WeMix

## ---- 1. FILE PATHS (same as your main script) ----
countries <- list(
  bangladesh = "C:/Users/User/Downloads/BD_2022_DHS_08312026_2337_222099/BDBR81DT/BDBR81FL.DTA",
  nepal      = "C:/Users/User/Downloads/NP_2022_DHS_08312026_2342_222099/NPBR82DT/NPBR82FL.DTA",
  pakistan   = "C:/Users/User/Downloads/PK_2017-18_DHS_09012026_013_222099/PKBR71DT/PKBR71FL.DTA",
  india      = "C:/Users/User/Downloads/IABR7EDT/IABR7EFL.DTA"
)

## ---- 2. BUILD CHILD-LEVEL DATASET PER COUNTRY ----
build_child_level_data <- function(br_path, country_name) {
  br <- read_dta(br_path)

  df <- br %>%
    mutate(
      died_under5 = ifelse(b5 == 0 & !is.na(b7) & b7 < 60, 1, 0),
      country     = country_name,
      mat_edu     = as_factor(v106),
      wealth      = as_factor(v190),
      residence   = as_factor(v025),
      birth_order = bord,
      birth_int   = b11,
      child_sex   = as_factor(b4),
      cluster_id  = paste0(country_name, "_", v001),
      weight_raw  = v005 / 1000000     # DHS standard weight scaling
    ) %>%
    select(died_under5, country, mat_edu, wealth, residence,
           birth_order, birth_int, child_sex, cluster_id, weight_raw) %>%
    filter(!is.na(died_under5), !is.na(weight_raw))

  rm(br); gc()
  return(df)
}

cat("Reading and preparing data for all 4 countries...\n")
pooled_child_data <- lapply(names(countries), function(cn) {
  cat(" -", cn, "\n")
  build_child_level_data(countries[[cn]], cn)
}) %>% bind_rows()

cat("\nTotal children in pooled dataset:", nrow(pooled_child_data), "\n")
cat("Deaths:", sum(pooled_child_data$died_under5), "\n")
cat("Countries:", paste(unique(pooled_child_data$country), collapse = ", "), "\n\n")

## ---- 3. BUILD SURVEY DESIGN OBJECT ----
## This properly accounts for DHS clustering (cluster_id) and sampling weight (v005)
## using design-based standard errors - far lighter computationally than WeMix,
## since it doesn't estimate a full random-effects variance structure.
cat("\nBuilding survey design object...\n")
des <- svydesign(
  id      = ~cluster_id,
  weights = ~weight_raw,
  data    = pooled_child_data
)

## ---- 4. FIT THE WEIGHTED LOGISTIC REGRESSION ----
cat("Fitting design-based weighted logistic regression (much lighter than WeMix)...\n")

model <- svyglm(
  died_under5 ~ mat_edu + wealth + residence + birth_order + birth_int +
    child_sex + country,
  design = des,
  family = quasibinomial(link = "logit")   # avoids a spurious non-integer-successes warning with weights
)

cat("\n=== MODEL SUMMARY (WEIGHTED, DESIGN-BASED) ===\n")
print(summary(model))

## ---- 5. TIDY OUTPUT: ODDS RATIOS + CONFIDENCE INTERVALS ----
coefs <- summary(model)$coefficients
ci <- confint(model)
model_results <- data.frame(
  term      = rownames(coefs),
  estimate  = exp(coefs[, "Estimate"]),
  std.error = coefs[, "Std. Error"],
  statistic = coefs[, "t value"],
  p.value   = coefs[, "Pr(>|t|)"],
  conf.low  = exp(ci[, 1]),
  conf.high = exp(ci[, 2])
)
print(model_results)

dir.create("outputs", showWarnings = FALSE)
write.csv(model_results, "outputs/multilevel_model_results.csv", row.names = FALSE)

cat("\nDone. Results saved to outputs/multilevel_model_results.csv\n")
cat("Columns: term | estimate (odds ratio) | std.error | statistic | p.value | conf.low | conf.high\n")
