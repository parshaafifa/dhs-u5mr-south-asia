## =========================================================
## DHS U5MR SPATIAL ANALYSIS PIPELINE (MEMORY-OPTIMIZED)
## Bangladesh, Nepal, Pakistan, India
## Processes ONE COUNTRY AT A TIME to avoid memory crashes on low-RAM machines.
## Run this script once per country by changing TARGET_COUNTRY below,
## OR run all countries in sequence (each one clears memory before the next).
## =========================================================

## ---- 0. PACKAGES ----
#install.packages(c("haven","dplyr","survey","sf","spdep","tmap","ggplot2","lme4","broom.mixed","rmapshaper"))
library(haven)
library(dplyr)
library(survey)
library(sf)
library(spdep)
library(tmap)
library(ggplot2)
library(rmapshaper)   # for simplifying heavy GADM shapefiles

dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/intermediate", showWarnings = FALSE)  # stores per-country results

## ---- 1. FILE PATHS (EDIT THESE) ----
countries <- list(
  bangladesh = list(
    br   = "C:/Users/User/Downloads/BD_2022_DHS_08312026_2337_222099/BDBR81DT/BDBR81FL.DTA",
    ge   = "C:/Users/User/Downloads/BD_2022_DHS_08312026_2337_222099/BDGE81FL/BDGE81FL.shp",
    gadm = "C:/Users/User/Downloads/gadm41_BGD_shp (2)/gadm41_BGD_1.shp"
  ),
  nepal = list(
    br   = "C:/Users/User/Downloads/NP_2022_DHS_08312026_2342_222099/NPBR82DT/NPBR82FL.DTA",
    ge   = "C:/Users/User/Downloads/NP_2022_DHS_08312026_2342_222099/NPGE82FL/NPGE82FL.shp",
    gadm = "C:/Users/User/Downloads/gadm41_NPL_shp/gadm41_NPL_1.shp"
  ),
  pakistan = list(
    br   = "C:/Users/User/Downloads/PK_2017-18_DHS_09012026_013_222099/PKBR71DT/PKBR71FL.DTA",
    ge   = "C:/Users/User/Downloads/PK_2017-18_DHS_09012026_013_222099/PKGE71FL/PKGE71FL.shp",
    gadm = "C:/Users/User/Downloads/gadm41_PAK_shp/gadm41_PAK_1.shp"
  ),
  india = list(
    br   = "C:/Users/User/Downloads/IABR7EDT/IABR7EFL.DTA",
    ge   = "C:/Users/User/Downloads/IAGE7AFL/IAGE7AFL.shp",
    gadm = "C:/Users/User/Downloads/gadm41_IND_shp/gadm41_IND_1.shp"
  )
)

## ---- 2. SET WHICH COUNTRY TO RUN ----
TARGET_COUNTRY <- "india"

cn   <- TARGET_COUNTRY
info <- countries[[cn]]

cat("=== Processing:", cn, "===\n")

## ---- 3. COMPUTE CLUSTER-LEVEL U5MR ----
cat("Step 1/5: Reading BR file and computing U5MR...\n")
br <- read_dta(info$br)

br <- br %>%
  mutate(
    weight = v005 / 1000000,
    died_under5 = ifelse(b5 == 0 & !is.na(b7) & b7 < 60, 1, 0)
  ) %>%
  filter(!is.na(v001))

cluster_u5mr <- br %>%
  group_by(v001, v024) %>%
  summarise(
    n_births = n(),
    deaths   = sum(died_under5, na.rm = TRUE),
    u5mr     = (deaths / n_births) * 1000,
    .groups = "drop"
  ) %>%
  mutate(country = cn)

rm(br); gc()   # free memory immediately after use

## ---- 4. SPATIAL JOIN: CLUSTER GPS -> ADMIN-1 REGION (SIMPLIFIED GADM) ----
cat("Step 2/5: Reading and simplifying GADM boundaries...\n")
gadm <- st_read(info$gadm, quiet = TRUE) %>%
  ms_simplify(keep = 0.05, keep_shapes = TRUE)   # keeps 5% of vertices - big memory saver

cat("Step 3/5: Reading GPS points and joining to regions...\n")
ge <- st_read(info$ge, quiet = TRUE) %>%
  filter(LATNUM != 0 & LONGNUM != 0) %>%
  rename(v001 = DHSCLUST) %>%
  select(v001, geometry) %>%
  st_transform(st_crs(gadm))

joined <- st_join(ge, gadm) %>%
  st_drop_geometry() %>%
  select(v001, NAME_1)

rm(ge); gc()

full_data <- cluster_u5mr %>% left_join(joined, by = "v001")

## ---- 5. AGGREGATE TO REGION LEVEL ----
region_u5mr <- full_data %>%
  filter(!is.na(NAME_1)) %>%
  group_by(country, NAME_1) %>%
  summarise(
    total_births = sum(n_births),
    total_deaths = sum(deaths),
    region_u5mr  = (total_deaths / total_births) * 1000,
    .groups = "drop"
  )

# Save this country's region-level result to disk (so we can combine all 4 later)
saveRDS(region_u5mr, paste0("outputs/intermediate/region_u5mr_", cn, ".rds"))
write.csv(region_u5mr, paste0("outputs/region_u5mr_", cn, ".csv"), row.names = FALSE)

## ---- 6. CHOROPLETH MAP ----
cat("Step 4/5: Drawing choropleth...\n")
merged <- gadm %>% left_join(region_u5mr, by = "NAME_1")

tm <- tm_shape(merged) +
  tm_polygons("region_u5mr", palette = "Reds", title = "U5MR per 1,000", style = "jenks") +
  tm_layout(title = paste("Under-5 Mortality Rate -", cn))

tmap_save(tm, filename = paste0("outputs/choropleth_", cn, ".png"), width = 8, height = 6, dpi = 300)

## ---- 7. MORAN'S I + LISA ----
cat("Step 5/5: Computing Moran's I and LISA...\n")
gadm_valid <- merged %>% filter(!is.na(region_u5mr))

nb <- poly2nb(gadm_valid, queen = TRUE)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

global_moran <- moran.test(gadm_valid$region_u5mr, lw, zero.policy = TRUE)
cat("Global Moran's I for", cn, ":\n")
print(global_moran)

local_moran <- localmoran(gadm_valid$region_u5mr, lw, zero.policy = TRUE)
gadm_valid$lisa_I    <- local_moran[, "Ii"]
gadm_valid$lisa_pval <- local_moran[, "Pr(z != E(Ii))"]

st_write(gadm_valid, paste0("outputs/lisa_", cn, ".shp"), delete_layer = TRUE)

# Save the Moran's I result too
saveRDS(global_moran, paste0("outputs/intermediate/morans_", cn, ".rds"))

cat("\n=== DONE:", cn, "===\n")
cat("Now: restart R (Ctrl+Shift+F10 in RStudio, or close/reopen R terminal),\n")
cat("change TARGET_COUNTRY to the next country, and re-run this script.\n")

## =========================================================
## AFTER ALL 4 COUNTRIES ARE DONE, run this separately to combine results
## and build the pooled multilevel model (lighter on memory since
## it only reads the small saved .rds/.csv files, not raw shapefiles):
## =========================================================

# all_regions <- lapply(names(countries), function(x) {
#   readRDS(paste0("outputs/intermediate/region_u5mr_", x, ".rds"))
# }) %>% bind_rows()
# write.csv(all_regions, "outputs/region_u5mr_ALL_COUNTRIES.csv", row.names = FALSE)
