# Build the country-year analysis dataset for 2000--2019.
# A conflict observed in year t is assigned to year t + 1 (lag 1).

library(dplyr)
library(tidyr)
library(janitor)

wb_to_long <- function(path, variable_name) {
  read.csv(path, check.names = FALSE) |>
    select(iso, indicator, X2000:X2019) |>
    pivot_longer(
      cols = starts_with("X"),
      names_to = "year",
      names_prefix = "X",
      values_to = variable_name
    ) |>
    mutate(year = as.integer(year)) |>
    select(-indicator)
}

# Confounders: mortality measures, one row per ISO code and year.
infant_mortality <- wb_to_long(
  "../data/raw/infant_mortality.csv", "infant_mortality"
)
maternal_mortality <- wb_to_long(
  "../data/raw/maternal_mortality.csv", "maternal_mortality"
)
neonatal_mortality <- wb_to_long(
  "../data/raw/neonatal_mortality.csv", "neonatal_mortality"
)
under5_mortality <- wb_to_long(
  "../data/raw/under5_mortality.csv", "under5_mortality"
)

# Mortality data establish the analysis panel (each unique ISO-year once).
analysis_data <- infant_mortality |>
  full_join(maternal_mortality, by = c("iso", "year")) |>
  full_join(neonatal_mortality, by = c("iso", "year")) |>
  full_join(under5_mortality, by = c("iso", "year")) |>
  distinct(iso, year, .keep_all = TRUE)

# Disaster confounders: indicators of at least one event in a country-year.
disaster_data <- read.csv("../data/raw/disaster.csv", check.names = FALSE) |>
  clean_names() |>
  filter(
    year >= 2000, year <= 2019,
    disaster_type %in% c("Earthquake", "Drought"),
    !is.na(iso), iso != ""
  ) |>
  transmute(
    iso,
    year = as.integer(year),
    earthquake = as.integer(disaster_type == "Earthquake"),
    drought = as.integer(disaster_type == "Drought")
  ) |>
  group_by(iso, year) |> ## this is a smart way of coding max and as integer, without using pivot
  summarise(
    earthquake = max(earthquake),
    drought = max(drought),
    .groups = "drop"
  )

# Exposure: sum battle-related deaths by country-year, use the >= 25 threshold,
# then assign it to the following outcome year (e.g., 1999 -> 2000).
conflict_data <- read.csv("../data/raw/conflict.csv", check.names = FALSE) |>
  filter(!is.na(iso), iso != "", !is.na(year)) |>
  group_by(iso, year) |>
  summarise(battle_deaths = sum(best, na.rm = TRUE), .groups = "drop") |>
  transmute(
    iso,
    year = as.integer(year) + 1L,
    conflict_lag1 = as.integer(battle_deaths >= 25)
  )

final_data <- analysis_data |>
  left_join(disaster_data, by = c("iso", "year")) |> ## disagree with leftjoin, in case there was missing data in outcomes data
  left_join(conflict_data, by = c("iso", "year")) |>
  mutate(
    earthquake = coalesce(earthquake, 0L), 
    drought = coalesce(drought, 0L),
    conflict_lag1 = coalesce(conflict_lag1, 0L) ## disagree with coagent, should retain NA for missing data reporting
  ) |>
  arrange(iso, year)

stopifnot(
  !anyDuplicated(final_data[c("iso", "year")]),
  all(final_data$earthquake %in% c(0L, 1L)),
  all(final_data$drought %in% c(0L, 1L)),
  all(final_data$conflict_lag1 %in% c(0L, 1L))
)

write.csv(final_data, "../data/processed/data.csv", row.names = FALSE, na = "")
