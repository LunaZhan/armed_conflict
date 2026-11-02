library(dplyr)
library(tidyr)
library(purrr)
# read in all other datasets 

infmor0 <- read.csv("./data/raw/infant_mortality.csv", header = TRUE)
matmor0 <- read.csv("./data/raw/maternal_mortality.csv", header = TRUE)
neomor0 <- read.csv("./data/raw/neonatal_mortality.csv", header = TRUE)
un5mor0 <- read.csv("./data/raw/under5_mortality.csv", header = TRUE)



### write a function that pivot each dataset to longer
wbfun <- function(dataname, varname){
  dataname |>
    dplyr::select(iso, indicator, X2000:X2019) |>
    pivot_longer(cols = starts_with("X"),
                 names_to = "year",
                 names_prefix = "X",
                 values_to = varname) |>
    mutate(year = as.numeric(year)) |>
    arrange(iso, year) |> 
    select(-indicator)
}

matmor <- wbfun(dataname = matmor0, varname = "matmor")
infmor <- wbfun(dataname = infmor0, varname = "infmor")
neomor <- wbfun(dataname = neomor0, varname = "neomor")
un5mor <- wbfun(dataname = un5mor0, varname = "un5mor")

#put all data frames into list
wblist <- list(matmor, infmor, neomor, un5mor)

#merge all data frames in list
wbdata <- wblist |> reduce(full_join, by = c('iso', 'year')) ## apply full join to each item in list iteratively 



#prepare disaster data 
disaster0 <- read.csv("./data/raw/disaster.csv", header = TRUE)

disaster0 <- disaster0 |> 
  janitor::clean_names()

disaster <- disaster0 |> 
  filter(disaster_type == "Earthquake" | disaster_type == "Drought") |> 
  filter(year >= 2000 & year <= 2019) |> 
  select(iso, year, disaster_type)

disaster$event <- 1

disaster <- left_join( wbdata |> select(iso, year), disaster, by = c("iso", "year"))

disaster <- disaster |> mutate(
  event = ifelse(is.na(disaster_type), 0, event),
  disaster_type = ifelse(is.na(disaster_type), "Earthquake", disaster_type)) ## coding no event as 0 after pivot-wider


## create dummy variable of drought and earthquake per country per year
disaster <- disaster |> 
  distinct(iso, year, disaster_type, .keep_all = T) |> 
  pivot_wider(
  id_cols = c("iso", "year"), 
  names_from = disaster_type, 
  values_from = event, 
  values_fill = 0
)



#prepare disaster data 
conflict <- read.csv("./data/raw/conflict.csv", header = TRUE)

conflict <- conflict |> 
  group_by(iso, year) |> 
  summarise(death_sum = sum(best, na.rm = T)) |> 
  mutate(conflict = ifelse(death_sum >= 25, 1, 0),  #binary variable indicating
# the presence of conflict for each country–year observation (0 = no, <25 battle-related
# deaths; 1 = yes, �25 battle-related deaths)
  year = year + 1) ## lag 1 year, eg year 1999 predicts deaths in year 2000  


# load covariates 
covariates <- read.csv("./data/raw/covariates.csv")

# merge all data 
data <- list(wbdata,covariates, disaster, conflict)

#merge all data frames in list
data |> reduce(full_join, by = c('iso', 'year')) -> data
## retains missing conflicts as NA 

analysis_data <- data 

### Processing variable format 
regions <- read.table("./data/raw/regions.txt", header = TRUE)

analysis_data <- left_join(analysis_data, regions |> select(country_code, region), by = c("iso" = "country_code"))

continuous_variables <- intersect(
  c(
    "matmor", "infmor", "neomor", "un5mor", "gdp_1000", "pop_dens",
    "urban", "age_dep", "male_edu", "temp", "rainfall_1000", "death_sum"
  ),
  names(analysis_data)
)
binary_variables <- intersect(
  c("oecd", "Drought", "Earthquake", "conflict"),
  names(analysis_data)
)

analysis_data <- analysis_data |>
  mutate(
    iso = as.character(iso),
    year = as.integer(year),
    across(all_of(continuous_variables), as.numeric),
    across(all_of(binary_variables), ~ factor(.x, levels = c(0, 1)))
  )

make_death_quintile <- function(x) {
  result <- rep(NA_character_, length(x))
  result[!is.na(x) & x == 0] <- "None (0 deaths)"

  positive_deaths <- !is.na(x) & x > 0
  result[positive_deaths] <- paste0(
    "Positive deaths: quintile ",
    dplyr::ntile(x[positive_deaths], n = 5)
  )

  factor(
    result,
    levels = c(
      "None (0 deaths)",
      paste0("Positive deaths: quintile ", 1:5)
    ),
    ordered = TRUE
  )
}

analysis_data <- analysis_data |>
  mutate(
    conflict_intensity = factor(
      case_when(
        is.na(death_sum) ~ NA_character_,
        death_sum < 25 ~ "No conflict (<25 deaths)",
        death_sum < 1000 ~ "Minor conflict (25-999 deaths)",
        death_sum >= 1000 ~ "War (>=1,000 deaths)"
      ),
      levels = c(
        "No conflict (<25 deaths)",
        "Minor conflict (25-999 deaths)",
        "War (>=1,000 deaths)"
      ),
      ordered = TRUE
    ),
    death_sum_quintile = make_death_quintile(death_sum)
  )

write.csv(analysis_data, "./data/processed/data.csv", row.names = FALSE, na = "")



 