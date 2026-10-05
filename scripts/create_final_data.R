
# read in all other datasets 

infmor0 <- read.csv("../data/raw/infant_mortality.csv", header = TRUE)
matmor0 <- read.csv("../data/raw/maternal_mortality.csv", header = TRUE)
neomor0 <- read.csv("../data/raw/neonatal_mortality.csv", header = TRUE)
un5mor0 <- read.csv("../data/raw/under5_mortality.csv", header = TRUE)



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
wblist |> reduce(full_join, by = c('iso', 'year')) -> wbdata ## apply full join to each item in list iteratively 



#prepare disaster data 
disaster0 <- read.csv("../data/raw/disaster.csv", header = TRUE)

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
  disaster_type = ifelse(is.na(disaster_type), "Earthquake", disaster_type))


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
conflict <- read.csv("../data/raw/conflict.csv", header = TRUE)

conflict <- conflict |> 
  group_by(iso, year) |> 
  summarise(death_sum = sum(best, na.rm = T)) |> 
  mutate(conflict = ifelse(death_sum >= 25, 1, 0),  #binary variable indicating
# the presence of conflict for each country–year observation (0 = no, <25 battle-related
# deaths; 1 = yes, �25 battle-related deaths)
  year = year + 1) ## lag 1 year, eg year 1999 predicts deaths in year 2000  

# merge all data 
data <- list(wbdata, disaster, conflict)

#merge all data frames in list
data |> reduce(full_join, by = c('iso', 'year')) -> data

write.csv(data, "../data/processed/data.csv", row.names = FALSE, na = "")

head(data)

 