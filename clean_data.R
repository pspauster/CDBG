library(tidycensus)
library(tidyverse)


cdbg <- read_csv("Expenditure-by-PGY-Matrix-24-FI-HQ-02125_11-1-24_Revised.csv") %>% 
  janitor::clean_names()

cdbg_classified <- cdbg %>% 
  mutate(geo_level = case_when(grantee %in% toupper(state.name) ~ "state",
                               str_detect(grantee, "COUNTY") ~ "county", #there may be some misaligned here
                               T~"place"
                               ))



vars <- load_variables(2023, dataset = "acs5")

city_stats <- get_acs(
  geography = "place",
  variables = c("total_population" = "B01003_001"),
  output = "tidy",
)

state_stats <- get_acs(
  geography = "state",
  variables = c("total_population" = "B01003_001"),
  output = "tidy",
)

county_stats <- get_acs(
  geography = "county",
  variables = c("total_population" = "B01003_001"),
  output = "tidy",
)

