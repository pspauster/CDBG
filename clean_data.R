library(tidycensus)
library(tidyverse)


cdbg <- read_csv("Expenditure-by-PGY-Matrix-24-FI-HQ-02125_11-1-24_Revised.csv") %>% 
  janitor::clean_names()

abbs_fips<- fips_codes %>% group_by(state) %>% summarize(state_fips = first(state_code))

cdbg_classified <- cdbg %>% 
  mutate(geo_level = case_when(grantee %in% toupper(state.name) ~ "state",
                               str_detect(grantee, "COUNTY") ~ "county", #there may be some misaligned here
                               T~"place"
                               ),
         grantee_name = tolower(trimws(gsub("\\s(COUNTY|TOWNSHIP).*", "", grantee)))
         ) %>% 
  left_join(abbs_fips, by = "state")

vars <- load_variables(2023, dataset = "acs5")


city_stats <- get_acs(
  year = 2023,
  geography = "place",
  variables = c("total_population" = "B01003_001"),
  output = "tidy",
) %>% 
  mutate(state_fips = str_sub(GEOID, 1, 2),
         place_name = tolower(trimws(gsub("\\s(CDP|town|city|City|Town|Borough|metro township).*", "", NAME))))

state_stats <- get_acs(
  year = 2023,
  geography = "state",
  variables = c("total_population" = "B01003_001"),
  output = "tidy",
) %>% 
  mutate(state_name = tolower(NAME))

county_stats <- get_acs(
  year = 2023,
  geography = "county",
  variables = c("total_population" = "B01003_001"),
  output = "tidy",
) %>% 
  mutate(state_fips = str_sub(GEOID, 1, 2),
         county_name = tolower(trimws(gsub("\\s(County|Borough|Census Area).*", "", NAME))))

state_merged <- inner_join(cdbg_classified %>% filter(geo_level=="state"), state_stats, by = c("grantee_name"="state_name"))
#check state merge
stopifnot(nrow(anti_join(cdbg_classified %>% filter(geo_level=="state"), state_stats, by = c("grantee_name"="state_name")))==0)

county_merged <- inner_join(cdbg_classified %>% filter(geo_level=="county"), county_stats, by = c("state_fips"="state_fips","grantee_name"="county_name"))
anti_join(cdbg_classified %>% filter(geo_level=="county"), county_stats, by = c("state_fips"="state_fips","grantee_name"="county_name")) %>% count(state, grantee) 

city_merged <- inner_join(cdbg_classified %>% filter(geo_level=="place"), city_stats, by = c("state_fips"="state_fips","grantee_name"="place_name"))
