setwd("/Users/alyssachen/Desktop/Projects/mn-reformer/ice_activity/deportation-data-proj")

library(tidyverse)
library(lubridate)
library(readxl)

## ARRESTS

parameters <- read_xlsx("data/arrests_filtered_20260331_144827.xlsx",sheet = 1)
arrests <- read_xlsx("data/arrests_filtered_20260331_144827.xlsx",sheet = 2)

arrests <- arrests %>%
  filter(duplicate_likely == FALSE) # de-duplicate

head(arrests)
daily_arrests <- arrests %>% 
  group_by(apprehension_date, apprehension_state) %>%
  summarize(arrests = n())

rolling_arrests <- daily_arrests %>% 
  group_by(apprehension_state) %>% 
  mutate(rolling_avg=rollmean(arrests,7, fill=0))

daily_arrests_pivot <- daily_arrests %>% 
  pivot_wider(names_from = apprehension_state, values_from = arrests) %>%
  mutate(apprehension_date = as.Date(apprehension_date)) %>%
  filter(apprehension_date > as.Date("2025-08-01"))

rolling_arrests <- rolling_arrests %>% 
  pivot_wider(names_from = apprehension_state, values_from = rolling_avg) %>%
  mutate(apprehension_date = as.Date(apprehension_date)) %>%
  filter(apprehension_date > as.Date("2025-08-01"))

write.csv(rolling_arrests, "output/rolling_avg_arrests.csv")
write.csv(daily_arrests_pivot, "output/daily_arrests.csv")

surge_arrests <- arrests %>% 
  filter(
    apprehension_state == "MINNESOTA",
    apprehension_date > as.Date("2025-12-01"))

metro_surge_by_country <- surge_arrests %>% 
  group_by(citizenship_country) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(citizenship_country = tools::toTitleCase(tolower(citizenship_country)))

metro_surge_by_country_top_10 <- metro_surge_by_country %>%
  top_n(10)

write.csv(metro_surge_by_country,"deportation-data-proj/output/metro_surge_by_country.csv")
write.csv(metro_surge_by_country_top_10,"deportation-data-proj/output/metro_surge_by_country_top_10.csv")

metro_surge_by_case_status <- surge_arrests %>% 
  group_by(case_status) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) 

metro_surge_by_criminality <- surge_arrests %>%
  group_by(apprehension_criminality) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) 

# number of surge arrests
surge_arrests %>% 
  summarize(count = n())

# arrests post surge wind-down
surge_arrests %>%
  filter(apprehension_date > as.Date("2026-02-17")) %>%
  group_by(apprehension_date) %>%
  summarize(arrests = n()) %>% 
  dplyr::select(arrests)


