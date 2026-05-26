setwd("/Users/alyssachen/Desktop/Projects/mn-reformer/ice_activity/deportation-data-proj")

library(tidyverse)
library(lubridate)
library(readxl)

## DETENTION CENTER POPULATIONS
detention_ctrs <- read_xlsx("data/detention-facility-daily-population_filtered_20260331_191604.xlsx",sheet = 2)

detention_ctrs <- detention_ctrs %>% 
  mutate(month_year = format(as.Date(date), "%Y-%m"),
         date = as.Date(date)) 

# daily population
detention_pop <- detention_ctrs %>% 
  dplyr::select(date,n_detained,detention_facility_code) %>%
  pivot_wider(names_from ="detention_facility_code", values_from = "n_detained")

# rolling avg
detention_pop_rolling <- detention_ctrs %>% 
  group_by(detention_facility_code) %>%
  mutate(rolling_avg=rollmean(n_detained,7, fill=0)) %>% 
  dplyr::select(date,rolling_avg,detention_facility_code) %>%
  pivot_wider(names_from ="detention_facility_code", values_from = "rolling_avg")

write.csv(detention_pop,"output/detention_pop.csv")
write.csv(detention_pop_rolling,"output/detention_pop_rolling.csv")

# Why is there a single Ramsey county Dec 8 2025 detention? 
detention_pop %>% 
  dplyr::select(date,RAADCMN) %>%
  filter(RAADCMN > 0)


