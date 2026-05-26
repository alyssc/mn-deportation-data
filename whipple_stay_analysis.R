setwd("/Users/alyssachen/Desktop/Projects/mn-reformer/ice_activity/deportation-data-proj")

library(tidyverse)
library(lubridate)
library(readxl)

## WHIPPLE DETENTIONS

# STINTS (separated into different detention center stays)
# Using parquet because file is large
library(nanoparquet)
library(duckdb)
library(duckplyr)
library(DBI)

nanoparquet::read_parquet_info("data/detention-stints-latest.parquet")
nanoparquet::parquet_column_types("data/detention-stints-latest.parquet")
con <- dbConnect(duckdb())

dbExecute(con,
          "CREATE VIEW whipple_stints AS
           SELECT * FROM PARQUET_SCAN('data/detention-stints-latest.parquet')
           WHERE detention_facility_code = 'SPMHOLD';")

whipple_stints <- tbl(con, "whipple_stints") |>
  collect()

colnames(whipple_stints)
whipple_surge_stints <- whipple_stints %>% 
  filter(likely_duplicate == FALSE,
         as.Date(stay_book_in_date_time) > as.Date("2025-12-01")) %>% # filter for STAYS that began after Dec. 1
  mutate(stint_duration_hr = as.numeric(difftime(book_out_date_time,book_in_date_time,units = "hours")) )%>%
  relocate(stint_duration_hr)

summary(as.numeric(whipple_surge_stints$stint_duration_hr))
view(whipple_surge_stints %>% arrange(desc(stint_duration_hr)))

whipple_surge_stints %>% 
  summarize(count=n_distinct(unique_identifier)) # 4317 unique IDs stayed in Whipple


# STAYS (groups multiple detention centers for a person's detention)
whipple_stays <- read_xlsx("data/detention-stays-whipple.xlsx",sheet = 2)
head(whipple_stays)
colnames(whipple_stays)
whipple_stays <- whipple_stays %>% 
  mutate(stay_book_in_date = as.Date(stay_book_in_date_time)) %>%
  filter(stay_book_in_date > as.Date("2025-12-01")) 

whipple_stays %>% 
  summarize(n_detained <- n_distinct(unique_identifier))

# overlapping unique IDs in Whipple stays and MN arrests
arrests <- read_xlsx("data/arrests_filtered_20260331_144827.xlsx",sheet = 2)
arrests <- arrests %>%
  filter(duplicate_likely == FALSE) # de-duplicate
surge_arrests <- arrests %>% 
  filter(
    apprehension_state == "MINNESOTA",
    apprehension_date > as.Date("2025-12-01"))

whipple_stays_ids <- unique(whipple_stays$unique_identifier)
surge_arrests_ids <- unique(surge_arrests$unique_identifier)
length(whipple_stays_ids)
length(surge_arrests_ids)
whipple_but_not_arrest <- setdiff(whipple_stays_ids, surge_arrests_ids) # individuals who were held at Whipple but not in MN arrest data

