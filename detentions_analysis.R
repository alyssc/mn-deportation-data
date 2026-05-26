setwd("/Users/alyssachen/Desktop/Projects/mn-reformer/ice_activity/deportation-data-proj")

library(tidyverse)
library(lubridate)
library(readxl)
# parquet
library(nanoparquet)
library(duckdb)
library(duckplyr)
library(DBI)

## DETENTION STAYS
nanoparquet::read_parquet_info("data/detention-stays-latest.parquet")
nanoparquet::parquet_schema("data/detention-stays-latest.parquet")$name
con <- dbConnect(duckdb())

dbExecute(con,
          "DROP VIEW surge_stays;
           CREATE VIEW surge_stays AS
           SELECT * FROM PARQUET_SCAN('data/detention-stays-latest.parquet')
           WHERE stay_book_in_date_time > '2025-12-01 00:00:00' AND
           (detention_facility_codes_all LIKE '%SPMHOLD%' OR
           book_in_aor = 'St. Paul Area of Responsibility');")

# stays going through SPMHOLD (Whipple) or booked in St. Paul AOR
surge_stays <- tbl(con, "surge_stays") |>
  collect()

# Where did people get deported to? 
surge_stays %>% group_by(departure_country) %>% summarize(count=n()) %>% arrange(desc(count))
surge_stays %>% group_by(stay_release_reason) %>% summarize(count=n()) %>% arrange(desc(count))
surge_stays %>% group_by(detention_facility_last) %>% summarize(count=n()) %>% arrange(desc(count))

# ARRESTS
arrests <- read_xlsx("data/arrests_filtered_20260331_144827.xlsx",sheet = 2)
arrests <- arrests %>%
  filter(duplicate_likely == FALSE) # de-duplicate
surge_arrests <- arrests %>% 
  filter(
    apprehension_state == "MINNESOTA",
    apprehension_date > as.Date("2025-12-01"))

## MERGED METRO SURGE ARRESTS AND DETENTION STAYS TABLES
dim(surge_stays)
dim(surge_arrests)
surge_arrest_stays <- merge(surge_stays, surge_arrests, by = "unique_identifier",all=FALSE)  # 3740 in both
length(unique(surge_arrest_stays$unique_identifier)) # 3571 unique identifiers? 

surge_stays %>% group_by(departure_country) %>% summarize(count=n()) %>% arrange(desc(count))
surge_stays %>% group_by(case_category) %>% summarize(count=n()) %>% arrange(desc(count))

## Metro surge stays data cleaning / simplifying
dat <- surge_arrest_stays %>% 
  dplyr::select(unique_identifier, n_stints, stay_release_reason, 
                detention_facility_codes_all,
                stay_book_in_date_time,
                stay_book_out_date_time,
                departure_country.x) %>%
  mutate(detention_facility_codes_all = paste(detention_facility_codes_all,"; ",sep='') )

# Step 1: Deduplicate stays based on booking time
dat_dedup <- dat %>% group_by(unique_identifier) %>% 
  distinct(stay_book_in_date_time, .keep_all=T) %>% ungroup()

# Step 2: 2 OPTIONS BELOW
# 2a: collapse multiple stays into one row
dat_collapsed <- dat_dedup %>% group_by(unique_identifier) %>%
  arrange(stay_book_in_date_time) %>% 
  mutate(detention_facility_codes_all = paste0(detention_facility_codes_all, collapse = "") ) %>% # string together detention facilities
  distinct(unique_identifier, .keep_all=T) %>% ungroup()

# 2b: keep only the most recent stay
dat_mostrecent <- dat_dedup %>% group_by(unique_identifier) %>%
  arrange(stay_book_in_date_time) %>% 
  slice_tail(n=1) %>% ungroup()
  
# Look at this test example to understand the difference between 2a and 2b
test <- dat_dedup %>% filter(unique_identifier=='a246e14d8369942af002d3c0b923e491f9e9fbef')
test_processed1 <- test %>% group_by(unique_identifier) %>% 
  distinct(stay_book_in_date_time, .keep_all=T) %>% ungroup()
test_processed2a <- test_processed1 %>% group_by(unique_identifier) %>%
  arrange(stay_book_in_date_time) %>% 
  mutate(detention_facility_codes_all = paste0(detention_facility_codes_all, collapse = "") ) %>% # string together detention facilities
  distinct(unique_identifier, .keep_all=T)
test_processed2b <- test_processed1 %>% group_by(unique_identifier) %>%
  arrange(stay_book_in_date_time) %>% 
  slice_tail(n=1)

view(test_processed1)
view(test_processed2a)
view(test_processed2b)
view(dat_mostrecent %>% filter(unique_identifier=='a246e14d8369942af002d3c0b923e491f9e9fbef'))

## ALLUVIAL GRAPH
# I'm going to use the "most recent" detention stay per person; 
# there is a difference of like 10 rows so it doesn't matter much for aggregate analysis. 
# dat_mostrecent = most recent detention stay for individuals arrested in MN after Dec. 1, 2025
view(dat_mostrecent)

alluvialize_row <- function(r, n_steps=10){
  facilities <- strsplit(r$detention_facility_codes_all, "; ")[[1]]
  departure_country <- r$departure_country.x
  no_book_out <- is.na(r$stay_book_out_date_time)
  alluvialized_rows <- data.frame(source=character(),
                                  dest=character(),
                                  step_from=integer(),
                                  step_to=integer())
  for(i in 1:r$n_stints){
    if(i==r$n_stints){
      if(!is.na(departure_country)){
        alluvialized_rows <- rbind(alluvialized_rows,data.frame(source=facilities[i],dest="Deported or left country",step_from=i,step_to=i+1))
        alluvialized_rows <- rbind(alluvialized_rows,data.frame(source="Deported or left country",dest=departure_country,step_from=i+1,step_to=i+2))
      } else if(no_book_out){
        alluvialized_rows <- rbind(alluvialized_rows,data.frame(source=facilities[i],dest="Detained as of early March",step_from=i,step_to=i+1))
      } else{
        alluvialized_rows <- rbind(alluvialized_rows,data.frame(source=facilities[i],dest="No longer detained",step_from=i,step_to=i+1))
        alluvialized_rows <- rbind(alluvialized_rows,data.frame(source="No longer detained",dest=r$stay_release_reason,step_from=i+1,step_to=i+2))
      }
    }
    else{
      alluvialized_rows <- rbind(alluvialized_rows,data.frame(source=facilities[i],dest=facilities[i+1],step_from=i,step_to=i+1))
    }
  }
  return(alluvialized_rows)
}

alluvialize_dat <- function(data){
  n_steps=max(data$n_stints)+1
  alluvialized_dat <- data.frame(source=character(),
                                 dest=character(),
                                 step_from=integer(),
                                 step_to=integer())
  
  for(j in 1:nrow(data)){
    row <- data[j,]
    alluvialized_dat <- rbind(alluvialized_dat, alluvialize_row(row,n_steps))
  }
  alluvialized_dat_counts <- alluvialized_dat %>%
    group_by(source,dest,step_from,step_to) %>%
    summarize(count=n()) 
  
  return(alluvialized_dat_counts)
}

# from prev version: write_csv(alluvialized_dat_counts,"output/all_alluvialized_dat.csv")

## ALLUVIAL GRAPH, BY N_STINTS
sum(is.na(dat_mostrecent$n_stints)) # n_stints is a fully populated field
quantile(dat_mostrecent$n_stints,probs=.9) # 90% of stays involved 5 or fewer stints (i.e. 4 or fewer transfers)
table(dat_mostrecent$n_stints)

# 1 stint
100*sum(dat_mostrecent$n_stints==1)/length(dat_mostrecent$n_stints) # 2.9%
dat_mostrecent %>% filter(n_stints==1) %>% group_by(detention_facility_codes_all) %>% summarize(count=n()) %>% arrange(desc(count))
dat_mostrecent %>% filter(n_stints==1) %>% group_by(stay_release_reason) %>% summarize(count=n()) %>% arrange(desc(count))


# 2 stints
100*sum(dat_mostrecent$n_stints==2)/length(dat_mostrecent$n_stints) # 29.6%

# 3 stints
100*sum(dat_mostrecent$n_stints==3)/length(dat_mostrecent$n_stints) # 28.8%

# 4 stints
100*sum(dat_mostrecent$n_stints==4)/length(dat_mostrecent$n_stints) # 18.17%

# 5 stints
100*sum(dat_mostrecent$n_stints==5)/length(dat_mostrecent$n_stints) # 12.48%



# How long were overall detention stays? 






