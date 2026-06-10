setwd("/Users/alyssachen/Desktop/Projects/mn-reformer/ice_activity/deportation-data-proj")

library(tidyverse)
library(lubridate)
library(readxl)
# parquet
library(nanoparquet)
library(duckdb)
library(duckplyr)
library(DBI)

## Facility names
facility_crosswalk <- read_xlsx("data/detention-facilities_filtered_20260526_205455.xlsx",sheet=2)
head(facility_crosswalk)

## DETENTION STAYS
nanoparquet::read_parquet_info("data/detention-stays-latest.parquet")
nanoparquet::parquet_schema("data/detention-stays-latest.parquet")$name
nanoparquet::read_parquet_info("data/detention-stints-latest.parquet")
nanoparquet::read_parquet_schema("data/detention-stints-latest.parquet")
con <- dbConnect(duckdb())

dbExecute(con,
          "CREATE VIEW surge_stays AS
           SELECT * FROM PARQUET_SCAN('data/detention-stays-latest.parquet')
           WHERE stay_book_in_date_time > '2025-12-01 00:00:00' AND
           (detention_facility_codes_all LIKE '%SPMHOLD%' OR
           book_in_aor = 'St. Paul Area of Responsibility');")

# stays going through SPMHOLD (Whipple) or booked in St. Paul AOR
surge_stays <- tbl(con, "surge_stays") |>
  collect()

## DETENTION STINTS
dbExecute(con,
          "CREATE VIEW stints AS
           SELECT * FROM PARQUET_SCAN('data/detention-stints-latest.parquet')
           WHERE stay_book_in_date_time > '2025-12-01 00:00:00' 
          ;")
dbExecute(con,
          "CREATE VIEW surge_stints AS
           SELECT * FROM stints
           JOIN
           (SELECT DISTINCT unique_identifier FROM surge_stays) stays 
           ON stints.unique_identifier = stays.unique_identifier
          ;")

surge_stints <- tbl(con, "surge_stints") |>
  collect()

# Check join involves same unique IDs
length(unique(surge_stays$unique_identifier))
length(unique(surge_stints$unique_identifier))

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

## MERGED METRO SURGE ARRESTS (MN Dec and after) AND DETENTION *STAYS* TABLES
dim(surge_stays)
dim(surge_arrests)
surge_arrest_stays <- merge(surge_stays, surge_arrests, by = "unique_identifier",all=FALSE)  # 3740 in the overlap of arrest and stay data
length(unique(surge_arrest_stays$unique_identifier)) # 3571 unique identifiers

surge_stays %>% group_by(departure_country) %>% summarize(count=n()) %>% arrange(desc(count))
surge_stays %>% group_by(case_category) %>% summarize(count=n()) %>% arrange(desc(count))

## MERGED METRO SURGE ARRESTS (MN Dec and after) AND DETENTION *STINTS* TABLES
dim(surge_stints)
dim(surge_arrests)
surge_arrest_stints <- merge(surge_stints, surge_arrests, by = "unique_identifier",all=FALSE)  
length(unique(surge_arrest_stints$unique_identifier)) # 3571 unique identifiers

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
# there is a difference of 10 rows so it doesn't matter much for aggregate analysis. 
# dat_mostrecent = most recent detention stay for individuals arrested in MN after Dec. 1, 2025
view(dat_mostrecent)

alluvialize_row <- function(r, n_steps=10){
  facilities <- strsplit(r$detention_facility_codes_all, "; ")[[1]]
  
  # use crosswalk to replace acronyms with names
  for(f in facilities){
    
  }
  
  departure_country <- r$departure_country.x
  left_country <- !is.na(departure_country) | r$stay_release_reason %in% c("Removed","Voluntary departure")
  
  no_book_out <- is.na(r$stay_book_out_date_time)
  alluvialized_rows <- data.frame(source=character(),
                                  dest=character(),
                                  step_from=integer(),
                                  step_to=integer())
  for(i in 1:r$n_stints){
    if(i==r$n_stints){
      if(left_country){
        alluvialized_rows <- rbind(alluvialized_rows,data.frame(source=facilities[i],dest="Deported or left country",step_from=i,step_to=i+1))
        if(!is.na(departure_country)){alluvialized_rows <- rbind(alluvialized_rows,data.frame(source="Deported or left country",dest=departure_country,step_from=i+1,step_to=i+2))}
      } else if(no_book_out){
        alluvialized_rows <- rbind(alluvialized_rows,data.frame(source=facilities[i],dest="Detained as of early March",step_from=i,step_to=i+1))
      } else{
        alluvialized_rows <- rbind(alluvialized_rows,data.frame(source=facilities[i],dest="No longer detained",step_from=i,step_to=i+1))
        # alluvialized_rows <- rbind(alluvialized_rows,data.frame(source="No longer detained",dest=r$stay_release_reason,step_from=i+1,step_to=i+2))
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

write_csv(alluvialize_dat(dat_mostrecent%>% filter(n_stints==1)),"output/alluvial1.csv")

# 2 stints
100*sum(dat_mostrecent$n_stints==2)/length(dat_mostrecent$n_stints) # 29.6%
write_csv(alluvialize_dat(dat_mostrecent%>% filter(n_stints==2)),"output/alluvial2.csv")

top4<-dat_mostrecent %>% filter(n_stints==2) %>%
  group_by(detention_facility_codes_all) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  head(4)

nstint2top4 <- dat_mostrecent%>%
  filter(detention_facility_codes_all %in% top4$detention_facility_codes_all)

write_csv(alluvialize_dat(nstint2top4),"output/alluvial2simplified.csv")

# 3 stints
100*sum(dat_mostrecent$n_stints==3)/length(dat_mostrecent$n_stints) # 28.8%
write_csv(alluvialize_dat(dat_mostrecent%>% filter(n_stints==3)),"output/alluvial3.csv")

top9 <- dat_mostrecent %>% filter(n_stints==3) %>%
  group_by(detention_facility_codes_all) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  head(9) %>% select(detention_facility_codes_all)

nstint3top9 <- dat_mostrecent%>%
  filter(detention_facility_codes_all %in% top9$detention_facility_codes_all)

write_csv(alluvialize_dat(nstint3top9),"output/alluvial3simplified.csv")

dat_mostrecent %>%
  group_by(stay_release_reason) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

# 4 stints
100*sum(dat_mostrecent$n_stints==4)/length(dat_mostrecent$n_stints) # 18.17%
write_csv(alluvialize_dat(dat_mostrecent%>% filter(n_stints==4)),"output/alluvial4.csv")

# 5 stints
100*sum(dat_mostrecent$n_stints==5)/length(dat_mostrecent$n_stints) # 12.48%
write_csv(alluvialize_dat(dat_mostrecent%>% filter(n_stints==5)),"output/alluvial5.csv")


# How long were overall detention stays? 
stay_lengths <- dat_mostrecent %>%
  filter(!is.na(stay_book_out_date_time)) %>%
  mutate(length_of_stay =interval(as_datetime(stay_book_in_date_time), as_datetime(stay_book_out_date_time)) %/% days(1) ) %>%
  select(length_of_stay) %>% group_by(length_of_stay) %>% summarize(count=n()) %>% arrange(desc(count))
write_csv(stay_lengths,"output/stay_lengths.csv")

## METRO SURGE DETENTION STINTS
surge_arrest_stints <- surge_arrest_stints %>% filter(likely_duplicate == FALSE) %>%
  mutate(stint_duration_hr = as.numeric(difftime(book_out_date_time,book_in_date_time,units = "hours")) )%>%
  mutate(stint_duration_day=stint_duration_hr/24)%>%
  relocate(stint_duration_hr,stint_duration_day)

# How long were stays in specific locations? 
view(head(surge_arrest_stints))
stint_lengths <- surge_arrest_stints %>% 
  filter(!is.na(book_out_date_time)) %>% 
  group_by(detention_facility_code) %>% 
  summarise(avg_stint_days = mean(stint_duration_day),
            median_stint_days = median(stint_duration_day),
            max_stint_days = max(stint_duration_day),
            n_stints = n(),
            n_people = n_distinct(unique_identifier)) %>%
  merge(facility_crosswalk, by="detention_facility_code") %>%
  select(name, avg_stint_days,median_stint_days,max_stint_days,n_stints,n_people,state) %>%
  arrange(desc(n_people)) 

write_csv(stint_lengths,"output/stint_lengths.csv")

elpaso_lengths <- surge_arrest_stints %>% 
  filter(!is.na(book_out_date_time),
         detention_facility=="ERO EL PASO CAMP EAST MONTANA") %>% 
  select(stint_duration_day)
write_csv(elpaso_lengths,"output/elpaso_lengths.csv")

whipple_lengths <- surge_arrest_stints %>% 
  filter(!is.na(book_out_date_time),
         detention_facility=="BISHOP HENRY WHIPPLE FED BLDG") %>% 
  select(stint_duration_day)
write_csv(whipple_lengths,"output/whipple_lengths.csv")
  

# Map attempt 1: animated points
colnames(surge_arrest_stints)

map <- surge_arrest_stints %>% 
  mutate(stint_start = as.Date(book_in_date_time),stint_end = as.Date(book_out_date_time), 
         stay_end = as.Date(stay_book_out_date_time)) %>% 
  merge(facility_crosswalk, by="detention_facility_code",suffixes = c(".x",".facility")) %>%
  select(stint_start, stint_end, stay_end, name, longitude, latitude, address, state.facility, unique_identifier) 
  

write_csv(map%>%filter(unique_identifier=="e763bad2cd6324e52af1ebb05f7af67d026bf92d"),"output/testmap.csv")

# Map attempt 2: heat maps
head(map)
heatmap_week <- map %>% group_by(name,latitude, longitude, state.facility,address) %>% 
  replace_na(list(stint_end = as.Date("2026-03-10"))) %>%
  summarise(
    'Week of Dec 01, 2025' = sum(as.Date('2025-12-01') <= stint_end & as.Date('2025-12-07') >= stint_start),
    'Week of Dec 08, 2025' = sum(as.Date('2025-12-08') <= stint_end & as.Date('2025-12-14') >= stint_start),
    'Week of Dec 15, 2025' = sum(as.Date('2025-12-15') <= stint_end & as.Date('2025-12-21') >= stint_start),
    'Week of Dec 22, 2025' = sum(as.Date('2025-12-22') <= stint_end & as.Date('2025-12-28') >= stint_start),
    'Week of Dec 29, 2025' = sum(as.Date('2025-12-29') <= stint_end & as.Date('2026-01-04') >= stint_start),
    'Week of Jan 05, 2026' = sum(as.Date('2026-01-05') <= stint_end & as.Date('2026-01-11') >= stint_start),
    'Week of Jan 12, 2026' = sum(as.Date('2026-01-12') <= stint_end & as.Date('2026-01-18') >= stint_start),
    'Week of Jan 19, 2026' = sum(as.Date('2026-01-19') <= stint_end & as.Date('2026-01-25') >= stint_start),
    'Week of Jan 26, 2026' = sum(as.Date('2026-01-26') <= stint_end & as.Date('2026-02-01') >= stint_start),
    'Week of Feb 02, 2026' = sum(as.Date('2026-02-02') <= stint_end & as.Date('2026-02-08') >= stint_start),
    'Week of Feb 09, 2026' = sum(as.Date('2026-02-09') <= stint_end & as.Date('2026-02-15') >= stint_start),
    'Week of Feb 16, 2026' = sum(as.Date('2026-02-16') <= stint_end & as.Date('2026-02-22') >= stint_start),
    'Week of Feb 23, 2026' = sum(as.Date('2026-02-23') <= stint_end & as.Date('2026-03-01') >= stint_start),
    'Week of Mar 02, 2026' = sum(as.Date('2026-03-02') <= stint_end & as.Date('2026-03-08') >= stint_start)
    ) %>%
  ungroup()

write_csv(heatmap_week,"output/heatmap_week.csv")

# DAILY HEATMAP
heatmap_day <- map %>% group_by(name,latitude, longitude, state.facility,address) %>% 
  replace_na(list(stint_end = as.Date("2026-03-10"))) %>%
  summarise(
    'Dec 01, 2025' = sum(as.Date('2025-12-01') <= stint_end & as.Date('2025-12-01') >= stint_start),
    'Dec 02, 2025' = sum(as.Date('2025-12-02') <= stint_end & as.Date('2025-12-02') >= stint_start),
    'Dec 03, 2025' = sum(as.Date('2025-12-03') <= stint_end & as.Date('2025-12-03') >= stint_start),
    'Dec 04, 2025' = sum(as.Date('2025-12-04') <= stint_end & as.Date('2025-12-04') >= stint_start),
    'Dec 05, 2025' = sum(as.Date('2025-12-05') <= stint_end & as.Date('2025-12-05') >= stint_start),
    'Dec 06, 2025' = sum(as.Date('2025-12-06') <= stint_end & as.Date('2025-12-06') >= stint_start),
    'Dec 07, 2025' = sum(as.Date('2025-12-07') <= stint_end & as.Date('2025-12-07') >= stint_start),
    'Dec 08, 2025' = sum(as.Date('2025-12-08') <= stint_end & as.Date('2025-12-08') >= stint_start),
    'Dec 09, 2025' = sum(as.Date('2025-12-09') <= stint_end & as.Date('2025-12-09') >= stint_start),
    'Dec 10, 2025' = sum(as.Date('2025-12-10') <= stint_end & as.Date('2025-12-10') >= stint_start),
    'Dec 11, 2025' = sum(as.Date('2025-12-11') <= stint_end & as.Date('2025-12-11') >= stint_start),
    'Dec 12, 2025' = sum(as.Date('2025-12-12') <= stint_end & as.Date('2025-12-12') >= stint_start),
    'Dec 13, 2025' = sum(as.Date('2025-12-13') <= stint_end & as.Date('2025-12-13') >= stint_start),
    'Dec 14, 2025' = sum(as.Date('2025-12-14') <= stint_end & as.Date('2025-12-14') >= stint_start),
    'Dec 15, 2025' = sum(as.Date('2025-12-15') <= stint_end & as.Date('2025-12-15') >= stint_start),
    'Dec 16, 2025' = sum(as.Date('2025-12-16') <= stint_end & as.Date('2025-12-16') >= stint_start),
    'Dec 17, 2025' = sum(as.Date('2025-12-17') <= stint_end & as.Date('2025-12-17') >= stint_start),
    'Dec 18, 2025' = sum(as.Date('2025-12-18') <= stint_end & as.Date('2025-12-18') >= stint_start),
    'Dec 19, 2025' = sum(as.Date('2025-12-19') <= stint_end & as.Date('2025-12-19') >= stint_start),
    'Dec 20, 2025' = sum(as.Date('2025-12-20') <= stint_end & as.Date('2025-12-20') >= stint_start),
    'Dec 21, 2025' = sum(as.Date('2025-12-21') <= stint_end & as.Date('2025-12-21') >= stint_start),
    'Dec 22, 2025' = sum(as.Date('2025-12-22') <= stint_end & as.Date('2025-12-22') >= stint_start),
    'Dec 23, 2025' = sum(as.Date('2025-12-23') <= stint_end & as.Date('2025-12-23') >= stint_start),
    'Dec 24, 2025' = sum(as.Date('2025-12-24') <= stint_end & as.Date('2025-12-24') >= stint_start),
    'Dec 25, 2025' = sum(as.Date('2025-12-25') <= stint_end & as.Date('2025-12-25') >= stint_start),
    'Dec 26, 2025' = sum(as.Date('2025-12-26') <= stint_end & as.Date('2025-12-26') >= stint_start),
    'Dec 27, 2025' = sum(as.Date('2025-12-27') <= stint_end & as.Date('2025-12-27') >= stint_start),
    'Dec 28, 2025' = sum(as.Date('2025-12-28') <= stint_end & as.Date('2025-12-28') >= stint_start),
    'Dec 29, 2025' = sum(as.Date('2025-12-29') <= stint_end & as.Date('2025-12-29') >= stint_start),
    'Dec 30, 2025' = sum(as.Date('2025-12-30') <= stint_end & as.Date('2025-12-30') >= stint_start),
    'Dec 31, 2025' = sum(as.Date('2025-12-31') <= stint_end & as.Date('2025-12-31') >= stint_start),
    'Jan 01, 2026' = sum(as.Date('2026-01-01') <= stint_end & as.Date('2026-01-01') >= stint_start),
    'Jan 02, 2026' = sum(as.Date('2026-01-02') <= stint_end & as.Date('2026-01-02') >= stint_start),
    'Jan 03, 2026' = sum(as.Date('2026-01-03') <= stint_end & as.Date('2026-01-03') >= stint_start),
    'Jan 04, 2026' = sum(as.Date('2026-01-04') <= stint_end & as.Date('2026-01-04') >= stint_start),
    'Jan 05, 2026' = sum(as.Date('2026-01-05') <= stint_end & as.Date('2026-01-05') >= stint_start),
    'Jan 06, 2026' = sum(as.Date('2026-01-06') <= stint_end & as.Date('2026-01-06') >= stint_start),
    'Jan 07, 2026' = sum(as.Date('2026-01-07') <= stint_end & as.Date('2026-01-07') >= stint_start),
    'Jan 08, 2026' = sum(as.Date('2026-01-08') <= stint_end & as.Date('2026-01-08') >= stint_start),
    'Jan 09, 2026' = sum(as.Date('2026-01-09') <= stint_end & as.Date('2026-01-09') >= stint_start),
    'Jan 10, 2026' = sum(as.Date('2026-01-10') <= stint_end & as.Date('2026-01-10') >= stint_start),
    'Jan 11, 2026' = sum(as.Date('2026-01-11') <= stint_end & as.Date('2026-01-11') >= stint_start),
    'Jan 12, 2026' = sum(as.Date('2026-01-12') <= stint_end & as.Date('2026-01-12') >= stint_start),
    'Jan 13, 2026' = sum(as.Date('2026-01-13') <= stint_end & as.Date('2026-01-13') >= stint_start),
    'Jan 14, 2026' = sum(as.Date('2026-01-14') <= stint_end & as.Date('2026-01-14') >= stint_start),
    'Jan 15, 2026' = sum(as.Date('2026-01-15') <= stint_end & as.Date('2026-01-15') >= stint_start),
    'Jan 16, 2026' = sum(as.Date('2026-01-16') <= stint_end & as.Date('2026-01-16') >= stint_start),
    'Jan 17, 2026' = sum(as.Date('2026-01-17') <= stint_end & as.Date('2026-01-17') >= stint_start),
    'Jan 18, 2026' = sum(as.Date('2026-01-18') <= stint_end & as.Date('2026-01-18') >= stint_start),
    'Jan 19, 2026' = sum(as.Date('2026-01-19') <= stint_end & as.Date('2026-01-19') >= stint_start),
    'Jan 20, 2026' = sum(as.Date('2026-01-20') <= stint_end & as.Date('2026-01-20') >= stint_start),
    'Jan 21, 2026' = sum(as.Date('2026-01-21') <= stint_end & as.Date('2026-01-21') >= stint_start),
    'Jan 22, 2026' = sum(as.Date('2026-01-22') <= stint_end & as.Date('2026-01-22') >= stint_start),
    'Jan 23, 2026' = sum(as.Date('2026-01-23') <= stint_end & as.Date('2026-01-23') >= stint_start),
    'Jan 24, 2026' = sum(as.Date('2026-01-24') <= stint_end & as.Date('2026-01-24') >= stint_start),
    'Jan 25, 2026' = sum(as.Date('2026-01-25') <= stint_end & as.Date('2026-01-25') >= stint_start),
    'Jan 26, 2026' = sum(as.Date('2026-01-26') <= stint_end & as.Date('2026-01-26') >= stint_start),
    'Jan 27, 2026' = sum(as.Date('2026-01-27') <= stint_end & as.Date('2026-01-27') >= stint_start),
    'Jan 28, 2026' = sum(as.Date('2026-01-28') <= stint_end & as.Date('2026-01-28') >= stint_start),
    'Jan 29, 2026' = sum(as.Date('2026-01-29') <= stint_end & as.Date('2026-01-29') >= stint_start),
    'Jan 30, 2026' = sum(as.Date('2026-01-30') <= stint_end & as.Date('2026-01-30') >= stint_start),
    'Jan 31, 2026' = sum(as.Date('2026-01-31') <= stint_end & as.Date('2026-01-31') >= stint_start),
    'Feb 01, 2026' = sum(as.Date('2026-02-01') <= stint_end & as.Date('2026-02-01') >= stint_start),
    'Feb 02, 2026' = sum(as.Date('2026-02-02') <= stint_end & as.Date('2026-02-02') >= stint_start),
    'Feb 03, 2026' = sum(as.Date('2026-02-03') <= stint_end & as.Date('2026-02-03') >= stint_start),
    'Feb 04, 2026' = sum(as.Date('2026-02-04') <= stint_end & as.Date('2026-02-04') >= stint_start),
    'Feb 05, 2026' = sum(as.Date('2026-02-05') <= stint_end & as.Date('2026-02-05') >= stint_start),
    'Feb 06, 2026' = sum(as.Date('2026-02-06') <= stint_end & as.Date('2026-02-06') >= stint_start),
    'Feb 07, 2026' = sum(as.Date('2026-02-07') <= stint_end & as.Date('2026-02-07') >= stint_start),
    'Feb 08, 2026' = sum(as.Date('2026-02-08') <= stint_end & as.Date('2026-02-08') >= stint_start),
    'Feb 09, 2026' = sum(as.Date('2026-02-09') <= stint_end & as.Date('2026-02-09') >= stint_start),
    'Feb 10, 2026' = sum(as.Date('2026-02-10') <= stint_end & as.Date('2026-02-10') >= stint_start),
    'Feb 11, 2026' = sum(as.Date('2026-02-11') <= stint_end & as.Date('2026-02-11') >= stint_start),
    'Feb 12, 2026' = sum(as.Date('2026-02-12') <= stint_end & as.Date('2026-02-12') >= stint_start),
    'Feb 13, 2026' = sum(as.Date('2026-02-13') <= stint_end & as.Date('2026-02-13') >= stint_start),
    'Feb 14, 2026' = sum(as.Date('2026-02-14') <= stint_end & as.Date('2026-02-14') >= stint_start),
    'Feb 15, 2026' = sum(as.Date('2026-02-15') <= stint_end & as.Date('2026-02-15') >= stint_start),
    'Feb 16, 2026' = sum(as.Date('2026-02-16') <= stint_end & as.Date('2026-02-16') >= stint_start),
    'Feb 17, 2026' = sum(as.Date('2026-02-17') <= stint_end & as.Date('2026-02-17') >= stint_start),
    'Feb 18, 2026' = sum(as.Date('2026-02-18') <= stint_end & as.Date('2026-02-18') >= stint_start),
    'Feb 19, 2026' = sum(as.Date('2026-02-19') <= stint_end & as.Date('2026-02-19') >= stint_start),
    'Feb 20, 2026' = sum(as.Date('2026-02-20') <= stint_end & as.Date('2026-02-20') >= stint_start),
    'Feb 21, 2026' = sum(as.Date('2026-02-21') <= stint_end & as.Date('2026-02-21') >= stint_start),
    'Feb 22, 2026' = sum(as.Date('2026-02-22') <= stint_end & as.Date('2026-02-22') >= stint_start),
    'Feb 23, 2026' = sum(as.Date('2026-02-23') <= stint_end & as.Date('2026-02-23') >= stint_start),
    'Feb 24, 2026' = sum(as.Date('2026-02-24') <= stint_end & as.Date('2026-02-24') >= stint_start),
    'Feb 25, 2026' = sum(as.Date('2026-02-25') <= stint_end & as.Date('2026-02-25') >= stint_start),
    'Feb 26, 2026' = sum(as.Date('2026-02-26') <= stint_end & as.Date('2026-02-26') >= stint_start),
    'Feb 27, 2026' = sum(as.Date('2026-02-27') <= stint_end & as.Date('2026-02-27') >= stint_start),
    'Feb 28, 2026' = sum(as.Date('2026-02-28') <= stint_end & as.Date('2026-02-28') >= stint_start),
    'Mar 01, 2026' = sum(as.Date('2026-03-01') <= stint_end & as.Date('2026-03-01') >= stint_start)
  ) %>%
  ungroup()

write_csv(heatmap_day,"output/heatmap_day.csv")


# horrendous mess to write code
code <- ""
for(t in 0:13){
  c <- t*7
  new <- paste("'Week of ", format(as.Date("2025-12-01")+c, format="%b %d, %Y"), 
        "' = sum(as.Date('", as.Date("2025-12-01")+c,
        "') <= stint_end & as.Date('", as.Date("2025-12-07")+c,
        "') >= stint_start),",sep="")
  code <- paste(code,new,sep="\n")
}
cat(code)

# for days
code <- ""
for(t in 0:90){
  c <- t
  new <- paste("'",format(as.Date("2025-12-01")+c, format="%b %d, %Y"), 
               "' = sum(as.Date('", as.Date("2025-12-01")+c,
               "') <= stint_end & as.Date('", as.Date("2025-12-01")+c,
               "') >= stint_start),",sep="")
  code <- paste(code,new,sep="\n")
}
cat(code)


