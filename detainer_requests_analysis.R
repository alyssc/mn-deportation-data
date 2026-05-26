setwd("/Users/alyssachen/Desktop/Projects/mn-reformer/ice_activity/deportation-data-proj")

library(tidyverse)
library(lubridate)
library(readxl)

## MN DETAINER REQUESTS
detainers <- read_xlsx("data/detainers_filtered_20260331_201550.xlsx",sheet = 2)
detainers <- detainers %>% 
  filter(duplicate_likely==FALSE) %>%
  mutate(detainer_prepare_date = ymd(detainer_prepare_date),
         apprehension_date = ymd(apprehension_date))

# number of total MN detainer requests over time
mn_detainers_over_time <- detainers %>% 
  group_by(detainer_prepare_date) %>%
  summarize(count = n())
write.csv(mn_detainers_over_time,"output/mn_detainers_over_time.csv")

mn_detainers_by_month <- detainers %>% 
  mutate(month_year = format(as.Date(detainer_prepare_date), "%Y-%m")) %>% 
  group_by(month_year) %>% 
  summarize(count = n())

write.csv(mn_detainers_by_month,"output/mn_detainers_by_month.csv")

# Why are there so many detainer requests filed Feb 11? 
feb11 <- detainers %>% 
  filter(detainer_prepare_date == ymd("2026-02-11")) 

feb11 %>%
  group_by(detainer_lift_reason) %>%
  summarize(count=n())

view(feb11 %>% group_by(detention_facility) %>%
       summarize(count=n()) %>%
       arrange(desc(count)))

feb11 %>% group_by(detainer_type, detainer_lift_reason) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

colnames(feb11)

## Metro Surge detainer requests
surge_detainers %>% group_by(detainer_type) %>% summarize(count=n()) %>% arrange(desc(count))

surge_detainers_by_liftreason <- surge_detainers %>% 
  dplyr::select(detention_facility,detainer_lift_reason) %>%
  group_by(detention_facility,detainer_lift_reason) %>%
  summarize(count=n()) %>%
  pivot_wider(names_from = detainer_lift_reason, values_from = count) %>%
  mutate(total = rowSums(across(where(is.numeric)),na.rm=T)) %>% 
  dplyr::arrange(desc(total)) 

surge_detainers_by_liftreason$other <- rowSums(surge_detainers_by_liftreason[,c(5:6,8:13)],na.rm=T)
surge_detainers_by_liftreason<- surge_detainers_by_liftreason[,-c(5:6,8:13)]

write.csv(surge_detainers_by_liftreason,"output/surge_detainers_by_liftreason.csv")

# 247A (48 hr) detainers
surge_detainers %>% 
  filter(startsWith(detainer_type,"I247A")) %>%
  group_by(detainer_lift_reason) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

all_247a <- surge_detainers %>% 
  filter(startsWith(detainer_type,"I247A"))

# 123 that are booked into detention
all_247a %>% dplyr::select(detention_facility,detainer_lift_reason) %>%
  group_by(detention_facility,detainer_lift_reason) %>%
  summarize(count=n()) %>%
  pivot_wider(names_from = detainer_lift_reason, values_from = count) %>%
  mutate(total = rowSums(across(where(is.numeric)),na.rm=T)) %>% 
  dplyr::arrange(desc(total))

# Where are they? 
detained_247a <- all_247a %>% filter(detainer_lift_reason == "Booked into Detention")
detained_247a %>% group_by(detention_facility) %>% summarize(count=n()) %>% arrange(desc(count))

st_cloud_denied <- all_247a %>% filter(
  detainer_lift_reason == "Detainer Declined by LEA",
  detention_facility == "MINN.C.F.,ST.CLOUD") 
write.csv(st_cloud_denied,"output/st_cloud_denied.csv")