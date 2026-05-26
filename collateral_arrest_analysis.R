setwd("/Users/alyssachen/Desktop/Projects/mn-reformer/ice_activity/deportation-data-proj")

library(tidyverse)
library(lubridate)
library(readxl)

## COLLATERAL ARRESTS
mn_arrests <- read_csv("data/arrests_filtered_20260406_154124_mn_all.csv")
mn_arrests <- mn_arrests %>%
  filter(duplicate_likely == FALSE) # de-duplicate

# surge stays
mn_stays <- read_xlsx("data/detention-stays_filtered_20260401_211316.xlsx",sheet = 2)

surge_stays <- mn_stays %>% 
  mutate(stay_book_in_date = as.Date(stay_book_in_date_time)) %>%
  filter(stay_book_in_date >= as.Date("2025-12-01"))

# collateral gets recorded starting august 2025
mn_arrests %>% 
  mutate(apprehension_date = as.Date(apprehension_date)) %>%
  filter(is.na(apprehension_type)) %>%
  arrange(desc(apprehension_date)) 

mn_arrests <- mn_arrests %>%filter(apprehension_date > as.Date("2025-09-01")) 

mn_arrests %>% 
  group_by(apprehension_type) %>% summarize(count=n()) %>% arrange(desc(count))

mn_collateral <- mn_arrests %>% 
  group_by(apprehension_type, apprehension_date) %>% 
  summarize(count=n()) %>%
  pivot_wider(names_from = apprehension_type, values_from = count) %>%
  replace(is.na(.), 0)

write.csv(mn_collateral,"output/mn_collateral.csv")

# how many during surge were collateral? --> 34.84%
surge_arrests <- mn_arrests %>%filter(apprehension_date > as.Date("2025-12-01")) 
surge_arrests %>% 
  group_by(apprehension_type) %>% summarize(count=n()) %>% arrange(desc(count))

# how many in sept, oct, nov were collateral? 
mn_arrests %>% filter(apprehension_date < as.Date("2025-12-01")) %>% 
  group_by(apprehension_type) %>% summarize(count=n()) %>% arrange(desc(count))

# during december?
mn_arrests %>% filter(apprehension_date > as.Date("2025-12-01") & apprehension_date <= as.Date("2025-12-14")) %>% 
  group_by(apprehension_type) %>% summarize(count=n()) %>% arrange(desc(count))

# finding the 5 people detained in Jan. 14 shooting
table_1 <- mn_arrests %>% 
  filter(apprehension_date %in% c(as.Date("2026-01-14"),as.Date("2026-01-15")),
         citizenship_country=="VENEZUELA",
         birth_year %in% c(1997:2001, 2006),
         is.na(departure_country)) %>%
  dplyr::select(apprehension_date_time, apprehension_type, birth_year, gender, unique_identifier, apprehension_criminality)

view(table_1)
view(mn_arrests %>% filter(apprehension_date == as.Date("2026-01-14"), birth_year %in% c(1998:2001) ) )
view(
  surge_stays %>% 
    filter(unique_identifier %in% 
             c("4f7da52b17de54f8c587cf4729b5929842fd50db",
               "a0f51ebac73653f0bed0c81177efb9ccfa439ddc",
               "8ee9f41abfd25d01b1f2a2f2a6679fcc1ff3e505",
               "de56b9fb0e3a059eef98ffc3e1c0b2cfb6ee6363",
               "4001c89ecc44c5926b5b2fe582da476499a5033e",
               "33ace600dcc088257d523f54fdd12a657dbfea08")) %>%
    dplyr::select(unique_identifier,gender,birth_year,detention_facility_codes_all,stay_book_in_date_time,
                  stay_book_out_date_time,detention_release_reason,
                  msc_charge,most_serious_conviction_code)
)

# Feb. 13 arrest
view(mn_arrests %>% 
       filter(apprehension_date %in% c(as.Date("2026-02-13")),
              citizenship_country=="ECUADOR",
              birth_year %in% c(1973:1975),
              is.na(departure_country)))
view(mn_arrests %>% 
       filter(apprehension_date %in% c(as.Date("2026-02-08")),
              citizenship_country=="EL SALVADOR",
              is.na(departure_country)))
view(
  surge_stays %>% 
    filter(unique_identifier %in% 
             c("2c18fc5245cc3f822401a6b87da057b4c693775d"))
)
