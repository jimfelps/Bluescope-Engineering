rm(list = ls(all.names = TRUE))
library(tidyverse)
library(janitor)
library(lubridate)
library(writexl)
library(readxl)
library(patchwork)
library(ggforce)
library(kableExtra)

eng_complexity_lookup <- read_csv("~/R/R Data/S&OP/Engineering/eng_complexity_lookup.csv", 
                                  col_types = cols(`Order Number` = col_character())) %>%
  clean_names("snake") %>%
  mutate(proj_num = str_sub(order_number, 1,8)) %>%
  select(proj_num,
         region,
         complexity)

backlog_status <- read_csv("~/R/R Data/Engineering/Dashboards/backlog_status.csv", 
                           col_types = cols(`COC Date` = col_date(format = "%m/%d/%Y"), 
                                            `DES Date` = col_date(format = "%m/%d/%Y"), 
                                            `DTC Date` = col_date(format = "%m/%d/%Y"), 
                                            `ECK Date` = col_date(format = "%m/%d/%Y"), 
                                            `MFG Date` = col_date(format = "%m/%d/%Y"), 
                                            `Order Number` = col_character(),
                                            `Order Number 1` = col_skip())) %>%
  clean_names() %>%
  mutate(proj_num = str_sub(order_number, 1, 8)) %>%
  select(-country_flag,
         -bucket_sort_year,
         -bucket_sort_month,
         -record_type_backlog) %>%
  filter(coc_status == "AC",
         division_incl_bsc != "BSC")

backlog_status_complexity <- backlog_status %>%
  left_join(eng_complexity_lookup, by = c("proj_num" = "proj_num")) %>%
  filter(!is.na(complexity)) 


backlog_status_complexity$complexity <- factor(backlog_status_complexity$complexity,
                                               levels = c("CII/FT",
                                                          "Simple (<=100)",
                                                          "Moderate (100-200)",
                                                          "Complex (200-400)",
                                                          "High Complex (400-800)",
                                                          "Very High Complex (>800)"))

backlog_status_summary <- backlog_status_complexity %>%
  group_by(complexity) %>%
  summarise(tons = sum(total_tons_backlog))


backlog_status_complexity %>%
  filter(complexity == "Very High Complex (>800)" |
           complexity == "High Complex (400-800)") %>%
  group_by(bucket) %>%
  summarise(tons = sum(total_tons_backlog)) %>%
  View()
