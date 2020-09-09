# Libraries and values ------------------------------------------------------------------

library(tidyverse)
library(janitor)
library(lubridate)
library(readxl)
library(RColorBrewer)
library(wesanderson)

theme_set(theme_minimal())

level_key <- c(`Simple (<=100)` = "Simple",
               `Moderate (100-200)` = "Moderate",
               `Complex (200-400)` = "Complex",
               `High Complex (400-800)` = "High Complexity",
               `Very High Complex (>800)` = "Very High Complexity")

## Import the data -----------------------------------------------------------------------

open_orders_future_dates_change_since_20191001 <- read_excel("//bmkc1alfin/Fin-Acct/1 - Personal Folders/Jimbo/Regional Finance/Regional Reporting/Engineering/Analysis/FY20/10 - Apr20/open_orders_future_dates_change_since_20191001.xlsx", 
                                                             col_types = c("text", "date", "text", 
                                                                           "text", "text", "text", "text", "text", 
                                                                           "text", "date", "date", "date", "text", 
                                                                           "numeric", "numeric", "numeric", 
                                                                           "numeric")) %>%
  clean_names() 


eng_complexity_lookup <- read_csv("~/R/R Data/S&OP/Engineering/eng_complexity_lookup.csv", 
                                  col_types = cols(`Order Number` = col_character())) %>%
  clean_names("snake") %>%
  mutate(proj_num = str_sub(order_number, 1,8)) %>%
  select(proj_num,
         region,
         complexity)

oswt_planned_eck_q4fy20 <- read_excel("//bmkc1alfin/Fin-Acct/1 - Personal Folders/Jimbo/Regional Finance/Regional Reporting/Engineering/Analysis/FY20/10 - Apr20/oswt_planned_eck_q4fy20.xlsx", 
                                      col_types = c("text","text","text","text","text",
                                                          "text","date","date","date","text",
                                                          "text","text","text","text","text",
                                                          "skip","date","date","date","numeric",
                                                          "skip","numeric","numeric","skip","numeric",
                                                          "skip","skip","skip","numeric","skip",
                                                          "skip","skip","skip","skip","skip",
                                                          "skip","skip","skip","skip","skip",
                                                          "skip","skip","text","date","date",
                                                          "date","date","text","date","date",
                                                          "date","date","text","date","date",
                                                          "date","date","text","date","date",
                                                          "date","date","text","date","date",
                                                          "date","date","text","date","date",
                                                          "date","date","skip","date","date",
                                                          "date","date","text","date","date",
                                                          "date","date","text","date","date",
                                                          "date","date","text","date","date",
                                                          "date","date","skip","skip","skip",
                                                          "date","date","text","date","date",
                                                          "date","date","text","date","date",
                                                          "date","date","text","date","date",
                                                          "date","date"), skip = 41) %>%
  mutate(proj_num = str_sub(`Order Number`, 1,8)) %>%
  clean_names() %>%
  left_join(eng_complexity_lookup, by = c("proj_num" = "proj_num")) %>%
  filter(!is.na(complexity)) %>%
  mutate(tons = if_else(order_status == "EN", planned_tons, eng_mfst_tons),
         eck_plan_week = ceiling_date(eck_sched_comp, "week"))

## clean OSWT a bit -----------------------------------------------------------------------------------

oswt_planned_eck_q4fy20$complexity <- recode(oswt_planned_eck_q4fy20$complexity, !!!level_key)

oswt_planned_eck_q4fy20$complexity <- factor(oswt_planned_eck_q4fy20$complexity, 
                                             levels = c("Simple",
                                                        "Moderate",
                                                        "Complex",
                                                        "High Complexity",
                                                        "Very High Complexity"))


complex_summary <- oswt_planned_eck_q4fy20 %>%
  group_by(eck_plan_week, complexity) %>%
  summarise(
    tons = sum(tons)
    )

complex_summary_week <- oswt_planned_eck_q4fy20 %>%
  group_by(eck_plan_week) %>%
  summarise(
    tons = sum(tons)
  )

## clean Gary's on-hold query -------------------------------------------------------------------------

hold_orders <- open_orders_future_dates_change_since_20191001 %>%
  mutate(
    hold_type = if_else(
      new_promise_date > "2029-10-31",
      "< 12 months",
      "Over 12 months"
    ),
    hold_week = ceiling_date(audit_date, "week"),
    ship_month = floor_date(old_promise_date, "month")
  )

hold_summary_week <- hold_orders %>%
  group_by(
    division, 
    region,
    hold_week,
    hold_type) %>%
  summarise(
    tons = sum(planned_tons),
    revenue = sum(order_amt_no_freight)
  )

hold_summary_shipments <- hold_orders %>%
  group_by(
    division, 
    region,
    ship_month) %>%
  summarise(
    tons = sum(planned_tons),
    revenue = sum(order_amt_no_freight)
  )

## visualize engineering schedule by complexity ----------------------------------------------------------  

eng_backlog_makeup <- complex_summary %>%
  ggplot(aes(eck_plan_week, tons, fill = fct_rev(complexity))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = wes_palette(n=5, name = "Moonrise3")) +
#  scale_fill_brewer(palette = "Paired") +
  labs(
    x = "",
    y = "% of Tons",
    title = "Complexity Make-up of Engineering Schedule",
    subtitle = "By ECK Week (Planned)",
    caption = "Source: Order Summary with Tons Report"
  ) +
  theme(
    legend.position = "top",
    legend.title = element_blank()
  )

ggsave("//bmkc1alfin/Fin-Acct/1 - Personal Folders/Jimbo/Regional Finance/Regional Reporting/Engineering/Analysis/FY20/10 - Apr20/eck_schedule_percent.png",
       plot = last_plot(),
       width = 16, 
       height = 9, 
       dpi = "retina")

eng_complex_weekly <- complex_summary %>%
  ggplot(aes(eck_plan_week, tons)) +
  geom_bar(stat = "identity") +
  #scale_fill_manual(values = wes_palette(n=5, name = "Moonrise3")) +
  #  scale_fill_brewer(palette = "Paired") +
  labs(
    x = "",
    y = "Tons",
    title = "Complexity Make-up of Engineering Schedule",
    subtitle = "By ECK Week (Planned)",
    caption = "Source: Order Summary with Tons Report"
  ) +
  facet_grid(
    rows = vars(complexity)
  )

ggsave("//bmkc1alfin/Fin-Acct/1 - Personal Folders/Jimbo/Regional Finance/Regional Reporting/Engineering/Analysis/FY20/10 - Apr20/eck_schedule_tons.png",
       plot = last_plot(),
       width = 16, 
       height = 9, 
       dpi = "retina")

## visualize holds by week ------------------------------------------------------------------------------------------------

# ALL HOLDS BY DIVISION
holds_by_division <- hold_summary_week %>%
  ggplot(aes(hold_week, tons)) +
  geom_bar(stat = "identity") +
  facet_grid(
    rows = vars(division)
  ) +
  labs(
    x = "",
    y = "Tons",
    title = "Tons Placed on Hold by Brand",
    subtitle = "By Week Promise Date changed",
    caption = "Source: Oracle Query of orders w/ promise date changed to 1/1/2030 or 10/31/2029"
  )

ggsave("//bmkc1alfin/Fin-Acct/1 - Personal Folders/Jimbo/Regional Finance/Regional Reporting/Engineering/Analysis/FY20/10 - Apr20/hold_week_by_division.png",
       plot = last_plot(),
       width = 16, 
       height = 9, 
       dpi = "retina")

# ALL HOLD BY DIV/REGION

holds_by_region_butler <- hold_summary_week %>%
  filter(division == "BUTLER",
         region != "BUC") %>%
  ggplot(aes(hold_week, tons)) +
  geom_bar(stat = "identity") +
  facet_grid(
    rows = vars(region)
  ) +
  labs(
    x = "",
    y = "Tons",
    title = "Butler Tons Placed on Hold by Region",
    subtitle = "By Week Promise Date changed",
    caption = "Source: Oracle Query of orders w/ promise date changed to 1/1/2030 or 10/31/2029"
  )

ggsave("//bmkc1alfin/Fin-Acct/1 - Personal Folders/Jimbo/Regional Finance/Regional Reporting/Engineering/Analysis/FY20/10 - Apr20/hold_week_butler.png",
       plot = last_plot(),
       width = 16, 
       height = 9, 
       dpi = "retina")

holds_by_region_vp <- hold_summary_week %>%
  filter(division == "VP",
         region != "BUC") %>%
  ggplot(aes(hold_week, tons)) +
  geom_bar(stat = "identity") +
  facet_grid(
    rows = vars(region)
  ) +
  labs(
    x = "",
    y = "Tons",
    title = "VP Tons Placed on Hold by Region",
    subtitle = "By Week Promise Date changed",
    caption = "Source: Oracle Query of orders w/ promise date changed to 1/1/2030 or 10/31/2029"
  )

ggsave("//bmkc1alfin/Fin-Acct/1 - Personal Folders/Jimbo/Regional Finance/Regional Reporting/Engineering/Analysis/FY20/10 - Apr20/hold_week_vp.png",
       plot = last_plot(),
       width = 16, 
       height = 9, 
       dpi = "retina")

##############################################
# Looks like most holds have been the "Under 12 month" variant so for now I'm not going to create these graphs

# HOLDS BY DIVISION/HOLD TYPE

#holds_by_type_butler <- hold_summary_week %>%
#  filter(division == "BUTLER",
#         region != "BUC") %>%
#  ggplot(aes(hold_week, tons)) +
#  geom_bar(stat = "identity") +
#  facet_grid(
#    rows = vars(hold_type)
#  )
#
#holds_by_type_vp <- hold_summary_week %>%
#  filter(division == "VP",
#         region != "BUC") %>%
#  ggplot(aes(hold_week, tons)) +
#  geom_bar(stat = "identity") +
#  facet_grid(
#    rows = vars(hold_type)
#  )
#
hold_summary_week %>%
  group_by(hold_type) %>%
  summarise(tons = sum(tons)) %>% view()

## visualize holds by projected ship date -------------------------------------------------------------------------------

hold_shipments_division <- hold_summary_shipments %>% 
  filter(ship_month < 	"2029-09-01") %>%
  ggplot(aes(ship_month, tons)) +
  geom_bar(stat = "identity") +
  facet_grid(
    rows = vars(division)
  ) +
  labs(
    x = "",
    y = "Tons",
    title = "Previous Expected Ship Month of Tons Placed on Hold",
   # subtitle = "By Week Promise Date changed",
    caption = "Source: Oracle Query of orders w/ promise date changed to 1/1/2030 or 10/31/2029"
  )

ggsave("//bmkc1alfin/Fin-Acct/1 - Personal Folders/Jimbo/Regional Finance/Regional Reporting/Engineering/Analysis/FY20/10 - Apr20/hold_shipments_by_division.png",
       plot = last_plot(),
       width = 16, 
       height = 9, 
       dpi = "retina")
  
hold_shipments_butler <- hold_summary_shipments %>% 
  filter(ship_month < 	"2029-09-01",
         division == "BUTLER") %>%
  ggplot(aes(ship_month, tons)) +
  geom_bar(stat = "identity") +
  facet_grid(
    rows = vars(region)
  ) +
  labs(
    x = "",
    y = "Tons",
    title = "Butler Shipments Lost to Holds",
    # subtitle = "By Week Promise Date changed",
    caption = "Source: Oracle Query of orders w/ promise date changed to 1/1/2030 or 10/31/2029"
  )

ggsave("//bmkc1alfin/Fin-Acct/1 - Personal Folders/Jimbo/Regional Finance/Regional Reporting/Engineering/Analysis/FY20/10 - Apr20/hold_shipments_butler.png",
       plot = last_plot(),
       width = 16, 
       height = 9, 
       dpi = "retina")

hold_shipments_vp <- hold_summary_shipments %>% 
  filter(ship_month < 	"2029-09-01",
         division == "VP",
         region != "BUC") %>%
  ggplot(aes(ship_month, tons)) +
  geom_bar(stat = "identity") +
  facet_grid(
    rows = vars(region)
  ) +
  labs(
    x = "",
    y = "Tons",
    title = "VP Shipments Lost to Holds",
    # subtitle = "By Week Promise Date changed",
    caption = "Source: Oracle Query of orders w/ promise date changed to 1/1/2030 or 10/31/2029"
  )

ggsave("//bmkc1alfin/Fin-Acct/1 - Personal Folders/Jimbo/Regional Finance/Regional Reporting/Engineering/Analysis/FY20/10 - Apr20/hold_shipments_vp.png",
       plot = last_plot(),
       width = 16, 
       height = 9, 
       dpi = "retina")


# Simple/Moderate work totals in 4Q -----------------------------------------------------------------------------------

complex_summary %>%
  filter(complexity == "Simple" |
           complexity == "Moderate") %>%
  group_by(complexity) %>%
  summarise(tons = sum(tons)) %>% view()