rm(list = ls(all.names = TRUE))
library(tidyverse)
library(lubridate)
library(readxl)
library(extrafont)
library(extrafontdb)
library(ggridges)

fy17_eck <- c(as.Date("2016-07-01"), 
              as.Date("2016-08-01"), 
              as.Date("2016-09-01"), 
              as.Date("2016-10-01"), 
              as.Date("2016-11-01"), 
              as.Date("2016-12-01"), 
              as.Date("2017-01-01"), 
              as.Date("2017-02-01"), 
              as.Date("2017-03-01"), 
              as.Date("2017-04-01"), 
              as.Date("2017-05-01"), 
              as.Date("2017-06-01"))
fy18_eck <- c(as.Date("2017-07-01"), 
              as.Date("2017-08-01"), 
              as.Date("2017-09-01"), 
              as.Date("2017-10-01"), 
              as.Date("2017-11-01"), 
              as.Date("2017-12-01"), 
              as.Date("2018-01-01"), 
              as.Date("2018-02-01"), 
              as.Date("2018-03-01"), 
              as.Date("2018-04-01"), 
              as.Date("2018-05-01"), 
              as.Date("2018-06-01"))
fy19_eck <- c(as.Date("2018-07-01"), 
              as.Date("2018-08-01"), 
              as.Date("2018-09-01"), 
              as.Date("2018-10-01"), 
              as.Date("2018-11-01"), 
              as.Date("2018-12-01"), 
              as.Date("2019-01-01"), 
              as.Date("2019-02-01"), 
              as.Date("2019-03-01"), 
              as.Date("2019-04-01"), 
              as.Date("2019-05-01"), 
              as.Date("2019-06-01"))
fy20_eck <- c(as.Date("2019-07-01"), 
              as.Date("2019-08-01"), 
              as.Date("2019-09-01"), 
              as.Date("2019-10-01"), 
              as.Date("2019-11-01"), 
              as.Date("2019-12-01"), 
              as.Date("2020-01-01"), 
              as.Date("2020-02-01"), 
              as.Date("2020-03-01"), 
              as.Date("2020-04-01"), 
              as.Date("2020-05-01"), 
              as.Date("2020-06-01"))
last_three_eck <- c(fy19_eck, fy18_eck, fy17_eck)

eng_complexity_lookup <- read_csv("~/R/R Data/Engineering/MBR Charts/eng_complexity_lookup.csv")
backlog_tons_detail <- read_csv("~/R/R Data/Engineering/MBR Charts/backlog_tons_detail.csv")
eng_backlog_v2 <- read_csv("~/R/R Data/Engineering/MBR Charts/eng_backlog_v2.csv", 
                           col_types = cols(`Order Creation Date` = col_date(format = "%m/%d/%Y"), 
                                            `Order Number` = col_character(), 
                                            `ECK Date` = col_date(format = "%m/%d/%Y"),
                                            `BD_Act_Compl_Date` = col_date(format = "%m/%d/%Y")))

# create project number field in a given data frame

eng_hours <- eng_complexity_lookup %>%
  group_by(`Order Number`) %>%
  summarise(act_hours = round(sum(`Actual Hours`),2),
            budget_hours = round(sum(`Budget Hours`),2)) %>%
  rename(order_number = `Order Number`)

eng_complexity <- eng_complexity_lookup %>% 
  mutate(proj_num = str_sub(`Order Number`, 1, 8)) %>%
  select(`Project Name`,
         Status, 
         `Order Number`,
         `Budget Hours`,
         `Actual Hours`,
         Complexity,
         Division,
         Region,
         proj_num)

no_val <- eng_complexity$`Budget Hours` == 0
eng_complexity$Complexity[no_val] <- "No Engineering Budget"

eng_backlog_select <- eng_backlog_v2 %>% 
  mutate(proj_num = str_sub(`Order Number`, 1, 8)) %>%
  filter(substr(`Transaction Type`,1,3) != "CSS") %>% #take out CSS jobs from the start since they have their own engineering resources
  left_join(eng_complexity, by = c("proj_num" = "proj_num")) %>%
  select(Division.x,
         `Order Number.x`,
         #Region,
         `Customer Name`,
         proj_num,
         `Order Creation Date`,
         `ECK Date`,
         BD_Act_Compl_Date,
         `Budget Hours.x`,
         `Actual Hours.x`,
         Complexity) %>%
  rename("division" = "Division.x",
         "order_number" = "Order Number.x",
         "customer_name" = "Customer Name",
         "order_create_date" = "Order Creation Date",
         "eck_date" = "ECK Date",
         "budget_hours" = "Budget Hours.x",
         "acutal_hours" = "Actual Hours.x")

# could filter out NA's in the code above but this will give visibility into what's filtered
# when I wrote this, the NA's were BLA region trx as well as BSC I/C orders and a couple intl I/C orders
eng_backlog_na <- eng_backlog_select %>%
  filter(is.na(Complexity))

no_budget_no_eck <- eng_backlog_select %>%
       filter(Complexity == "No Engineering Budget" &
                is.na(eck_date))

eng_backlog_select_final <- eng_backlog_select %>%
  filter(!is.na(Complexity),
         Complexity != "No Engineering Budget",
         !is.na(eck_date),
         budget_hours >= 15)

eng_backlog_select_final <- eng_backlog_select_final %>%
  mutate(eck_lead_wks = round(((eck_date - order_create_date) / 7),2),
         eck_range_high = round((budget_hours * 0.025) + 12, 2),
         eck_range_low = round((budget_hours * 0.0075) + 0.05, 2),
         include = if_else(
           eck_lead_wks <= (eck_range_high + 6) & eck_lead_wks >= eck_range_low, "Yes", "No"),
         days_to_book = round(BD_Act_Compl_Date - eck_date, 2),
         eck_month = floor_date(eck_date, "month"),
         book_month = floor_date(BD_Act_Compl_Date, "month")) %>%
  select(division,
         order_number,
         customer_name,
         proj_num,
         Complexity,
         eck_lead_wks,
         include,
         eck_month,
         book_month,
         days_to_book)

eng_backlog_select_final$eck_lead_wks <- as.numeric(eng_backlog_select_final$eck_lead_wks)

eng_backlog_select_final$Complexity <- factor(eng_backlog_select_final$Complexity,
                                              levels = c("CII/FT",
                                                         "Simple (<=100)",
                                                         "Moderate (100-200)",
                                                         "Complex (200-400)",
                                                         "High Complex (400-800)",
                                                         "Very High Complex (>800)"))

lead_time_avg <- eng_backlog_select_final %>%
  filter(include == "Yes",
         eck_month %in% fy20_eck) %>%
  summarise(avg = mean(eck_lead_wks, na.rm = TRUE)) %>%
  pull(avg)

fy19_lead_time_avg <- eng_backlog_select_final %>%
  group_by(Complexity) %>%
  filter(include == "Yes",
         eck_month %in% fy19_eck) %>%
  summarise(avg = mean(eck_lead_wks, na.rm = TRUE)) %>%
  ungroup()
fy18_lead_time_avg <- eng_backlog_select_final %>%
  group_by(Complexity) %>%
  filter(include == "Yes",
         eck_month %in% fy18_eck) %>%
  summarise(avg = mean(eck_lead_wks, na.rm = TRUE)) %>%
  ungroup()
fy17_lead_time_avg <- eng_backlog_select_final %>%
  group_by(Complexity) %>%
  filter(include == "Yes",
         eck_month %in% fy17_eck) %>%
  summarise(avg = mean(eck_lead_wks, na.rm = TRUE)) %>%
  ungroup()
  
loadfonts(device = "win")

theme_set(theme_light(base_size = 15, base_family = "Poppins"))

g <- eng_backlog_select_final %>%
  filter(include == "Yes",
         eck_month %in% fy20_eck) %>%
  ggplot(aes(x = Complexity, y = eck_lead_wks, color = Complexity)) +
    coord_flip() +
    labs(x = NULL, y = "Engineering Lead Time in Weeks") +
    #ggtitle("Engineering Lead Times by Complexity") +
    theme(legend.position = "none",
        axis.title = element_text(size = 12),
        axis.text.x = element_text(family = "Roboto Mono", size = 10),
        panel.grid = element_blank())

engineering_lead <- g +
  geom_hline(aes(yintercept = lead_time_avg), color = "gray70", size = 0.6) +
  geom_jitter(size = 2, alpha = 0.25, width = 0.2) +
  stat_summary(fun.y = mean, geom = "point", size = 5, color = "gray20") +
  geom_point(data = fy17_lead_time_avg, aes(x = Complexity, y = avg, color = "C06C84", size = 4, alpha = 0.9)) +
  #geom_point(data = fy19_lead_time_avg, aes(x = Complexity, y = avg, color = "F8B195", size = 4, alpha = 0.8)) +
  annotate("text", x = 6.4, y = 35, family = "Poppins", size = 2.7, color = "gray20",
           label = "Large red dots represent FY17 average \nlead time by complexity") +
  annotate("text", x = 4.5, y = 29, family = "Poppins", size = 2.7, color = "gray20",
           label = "Black dots represent FY20 \naverage lead time by complexity") +
  annotate("text", x = 6, y = 4, family = "Poppins", size = 2.7, color = "gray20",
           label = "FY20 average across complexities") +
  geom_curve(aes(x = 6.4, xend = 6.05,
                 y = 32, yend = 30, color = "gray20"), arrow = arrow(length = unit(0.01, "npc"))) +
  geom_curve(aes(x = 4.5, xend = 4.9,
                 y = 26.5, yend = 20, color = "gray20"), arrow = arrow(length = unit(0.01, "npc")), curvature = -0.5) +
  geom_curve(aes(x = 5.8, xend = 5.35,
                 y = 6, yend = 10.75, color = "gray20"), arrow = arrow(length = unit(0.01, "npc")), curvature = 0.25)

ggsave("~/R/Git/Project/Bluescope-Engineering/engineering_lead.png", engineering_lead, width = 16, height = 9, dpi = 320)
