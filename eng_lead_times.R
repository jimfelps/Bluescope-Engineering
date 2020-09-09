rm(list = ls(all.names = TRUE))
library(tidyverse)
library(lubridate)
library(readxl)
library(extrafont)
library(extrafontdb)
library(ggridges)
library(gganimate)
library(gifski)
library(png)

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
ltm_eck <- c(as.Date("2018-11-01"), 
             as.Date("2018-12-01"), 
             as.Date("2019-01-01"), 
             as.Date("2019-02-01"), 
             as.Date("2019-03-01"), 
             as.Date("2019-04-01"), 
             as.Date("2019-05-01"), 
             as.Date("2019-06-01"),
             as.Date("2019-07-01"), 
             as.Date("2019-08-01"), 
             as.Date("2019-09-01"), 
             as.Date("2019-10-01"))
last_three_eck <- c(fy19_eck, fy18_eck, fy17_eck)

eng_complexity_lookup <- read_csv("~/R/R Data/Engineering/MBR Charts/eng_complexity_lookup.csv")
backlog_tons_detail <- read_csv("~/R/R Data/Engineering/MBR Charts/backlog_tons_detail.csv")
eng_backlog_v2 <- read_csv("~/R/R Data/Engineering/MBR Charts/eng_backlog_v2.csv", 
                           col_types = cols(`Order Creation Date` = col_date(format = "%m/%d/%Y"), 
                                            `Order Number` = col_character(), 
                                            `ECK Date` = col_date(format = "%m/%d/%Y"),
                                            `BD_Act_Compl_Date` = col_date(format = "%m/%d/%Y"))) #this is just the engineering complexity lookup. Seems I don't need the lookup in this script but it was late and I copied it from another script

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
  #left_join(eng_complexity, by = c("proj_num" = "proj_num")) %>%
  select(Division,
         `Order Number`,
         Region,
         #`Customer Name`,
         proj_num,
         `Order Creation Date`,
         ECK_Date,
         BD_Act_Compl_Date,
         `Budget Hours`,
         `Actual Hours`,
         Complexity) %>%
  rename("division" = "Division",
         "order_number" = "Order Number",
         #"customer_name" = "Customer Name",
         "order_create_date" = "Order Creation Date",
         "eck_date" = "ECK_Date",
         "budget_hours" = "Budget Hours",
         "acutal_hours" = "Actual Hours",
         "complexity" = "Complexity",
         "division" = "Division")

# could filter out NA's in the code above but this will give visibility into what's filtered
# when I wrote this, the NA's were BLA region trx as well as BSC I/C orders and a couple intl I/C orders
eng_backlog_na <- eng_backlog_select %>%
  filter(is.na(complexity))

no_budget_no_eck <- eng_backlog_select %>%
       filter(complexity == "No Engineering Budget" &
                is.na(eck_date))

eng_backlog_select_final <- eng_backlog_select %>%
  filter(!is.na(complexity),
         complexity != "No Engineering Budget",
         !is.na(eck_date),
         budget_hours >= 15)

check_lead_limits <- eng_backlog_select %>%
  mutate(eck_lead_wks = round(((eck_date - order_create_date) / 7),2),
         eck_range_high = round((budget_hours * 0.025) + 12, 2),
         eck_range_low = round((budget_hours * 0.0075) + 0.05, 2),
         include = if_else(
           eck_lead_wks <= (eck_range_high) & eck_lead_wks >= eck_range_low, "Yes", "No"),
         days_to_book = round(BD_Act_Compl_Date - eck_date, 2),
         eck_month = floor_date(eck_date, "month"),
         book_month = floor_date(BD_Act_Compl_Date, "month"),
         fiscal_year = if_else(eck_month %in% fy17_eck, 2017,
                               if_else(eck_month %in% fy18_eck, 2018,
                                       if_else(eck_month %in% fy19_eck, 2019, 2020)))) %>%
  filter(include == "No") %>%
  mutate(above_limit = eck_lead_wks - eck_range_high) %>%
  arrange(above_limit) %>%
  filter(above_limit >= 0 & above_limit <= 1.5)

eng_backlog_select_final <- eng_backlog_select_final %>%
  mutate(eck_lead_wks = round(((eck_date - order_create_date) / 7),2),
         eck_range_high = round((budget_hours * 0.025) + 12, 2),
         eck_range_low = round((budget_hours * 0.0075) + 0.05, 2),
         include = if_else(
           eck_lead_wks <= (eck_range_high) & eck_lead_wks >= eck_range_low, "Yes", "No"),
         days_to_book = round(BD_Act_Compl_Date - eck_date, 2),
         eck_month = floor_date(eck_date, "month"),
         book_month = floor_date(BD_Act_Compl_Date, "month"),
         fiscal_year = if_else(eck_month %in% fy17_eck, 2017,
                               if_else(eck_month %in% fy18_eck, 2018,
                                       if_else(eck_month %in% fy19_eck, 2019, 2020)))) %>%
  select(division,
         order_number,
         #customer_name,
         proj_num,
         complexity,
         fiscal_year,
         eck_lead_wks,
         include,
         eck_month,
         book_month,
         days_to_book) %>%
  arrange(fiscal_year)

eng_backlog_select_final$eck_lead_wks <- as.numeric(eng_backlog_select_final$eck_lead_wks)

eng_backlog_select_final$complexity <- factor(eng_backlog_select_final$complexity,
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

fy20_lead_time_avg <- eng_backlog_select_final %>%
  group_by(complexity) %>%
  filter(include == "Yes",
         eck_month %in% fy20_eck) %>%
  summarise(avg = mean(eck_lead_wks, na.rm = TRUE)) %>%
  ungroup()
fy19_lead_time_avg <- eng_backlog_select_final %>%
  group_by(complexity) %>%
  filter(include == "Yes",
         eck_month %in% fy19_eck) %>%
  summarise(avg = mean(eck_lead_wks, na.rm = TRUE)) %>%
  ungroup()
fy18_lead_time_avg <- eng_backlog_select_final %>%
  group_by(complexity) %>%
  filter(include == "Yes",
         eck_month %in% fy18_eck) %>%
  summarise(avg = mean(eck_lead_wks, na.rm = TRUE)) %>%
  ungroup()
fy17_lead_time_avg <- eng_backlog_select_final %>%
  group_by(complexity) %>%
  filter(include == "Yes",
         eck_month %in% fy17_eck) %>%
  summarise(avg = mean(eck_lead_wks, na.rm = TRUE)) %>%
  ungroup()
  
loadfonts(device = "win")

theme_set(theme_light(base_size = 15, base_family = "Poppins"))

all_points <- eng_backlog_select_final %>%
  filter(include == "Yes") %>%
  ggplot(aes(x = complexity, y = eck_lead_wks, color = complexity)) +
    coord_flip() +
    labs(x = NULL, y = "Engineering Lead Time in Weeks") +
    #ggtitle("Engineering Lead Times by Complexity") +
    theme(legend.position = "none",
        axis.title = element_text(size = 12),
        axis.text.x = element_text(family = "Roboto Mono", size = 10),
        panel.grid = element_blank())

engineering_lead <- all_points +
  ggtitle("Last 3 Years") +
  #geom_hline(aes(yintercept = lead_time_avg), color = "gray70", size = 0.6) +
  geom_jitter(size = 2, alpha = 0.25, width = 0.2) +
  #stat_summary(fun.y = mean, geom = "point", size = 5, color = "gray20") +
  scale_color_manual(values = c("#F8971F", "#FFD600", "#8A8D8F", "#BF5700", "#579D42", "#00A9B7", "#005F86", "#CFB500")) +
  geom_point(data = fy20_lead_time_avg, aes(x = complexity, y = avg, color = "gray20", size = 4)) +
  geom_point(data = fy17_lead_time_avg, aes(x = complexity, y = avg, color = "red", size = 4)) +
  scale_y_continuous(breaks=c(5,10,15,20,25,30,35,40,45,50)) +
  #geom_point(data = fy19_lead_time_avg, aes(x = Complexity, y = avg, color = "F8B195", size = 4, alpha = 0.8)) +
  annotate("text", x = 6.4, y = 36, family = "Poppins", size = 2.7, color = "gray20",
           label = "Large teal dots represent FY17 average \nlead time by complexity") +
  annotate("text", x = 4.5, y = 31, family = "Poppins", size = 2.7, color = "gray20",
           label = "Gray dots represent FY20 \naverage lead time by complexity") +
  #annotate("text", x = 6, y = 4, family = "Poppins", size = 2.7, color = "gray20",
  #         label = "FY20 average across complexities") +
  geom_curve(aes(x = 6.4, xend = 6.05,
                 y = 32, yend = 30, color = "gray20"), arrow = arrow(length = unit(0.01, "npc"))) +
  geom_curve(aes(x = 4.4, xend = 4.9,
                 y = 26.5, yend = 19.3, color = "gray20"), arrow = arrow(length = unit(0.01, "npc")), curvature = -0.5)
  #geom_curve(aes(x = 5.8, xend = 5.35,
  #               y = 6, yend = 9.48, color = "gray20"), arrow = arrow(length = unit(0.01, "npc")), curvature = 0.25)
engineering_lead

ggsave("~/R/Git/Project/Bluescope-Engineering/engineering_lead.png", engineering_lead, width = 16, height = 9, dpi = 320)

base_chart <- eng_backlog_select_final %>%
  filter(include == "Yes") %>%
  ggplot(aes(x = complexity, y = eck_lead_wks, color = complexity)) +
  coord_flip() +
  labs(x = NULL, y = "Engineering Lead Time in Weeks") +
  #ggtitle("Engineering Lead Times by Complexity") +
  theme(legend.position = "none",
        axis.title = element_text(size = 12),
        axis.text.x = element_text(family = "Roboto Mono", size = 10),
        panel.grid = element_blank()) +
  geom_jitter(size = 2, alpha = 0.25, width = 0.2) +
  stat_summary(fun.y = mean, geom = "point", size = 5, color = "gray20")

animate <- base_chart +
  transition_time(as.integer(fiscal_year))

animation <- animate +
  #ease_aes('cubic-in-out') +
  enter_grow() +
  enter_fade() +
  ggtitle("{frame_time}",
          subtitle = "Frame {frame} of {nframes}")
animation

anim_save("~/R/Git/Project/Bluescope-Engineering/animation.gif", animation = animation)


# next I'm going to look just at the past 12 months to see if we find any improvement in lead time avg
ltm_max <- eng_backlog_select_final %>%
  group_by(complexity, eck_month) %>%
  filter(include == "Yes",
         eck_month %in% ltm_eck) %>%
  summarise(avg = mean(eck_lead_wks, na.rm = TRUE)) %>%
  filter(eck_month == max(eck_month)) %>%
  ungroup()
ltm_min <- eng_backlog_select_final %>%
  group_by(complexity, eck_month) %>%
  filter(include == "Yes",
         eck_month %in% ltm_eck) %>%
  summarise(avg = mean(eck_lead_wks, na.rm = TRUE)) %>%
  filter(eck_month == min(eck_month)) %>%
  ungroup()

ltm_lead_time_avg <- eng_backlog_select_final %>%
  filter(include == "Yes",
         eck_month %in% ltm_eck) %>%
  summarise(avg = mean(eck_lead_wks, na.rm = TRUE)) %>%
  pull(avg)

ltm_points <- eng_backlog_select_final %>%
  filter(include == "Yes",
         eck_month %in% ltm_eck) %>%
  ggplot(aes(x = complexity, y = eck_lead_wks, color = complexity)) +
  coord_flip() +
  labs(x = NULL, y = "Engineering Lead Time in Weeks") +
  #ggtitle("Engineering Lead Times by Complexity") +
  theme(legend.position = "none",
        axis.title = element_text(size = 12),
        axis.text.x = element_text(family = "Roboto Mono", size = 10),
        panel.grid = element_blank())

engineering_lead_ltm <- ltm_points +
  ggtitle("Last 12 Months") +
  #geom_hline(aes(yintercept = ltm_lead_time_avg), color = "gray70", size = 0.6) +
  geom_jitter(size = 2, alpha = 0.25, width = 0.2) +
  #stat_summary(fun.y = mean, geom = "point", size = 5, color = "gray20") +
  scale_color_manual(values = c("#F8971F", "#FFD600", "#8A8D8F", "#BF5700", "#579D42", "#00A9B7", "#005F86", "#CFB500")) +
  scale_y_continuous(breaks=c(5,10,15,20,25,30,35,40,45,50)) +
  geom_point(data = ltm_max, aes(x = complexity, y = avg, color = "gray20", size = 4)) +
  geom_point(data = ltm_min, aes(x = complexity, y = avg, color = "red", size = 4)) +
  #geom_point(data = fy19_lead_time_avg, aes(x = Complexity, y = avg, color = "F8B195", size = 4, alpha = 0.8)) +
  annotate("text", x = 6.4, y = 36, family = "Poppins", size = 2.7, color = "gray20",
           label = "Large teal dots represent average \nfrom 12 months ago") +
  annotate("text", x = 4.5, y = 31, family = "Poppins", size = 2.7, color = "gray20",
           label = "Gray dots represent average \nfor most recently completed month") +
  geom_curve(aes(x = 6.4, xend = 6.05,
                 y = 32, yend = 24.3, color = "gray20"), arrow = arrow(length = unit(0.01, "npc")), curvature = 0.25) +
  geom_curve(aes(x = 4.25, xend = 3.9,
                 y = 31, yend = 16.4, color = "gray20"), arrow = arrow(length = unit(0.01, "npc")), curvature = -0.5)
  
engineering_lead_ltm

ggsave("~/R/Git/Project/Bluescope-Engineering/engineering_lead_ltm.png", engineering_lead_ltm, width = 16, height = 9, dpi = 320)

write_csv(eng_backlog_select_final, "~/R/Git/Project/Bluescope-Engineering/eng_backlog_select_final.csv")
write_csv(check_lead_limits, "~/R/Git/Project/Bluescope-Engineering/check_lead_limits.csv")