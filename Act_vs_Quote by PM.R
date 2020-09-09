rm(list = ls(all.names = TRUE))
library(tidyverse)
library(readxl)
library(janitor)
library(patchwork)
library(writexl)

month <- "FY19"
labels <- c(
  "CII/FT" = "CII/FT",
  "Simple" = "Simple\n < 100",
  "Moderate" = "Moderate\n < 200",
  "Complex" = "Complex\n < 400",
  "High Complexity" = "High\n Complexity\n < 800",
  "Very High Complexity" = "Very High\n Complexity\n 800+"
)

theme_set(theme_light())

eng_complexity_lookup <- read_csv("~/R/R Data/Engineering/MBR Charts/eng_complexity_lookup.csv", 
                                  col_types = cols(`Order Number` = col_character())) %>%
  clean_names("snake") %>%
  mutate(proj_num = str_sub(order_number, 1,8)) %>%
  select(proj_num,
         region,
         complexity)

fy19_order_summary <- read_csv("~/R/R Data/Engineering/Monthly Metrics/Order Summary Reports/fy19_order_summary.csv", 
                               col_types = cols(`Order Number` = col_character()), 
                               skip = 41) %>%
  clean_names("snake") %>%
  mutate(proj_num = str_sub(order_number, 1,8))

new_oswt <- fy19_order_summary %>%
  mutate(brand = if_else(str_detect(order_type, "CLAIM"), "CLAIM",
                         if_else(str_detect(order_type, "PART"), "PARTS ORDER",
                                 if_else(str_detect(order_type, "GB"), "BUTLER",
                                         if_else(str_detect(order_type, "VP"), "VP",
                                                 if_else(str_detect(order_type, "CSS/CONV"), "CSS",
                                                         if_else(str_detect(order_type, "CSS/HS"), "HEAVY STRUCTURES",
                                                                 if_else(str_sub(order_type, 1,4) == "ROOF", "BUTLER", "Other"))))))),
         avq_hours = round(actual_hours/budgeting_hours,2),
         corp = c("BBNA")) %>%
  select(corp,
         brand,
         avq_hours,
         project_name,
         order_number,
         proj_num,
         project_manager,
         #builder_name,
         actual_hours,
         budgeting_hours) %>%
  filter(brand == "BUTLER" |
           brand == "VP",
         !is.nan(avq_hours),
         !is.infinite(avq_hours)) %>%
  left_join(eng_complexity_lookup, by = c("proj_num" = "proj_num")) %>%
  group_by(brand, complexity) %>%
  mutate(pct_hours = round(actual_hours/sum(actual_hours),2),
         five_pct = if_else(pct_hours >= 0.05, "Yes", "No")) %>%
  ungroup() %>%
  filter(!is.na(complexity))                                             # removing NA's but need to review in future months (only one in January when developing)

new_oswt$complexity <- factor(new_oswt$complexity,
                              levels = c("CII/FT",
                                         "Simple (<=100)",
                                         "Moderate (100-200)",
                                         "Complex (200-400)",
                                         "High Complex (400-800)",
                                         "Very High Complex (>800)"))

new_oswt$complexity <- fct_recode(new_oswt$complexity,
                                  "Simple" = "Simple (<=100)",
                                  "Moderate" = "Moderate (100-200)",
                                  "Complex" = "Complex (200-400)",
                                  "High Complexity" = "High Complex (400-800)",
                                  "Very High Complexity" = "Very High Complex (>800)")

new_oswt <- new_oswt %>%
  mutate(over_hours = if_else(complexity == "CII/FT" & actual_hours > 40, "Yes",
                              if_else(complexity == "Simple" & actual_hours > 100, "Yes",
                                      if_else(complexity == "Moderate" & actual_hours > 200, "Yes",
                                              if_else(complexity == "Complex" & actual_hours > 400, "Yes",
                                                      if_else(complexity == "High Complexity" & actual_hours > 800, "Yes",
                                                              if_else(complexity == "Very High Complexity" & actual_hours > 1000, "Yes", "No")))))))

butler_avg <- new_oswt %>%
  group_by(brand) %>%
  summarise(
    brand_mean = round(mean(avq_hours),2),
    brand_median = round(median(avq_hours),2)
  ) %>%
  filter(brand == "BUTLER")
vp_avg <- new_oswt %>%
  group_by(brand) %>%
  summarise(
    brand_mean = round(mean(avq_hours),2),
    brand_median = round(median(avq_hours),2)
  ) %>%
  filter(brand == "VP")

butler_avq <- new_oswt %>%
  filter(brand == "BUTLER") %>%
  ggplot(aes(corp, avq_hours, color = over_hours)) +
  geom_point(alpha = 0.4) +
  coord_flip() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  labs(x = "",
       y = "",
       subtitle = paste0("Butler Jobs ECK'd in ", month)) +
  ggtitle("Actual vs Quoted Engineering Hours") +
  scale_y_continuous(limits = c(0,6)) +
  geom_hline(data = butler_avg, aes(yintercept = brand_mean))
butler_avq

vp_avq <- new_oswt %>%
  filter(brand == "VP") %>%
  ggplot(aes(corp, avq_hours, color = over_hours)) +
  geom_point(alpha = 0.4) +
  coord_flip() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  labs(x = "",
       y = "",
       subtitle = paste0("VP Jobs ECK'd in ", month),
       caption = "green dots represent jobs over complexity ceiling hours") +
  #ggtitle("Actual vs Quoted Engineering Hours") +
  scale_y_continuous(limits = c(0,6)) +
  geom_hline(data = vp_avg, aes(yintercept = brand_mean))
vp_avq

brand_all_avq <- butler_avq / vp_avq
brand_all_avq

#by PM------------------------------------------------

vp_pm_db <- new_oswt %>%
  filter(brand == "VP") %>%
  count(project_manager, sort = TRUE)
vp_pm_1 <- vp_pm_db$project_manager[1:8] 
vp_pm_2 <- vp_pm_db$project_manager[9:16]
vp_pm_3 <- vp_pm_db$project_manager[17:24]
vp_pm_4 <- vp_pm_db$project_manager[25:32]
vp_pm_5 <- vp_pm_db$project_manager[33:40]
vp_pm_6 <- vp_pm_db$project_manager[41:46]

vp_pm_avg_1 <- new_oswt %>%
  filter(project_manager %in% vp_pm_1) %>%
  group_by(project_manager) %>%
  summarise(
    pm_mean = round(mean(avq_hours),2),
    pm_median = round(median(avq_hours),2)
  )
vp_pm_avg_2 <- new_oswt %>%
  filter(project_manager %in% vp_pm_2) %>%
  group_by(project_manager) %>%
  summarise(
    pm_mean = round(mean(avq_hours),2),
    pm_median = round(median(avq_hours),2)
  )
vp_pm_avg_3 <- new_oswt %>%
  filter(project_manager %in% vp_pm_3) %>%
  group_by(project_manager) %>%
  summarise(
    pm_mean = round(mean(avq_hours),2),
    pm_median = round(median(avq_hours),2)
  )
vp_pm_avg_4 <- new_oswt %>%
  filter(project_manager %in% vp_pm_4) %>%
  group_by(project_manager) %>%
  summarise(
    pm_mean = round(mean(avq_hours),2),
    pm_median = round(median(avq_hours),2)
  )
vp_pm_avg_5 <- new_oswt %>%
  filter(project_manager %in% vp_pm_5) %>%
  group_by(project_manager) %>%
  summarise(
    pm_mean = round(mean(avq_hours),2),
    pm_median = round(median(avq_hours),2)
  )
vp_pm_avg_6 <- new_oswt %>%
  filter(project_manager %in% vp_pm_6) %>%
  group_by(project_manager) %>%
  summarise(
    pm_mean = round(mean(avq_hours),2),
    pm_median = round(median(avq_hours),2)
  )

vp_pm_graph_1 <- new_oswt %>%
  filter(project_manager %in% vp_pm_1) %>%
  ggplot(aes(corp, avq_hours, color = over_hours)) +
  geom_point(alpha = 0.4) +
  coord_flip() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  labs(x = "",
       y = "",
       subtitle = "by PM",
       caption = "green dots represent jobs over complexity ceiling hours") +
  ggtitle("Actual vs Quoted Engineering Hours") +
  scale_y_continuous(limits = c(0,6)) +
  facet_grid(rows = vars(project_manager)) +
  geom_hline(data = vp_pm_avg_1, aes(yintercept = pm_mean))
vp_pm_graph_1

vp_pm_graph_2 <- new_oswt %>%
  filter(project_manager %in% vp_pm_2) %>%
  ggplot(aes(corp, avq_hours, color = over_hours)) +
  geom_point(alpha = 0.4) +
  coord_flip() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  labs(x = "",
       y = "",
       subtitle = "by PM",
       caption = "green dots represent jobs over complexity ceiling hours") +
  ggtitle("Actual vs Quoted Engineering Hours") +
  scale_y_continuous(limits = c(0,6)) +
  facet_grid(rows = vars(project_manager)) +
  geom_hline(data = vp_pm_avg_2, aes(yintercept = pm_mean))
vp_pm_graph_2

vp_pm_graph_3 <- new_oswt %>%
  filter(project_manager %in% vp_pm_3) %>%
  ggplot(aes(corp, avq_hours, color = over_hours)) +
  geom_point(alpha = 0.4) +
  coord_flip() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  labs(x = "",
       y = "",
       subtitle = "by PM",
       caption = "green dots represent jobs over complexity ceiling hours") +
  ggtitle("Actual vs Quoted Engineering Hours") +
  scale_y_continuous(limits = c(0,6)) +
  facet_grid(rows = vars(project_manager)) +
  geom_hline(data = vp_pm_avg_3, aes(yintercept = pm_mean))
vp_pm_graph_3

vp_pm_graph_4 <- new_oswt %>%
  filter(project_manager %in% vp_pm_4) %>%
  ggplot(aes(corp, avq_hours, color = over_hours)) +
  geom_point(alpha = 0.4) +
  coord_flip() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  labs(x = "",
       y = "",
       subtitle = "by PM",
       caption = "green dots represent jobs over complexity ceiling hours") +
  ggtitle("Actual vs Quoted Engineering Hours") +
  scale_y_continuous(limits = c(0,6)) +
  facet_grid(rows = vars(project_manager)) +
  geom_hline(data = vp_pm_avg_4, aes(yintercept = pm_mean))
vp_pm_graph_4

vp_pm_graph_5 <- new_oswt %>%
  filter(project_manager %in% vp_pm_5) %>%
  ggplot(aes(corp, avq_hours, color = over_hours)) +
  geom_point(alpha = 0.4) +
  coord_flip() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  labs(x = "",
       y = "",
       subtitle = "by PM",
       caption = "green dots represent jobs over complexity ceiling hours") +
  ggtitle("Actual vs Quoted Engineering Hours") +
  scale_y_continuous(limits = c(0,6)) +
  facet_grid(rows = vars(project_manager)) +
  geom_hline(data = vp_pm_avg_5, aes(yintercept = pm_mean))
vp_pm_graph_5

vp_pm_graph_6 <- new_oswt %>%
  filter(project_manager %in% vp_pm_6) %>%
  ggplot(aes(corp, avq_hours, color = over_hours)) +
  geom_point(alpha = 0.4) +
  coord_flip() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  labs(x = "",
       y = "",
       subtitle = "by PM",
       caption = "green dots represent jobs over complexity ceiling hours") +
  ggtitle("Actual vs Quoted Engineering Hours") +
  scale_y_continuous(limits = c(0,6)) +
  facet_grid(rows = vars(project_manager)) +
  geom_hline(data = vp_pm_avg_6, aes(yintercept = pm_mean))
vp_pm_graph_6


pm_avg <- new_oswt %>%
  group_by(brand, project_manager) %>%
  summarise(
    pm_mean = round(mean(avq_hours),2),
    pm_median = round(median(avq_hours),2),
    count = n(),
  ) %>%
  arrange(desc(pm_mean))

write_xlsx(pm_avg, "~/R/R Data/Engineering/MBR Charts/Charts/pm_avg.xlsx")

#AvQ by Region---------------------------------------------

butler_region_mean <- new_oswt %>%
  group_by(brand, region) %>%
  summarise(region_mean = round(mean(avq_hours),2),
            region_median = round(median(avq_hours),2)) %>%
  filter(brand == "BUTLER")

vp_region_mean <- new_oswt %>%
  group_by(brand, region) %>%
  summarise(region_mean = round(mean(avq_hours),2),
            region_median = round(median(avq_hours),2)) %>%
  filter(brand == "VP")

butler_region_avq <- new_oswt %>%
  filter(brand == "BUTLER") %>%
  ggplot(aes(corp, avq_hours, fill = over_hours)) +
  geom_violin() +
  coord_flip() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  labs(x = "",
       y = "",
       subtitle = paste0("Butler Jobs ECK'd in ", month)) +
  ggtitle("Actual vs Quoted Engineering Hours") +
  facet_grid(rows = vars(region)) +
  scale_y_continuous(limits = c(0,7.5)) +
  geom_hline(data = butler_region_mean, aes(yintercept = region_median)) +
  geom_hline(data = butler_region_mean, aes(yintercept = region_mean), color = "green")
butler_region_avq
  
