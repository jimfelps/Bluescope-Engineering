rm(list = ls(all.names = TRUE))
library(tidyverse)
library(readxl)
library(janitor)
library(patchwork)
library(writexl)
library(ggbeeswarm)

month <- "December 2019"
labels_avq <- c(
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

Engineering_Metrics_Dec19 <- read_excel("~/R/R Data/Engineering/MBR Charts/Engineering Metrics Dec19.xlsx", 
                                        sheet = "Compass Order Summary", col_types = c("text", 
                                                                                       "text", "text", "text", "text", "text", 
                                                                                       "numeric", "numeric", "numeric", 
                                                                                       "text", "text", "text", "text", "text", 
                                                                                       "text", "text", "numeric", "numeric", 
                                                                                       "numeric", "numeric", "numeric", 
                                                                                       "numeric", "numeric", "numeric", 
                                                                                       "numeric", "numeric", "numeric", 
                                                                                       "numeric", "numeric", "numeric", 
                                                                                       "numeric", "numeric", "numeric", 
                                                                                       "numeric", "numeric", "numeric", 
                                                                                       "numeric", "text", "numeric", "numeric", 
                                                                                       "numeric", "numeric", "text", "numeric", 
                                                                                       "numeric", "numeric", "numeric", 
                                                                                       "text", "numeric", "numeric", "numeric", 
                                                                                       "numeric", "text", "numeric", "numeric", 
                                                                                       "numeric", "numeric", "text", "numeric", 
                                                                                       "numeric", "numeric", "numeric", 
                                                                                       "text", "numeric", "numeric", "numeric", 
                                                                                       "numeric", "text", "numeric", "numeric", 
                                                                                       "numeric", "numeric", "text", "numeric", 
                                                                                       "numeric", "numeric", "numeric", 
                                                                                       "text", "numeric", "numeric", "numeric", 
                                                                                       "numeric", "text", "numeric", "numeric", 
                                                                                       "numeric", "numeric", "text", "numeric", 
                                                                                       "numeric", "numeric", "numeric", 
                                                                                       "numeric", "numeric", "numeric", 
                                                                                       "numeric", "numeric", "text", "numeric", 
                                                                                       "numeric", "numeric", "numeric", 
                                                                                       "text", "numeric", "numeric", "numeric", 
                                                                                       "numeric", "text", "numeric", "numeric", 
                                                                                       "numeric", "numeric", "text", "text"), 
                                        skip = 41) %>%
  clean_names("snake") %>%
  mutate(proj_num = str_sub(order_number, 1,8))

new_oswt <- Engineering_Metrics_Dec19 %>%
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
butler_complex_avg <- new_oswt %>%
  group_by(brand, complexity) %>%
  summarise(
    complex_mean = round(mean(avq_hours),2),
    complex_median = round(median(avq_hours),2)
  ) %>%
  filter(brand == "BUTLER")
vp_complex_avg <- new_oswt %>%
  group_by(brand, complexity) %>%
  summarise(
    complex_mean = round(mean(avq_hours),2),
    complex_median = round(median(avq_hours),2)
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
  scale_y_continuous(limits = c(0,4.5), breaks = c(0.5,1,1.5,2,2.5,3,3.5,4)) +
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
  scale_y_continuous(limits = c(0,4.5), breaks = c(0.5,1,1.5,2,2.5,3,3.5,4)) +
  geom_hline(data = vp_avg, aes(yintercept = brand_mean))
vp_avq

butler_complex <- new_oswt %>%
  filter(brand == "BUTLER") %>%
  ggplot(aes(brand, avq_hours, color = over_hours)) +
  geom_jitter(alpha = 0.5) +
  coord_flip() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  labs(x = "",
       y = "",
       subtitle = "Butler") +
  #ggtitle("Actual vs Quoted Engineering Hours by Complexity") +
  scale_y_continuous(limits = c(0,4.5), breaks = c(0.5,1,1.5,2,2.5,3,3.5,4)) +
  scale_color_manual(values = c("#000000", "#e63505")) +
  facet_grid(rows = vars(complexity), labeller = labeller(complexity = labels_avq)) +
  geom_hline(data = butler_complex_avg, aes(yintercept = complex_mean))
butler_complex

vp_complex <- new_oswt %>%
  filter(brand == "VP") %>%
  ggplot(aes(brand, avq_hours, color = over_hours)) +
  geom_jitter(alpha = 0.5) +
  coord_flip() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  labs(x = "",
       y = "",
       subtitle = "VP ",
       caption = "orange dots represent jobs over complexity ceiling hours.\nSize of dot is relative proportion of hours within complexity.") +
  #ggtitle("Actual vs Quoted Engineering Hours by Complexity") +
  scale_y_continuous(limits = c(0,4.5), breaks = c(0.5,1,1.5,2,2.5,3,3.5,4)) +
  scale_color_manual(values = c("#000000", "#e63505")) +
  facet_grid(rows = vars(complexity), labeller = labeller(complexity = labels_avq)) +
  geom_hline(data = vp_complex_avg, aes(yintercept = complex_mean))
vp_complex

brand_all_avq <- butler_avq / vp_avq
brand_all_avq

brand_by_complex <- butler_complex | vp_complex
brand_by_complex

brand_dot <- new_oswt %>%
  ggplot(aes(budgeting_hours, actual_hours)) +
  geom_point(size = 4, alpha = 0.5) +
  geom_abline(slope = 1, intercept = 1) +
  scale_y_continuous(limits = c(0,2500)) +
  scale_x_continuous(limits = c(0,1500)) +
  labs(x = "Budget Hours",
       y = "Actual Hours") +
  facet_grid(cols = vars(brand)) +
  scale_color_manual(values = c("#000000", "#e63505"))

brand_dot

ggsave("~/R/R Data/Engineering/MBR Charts/Charts/brand_all_avq.png", brand_all_avq, width = 16, height = 9, dpi = "retina")
ggsave("~/R/R Data/Engineering/MBR Charts/Charts/brand_complex_avq.png", brand_by_complex, width = 16, height = 9, dpi = "retina")
ggsave("~/R/R Data/Engineering/MBR Charts/Charts/brand_dot.png", brand_dot, width = 16, height = 9, dpi = "retina")

butler_table <- new_oswt %>%
  filter(brand == "BUTLER") %>%
  arrange(desc(complexity, brand, avq_hours))
vp_table <- new_oswt %>%
  filter(brand == "VP") %>%
  arrange(desc(complexity, brand, avq_hours))

write_xlsx(butler_table, "~/R/R Data/Engineering/MBR Charts/Charts/butler_table.xlsx")
write_xlsx(vp_table, "~/R/R Data/Engineering/MBR Charts/Charts/vp_table.xlsx")
