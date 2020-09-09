library(lubridate)
library(janitor)
library(tidyverse)
library(readxl)

theme_set(theme_minimal())

detailing_1 <- c("B1 - 0 to 100 hrs total", 
               "B2 - 0 to 100 hrs total", 
               "C1 - 100 to 200 hrs total",
               "D1 - 200 to 400 hrs total",
               "E1 - 400 to 800 hrs total")

detailing_2 <- c("F1 - 800+ hours total",
                 "F2 - 800+ hours total",
                 "F3 - 800+ hours total",
                 "G1 - 400 hrs+ & 6+Mhr/Ton",
                 "R - Roof")


design <- c("DVS - 0 to 100 hrs total",
            "DS - 100 to 200 hrs total",
            "DM - 201 to 400 hrs total",
            "DC - 401 to 800 hrs total",
            "DHC - 800+ hrs total")

design_label <- c("DVS - 0 to 100 hrs total" = "Simple",
            "DS - 100 to 200 hrs total" = "Moderate",
            "DM - 201 to 400 hrs total" = "Complex",
            "DC - 401 to 800 hrs total" = "High\n Complexity",
            "DHC - 800+ hrs total" = "Very\n High\n Complexity")

detail_label_1 <- c("B1 - 0 to 100 hrs total" = "Simple\n (B1)", 
                  "B2 - 0 to 100 hrs total" = "Simple\n (B2)", 
                  "C1 - 100 to 200 hrs total" = "Moderate",
                  "D1 - 200 to 400 hrs total" = "Complex",
                  "E1 - 400 to 800 hrs total" = "High\n Complexity")

detail_label_2 <- c("F1 - 800+ hours total" = "Very\n High\n Complexity (F1)",
                    "F2 - 800+ hours total" = "Very\n High\n Complexity (F2)",
                    "F3 - 800+ hours total" = "Very\n High\n Complexity (F3)",
                    "G1 - 400 hrs+ & 6+Mhr/Ton" = ">400 Hrs\n 6+ hr/Ton",
                    "R - Roof" = "Roof")


engineering_time_detail <- read_excel("Utilization/engineering_time_detail.xlsx") %>%
  clean_names()

employee_cell <- read_excel("Utilization/employee_cell.xlsx") %>%
  clean_names()

employee_cell <- employee_cell %>%
  filter(!is.na(emp_id)) %>%
  select(emp_id,
         resource_cell)

training_time <- engineering_time_detail %>%
    filter(str_detect(project_number, "OVH"),
           task == "TRAIN") %>%
  mutate(expend_date = mdy(expenditure_ending_date)) %>%
  left_join(employee_cell, by = c("employee_number" = "emp_id"))

#####################################################################################
## DESIGN ##

design_training <- training_time %>%
  filter(resource_cell %in% design) %>%
  mutate(
    ordered_cell = factor(resource_cell, 
                          levels = c("DVS - 0 to 100 hrs total",
                                     "DS - 100 to 200 hrs total",
                                     "DM - 201 to 400 hrs total",
                                     "DC - 401 to 800 hrs total",
                                     "DHC - 800+ hrs total")))

design_graph <- design_training %>%
  group_by(ordered_cell, expend_date) %>%
  summarise(hours = sum(actual_hours)) %>%
  ggplot(aes(expend_date, hours)) +
  geom_bar(stat = "identity", fill = "#c563fd") + 
  facet_grid(
    rows = vars(ordered_cell),
    labeller = labeller(ordered_cell = design_label)) +
  labs(
    y = "Hours",
    x = "",
    title = "Design Training Hours",
    subtitle = "Training hours by week",
    caption = "Source: ADP via BI query\n Complexity cells taken from Saviom and changed to match traditional complexity reporting\n (e.g. DVS = 'Simple', DS = 'Moderate', etc)"
  ) +
  theme(
    strip.background = element_rect(fill = "#e0aaff"),
    strip.text = element_text(face = "bold", size = 15),
    axis.text = element_text(size = 13)
  )

design_graph

############################################################################
## DETAILING ##


detail_training_1 <- training_time %>%
  filter(resource_cell %in% detailing_1) %>%
  mutate(
    ordered_cell = factor(resource_cell, 
                          levels = c("B1 - 0 to 100 hrs total", 
                                     "B2 - 0 to 100 hrs total", 
                                     "C1 - 100 to 200 hrs total",
                                     "D1 - 200 to 400 hrs total",
                                     "E1 - 400 to 800 hrs total")))

detail_training_2 <- training_time %>%
  filter(resource_cell %in% detailing_2) %>%
  mutate(
    ordered_cell = factor(resource_cell, 
                          levels = c("F1 - 800+ hours total",
                                     "F2 - 800+ hours total",
                                     "F3 - 800+ hours total",
                                     "G1 - 400 hrs+ & 6+Mhr/Ton",
                                     "R - Roof")))

detail_graph_1 <- detail_training_1 %>%
  group_by(ordered_cell, expend_date) %>%
  summarise(hours = sum(actual_hours)) %>%
  ggplot(aes(expend_date, hours)) +
  geom_bar(stat = "identity", fill = "#ee68e2") + 
  facet_grid(
    rows = vars(ordered_cell),
    labeller = labeller(ordered_cell = detail_label_1)) +
  labs(
    y = "Hours",
    x = "",
    title = "Engineering Tech Training Hours",
    subtitle = "Training hours by week",
    caption = "Source: ADP via BI query\n Complexity cells taken from Saviom and changed to match traditional complexity reporting for cells B though F\n G cell and Roof do not align w/ traditional complexity grouping"
  ) +
  theme(
    strip.background = element_rect(fill = "#ee68e2"),
    strip.text = element_text(face = "bold", size = 15),
    axis.text = element_text(size = 13)
  )

detail_graph_2 <- detail_training_2 %>%
  group_by(ordered_cell, expend_date) %>%
  summarise(hours = sum(actual_hours)) %>%
  ggplot(aes(expend_date, hours)) +
  geom_bar(stat = "identity", fill = "#ee68e2") + 
  facet_grid(
    rows = vars(ordered_cell),
    labeller = labeller(ordered_cell = detail_label_2)) +
  labs(
    y = "Hours",
    x = "",
    title = "Engineering Tech Training Hours",
    subtitle = "Training hours by week",
    caption = "Source: ADP via BI query\n Complexity cells taken from Saviom and changed to match traditional complexity reporting for cells B though F\n G cell and Roof do not align w/ traditional complexity grouping"
  ) +
  theme(
    strip.background = element_rect(fill = "#f9a6f1"),
    strip.text = element_text(face = "bold", size = 15),
    axis.text = element_text(size = 13)
  )

detail_graph_1
detail_graph_2


############################################################################
## LET'S TAKE A LOOK AT THE NA'S

na_training <- training_time %>%
  filter(is.na(resource_cell))

na_graph <- na_training %>%
  group_by(resource_cell, expend_date) %>%
  summarise(hours = sum(actual_hours)) %>%
  ggplot(aes(expend_date, hours)) +
  geom_bar(stat = "identity", fill = "#b8be0c") + 
  facet_grid(
    rows = vars(resource_cell)) +
  labs(
    y = "Hours",
    x = "",
    title = "Ex-Employee/Team Lead Training Hours",
    subtitle = "Training hours by week",
    caption = "Source: ADP via BI query\n These employees did not return a complexity cell so it's presumed that they are either no longer employed by BBNA or are team leads"
  ) +
  theme(
    strip.background = element_rect(fill = "#fcff9f"),
    strip.text = element_text(face = "bold", size = 15),
    axis.text = element_text(size = 13)
  )

na_graph

############################################################################
## SAVE THE GRAPHS

ggsave("~/R/Git/Project/Bluescope-Engineering/Utilization/graphs/design_training.png", design_graph, width = 16, height = 9, dpi = 320)
ggsave("~/R/Git/Project/Bluescope-Engineering/Utilization/graphs/detailing_training_1.png", detail_graph_1, width = 16, height = 9, dpi = 320)
ggsave("~/R/Git/Project/Bluescope-Engineering/Utilization/graphs/detailing_training_2.png", detail_graph_2, width = 16, height = 9, dpi = 320)
ggsave("~/R/Git/Project/Bluescope-Engineering/Utilization/graphs/ex_employee_training.png", na_graph, width = 16, height = 9, dpi = 320)