rm(list = ls(all.names = TRUE))

library(tidyverse)
library(janitor)
library(readxl)
library(writexl)

eng_budget_actual_expenditures <- read_excel("~/R/Git/Project/Bluescope-Engineering/AvQ/eng_budget_actual_20191230_20200126_expenditures.xlsx", 
                                                               col_types = c("text", "text", "text", 
                                                                             "numeric", "text", "text", "text", 
                                                                             "text", "text", "numeric", "numeric", 
                                                                             "numeric", "numeric", "numeric", 
                                                                             "numeric", "numeric", "numeric", 
                                                                             "numeric", "numeric", "numeric", 
                                                                             "numeric", "numeric", "numeric", 
                                                                             "numeric", "numeric", "numeric", 
                                                                             "numeric", "numeric", "numeric", 
                                                                             "numeric", "numeric", "numeric", 
                                                                             "numeric", "numeric", "numeric")) %>%
  clean_names("snake") %>%
  mutate(proj_num = str_sub(project_no, 1,8))

budget_task <- eng_budget_actual_expenditures %>%
  pivot_longer(cols = ends_with("_budget"),
               names_to = "task",
               values_to = "budget") %>%
  replace_na(list(
    budget = 0)) %>%
  mutate(task_num = str_sub(task,1,3)) %>%
  select(proj_num,
         project_no,
         task_num,
         budget)

tidy_eng_compare <- eng_budget_actual_expenditures %>%
  pivot_longer(cols = ends_with("_actual"),
               names_to = "task",
               values_to = "actual") %>%
  replace_na(list(
    actual = 0)) %>%
  mutate(task_num = str_sub(task,1,3)) %>%
  select(division,
         region,
         proj_num,
         project_no,
         customer_name,
         customer_number,
         project_name,
         task_num,
         actual) %>%
  left_join(budget_task, by = c("project_no" = "project_no", "task_num" = "task_num")) %>%
  replace_na(list(
    budget = 0)) %>%
  select(-proj_num.y) %>%
  rename("proj_num" = "proj_num.x")

tidy_project_compare <- tidy_eng_compare %>%
  group_by(division, region, proj_num, customer_name, customer_number, task_num) %>%
  summarise(budget_hrs = sum(budget),
            actual_hrs = sum(actual)) %>%
  mutate(act_quote_variance = actual_hrs - budget_hrs)

#write_xlsx(tidy_project_compare, "~/R/R Data/Engineering/Monthly Metrics/avq_testing/project_compare.xlsx")

# TOTAL HOURS BY PROJECT----------------------------------

project_totals <- tidy_eng_compare %>%
  group_by(division, region, proj_num) %>%
  summarise(budget_hrs = sum(budget),
            actual_hrs = sum(actual))

# filter for just clarification, design and detailing, then create variance variables to analyze 
task_specific_look <- tidy_project_compare %>%
  filter(task_num == "ecd" |
         task_num == "eds" |
         task_num == "edt")
test_pivot <- task_specific_look %>%
  pivot_wider(names_from = task_num,
              values_from = c(actual_hrs, budget_hrs, act_quote_variance))

# initial plots to see if we have any strong correlation
# first plot is to see if clarification variance could help explain design variance
    # The engineering workflow goes clarification -> design -> detail
    # So my first question is: does favorable/unfavorable variance affect the next step in our engineering workflow?

plot_1 <- test_pivot %>%
  ggplot(aes(act_quote_variance_ecd, act_quote_variance_eds)) +
  geom_point()

# next plot is design to detailing
plot_2 <- test_pivot %>%
  ggplot(aes(act_quote_variance_eds, act_quote_variance_edt)) +
  geom_point()

# final plot is to look at any relationship between clarification (1st step) and detailing (final step)
plot_3 <- test_pivot %>%
  ggplot(aes(act_quote_variance_ecd, act_quote_variance_edt)) +
  geom_point()

cor(test_pivot$act_quote_variance_ecd,test_pivot$act_quote_variance_eds)
cor(test_pivot$act_quote_variance_eds,test_pivot$act_quote_variance_edt)

# not a lot of correlation between these variables, so maybe my eyes deceive me. It does look like there
# is a relationship here. Luckily I understand the data pretty well and know that there are some issues.
    # There are several LARGE projects that do not input budget hours but do charge hours to those tasks
    # The final step after detailing is to ECK, which we charge actual hours to this task but do not budget. Based on my conversations with engineering managers, ECK hours could be added to detailing time. I'll need to reformat the data to get this done


test_pivot <- test_pivot %>%
  mutate(
    clar_error = if_else(budget_hrs_ecd == 0 & actual_hrs_ecd > 0, "Exclude", "Include")
  )
test_pivot %>%
  filter(clar_error == "Include",
         act_quote_variance_ecd == 0) %>%
  View()

test_pivot %>%
  filter(clar_error == "Include",
         act_quote_variance_ecd < 50 &
         act_quote_variance_ecd > -50) %>%
  ggplot(aes(act_quote_variance_ecd)) +
  geom_histogram(binwidth = 5.0)
#ggsave("~R/R Data/Enigneering/Monthly Metrics/avq_testing/plot_1.png", plot_1, width = 16, height = 9, dpi = "retina")
#ggsave("~R/R Data/Enigneering/Monthly Metrics/avq_testing/plot_2.png", plot_2, width = 16, height = 9, dpi = "retina")
