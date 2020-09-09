library(lubridate)
library(janitor)

backlog_with_dates <- read_excel("C:/Users/felpj/Downloads/backlog_with_dates.xlsx") %>%
  clean_names()

modified_backlog <- backlog_with_dates %>%
  mutate(backlog_date = as.Date(backlog_date, "%m/%d/%Y"),
         backlog_month = floor_date(backlog_date, "month"))
writexl::write_xlsx(modified_backlog, "~/R/R Data/backlog_with_backlog_month.xlsx")


# using the BI query for all charges -----------------------------------------
engineering_historical <- read_excel("Utilization/engineering_historical.xlsx", 
                                     sheet = "Sheet1") %>%
  clean_names()
engineering_historical <- engineering_historical %>%
  mutate(expenditure_ending_date = mdy(expenditure_ending_date))

overhead_tasks_grouped <- engineering_historical %>%
  filter(str_detect(order_number, "OVH")) %>%
  group_by(expenditure_ending_date, task) %>%
  summarise(approved_hours = sum(approved_hours),
            unapproved_hours = sum(unapproved_hours),
            total_hours = sum(actual_hours))


training <- overhead_tasks_grouped %>%
  filter(task == "TRAIN") %>%
  ggplot(aes(expenditure_ending_date, total_hours)) +
  geom_bar(stat = "identity", position = "stack") +
  ggtitle("Training Time per Week")
training

ggsave("Utilization/training_time.png", training, width = 16, height = 9, dpi = 320)
