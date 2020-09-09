#rm(list = ls(all.names = TRUE))
library(tidyverse)
library(readxl)
library(glue)

h1 <- c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
h2 <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun")

month <- format(Sys.Date() - months(1), "%b")

period <- str_to_upper(format(Sys.Date() - months(1), "%b-%y"))

fiscal_year <- if_else(
  month %in% h1, 
  paste0("FY", format(Sys.Date() + 365, "%y")), 
  paste0("FY", format(Sys.Date(), "%y")))

theme_set(theme_light())

gender_diversity <- read_excel("~/R/R Data/Engineering/MBR Charts/gender_diversity.xlsx", 
                               col_types = c("text", "numeric", "text", "text", 
                                             "text", "text", "numeric")) %>% 
  mutate(month_year = paste0({Month}, "-", {Year}))

gender_diversity$month_year <- factor(gender_diversity$month_year, levels = 
                                   c("Jul-2019",
                                     "Aug-2019",
                                     "Sep-2019",
                                     "Oct-2019",
                                     "Nov-2019",
                                     "Dec-2019",
                                     "Jan-2020",
                                     "Feb-2020",
                                     "Mar-2020",
                                     "Apr-2020",
                                     "May-2020",
                                     "Jun-2020",
                                     "Jul-2020",
                                     "Aug-2020",
                                     "Sep-2020",
                                     "Oct-2020",
                                     "Nov-2020",
                                     "Dec-2020",
                                     "Jan-2021",
                                     "Feb-2021",
                                     "Mar-2021",
                                     "Apr-2021",
                                     "May-2021",
                                     "Jun-2021"))

gender_diversity$Role <- factor(gender_diversity$Role, levels = 
                                   c("Designer",
                                     "Technician",
                                     "PE"))

gender_diversity$Role <- factor(gender_diversity$Role, levels = 
                                  c("Designer",
                                    "Technician",
                                    "PE"))

gender_diversity$`Fiscal Year` <- factor(gender_diversity$`Fiscal Year`, levels = 
                                  c("FY20",
                                    "FY21"))

gender_chart <- gender_diversity %>%
  filter(Pct > 0,
         Gender == "Female") %>%
  ggplot(aes(month_year, Pct, fill = Role)) +
  geom_bar(stat = "Identity", position = "dodge", alpha = 0.75) +
  scale_fill_manual(values = c("#7bdaea", "#1146b6", "#7ba3b4")) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), labels = scales::percent) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        panel.border = element_blank(),
        panel.grid.major.y = element_line(color = "darkgray"),
        panel.grid.minor.y = element_line(color = "darkgray"),
        panel.grid.major.x = element_blank(),
        axis.text = element_text(size = 12)) +
  labs(x = "",
       y = "% Female")
gender_chart

ggsave("~/R/R Data/Engineering/MBR Charts/Charts/gender_diversity.png", gender_chart, width = 16, height = 9, dpi = "retina")
