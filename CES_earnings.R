setwd("/Users/iregmii/Documents/Data Projects/JOBS 2023")
#### JOBS #### 
#install.packages("RColorBrewer")
library(RColorBrewer)
library(hrbrthemes)
library(janitor)
library(tidyverse)
library(ggtext)
library(ggrepel)
library(huxtable)
library(scales)
library(lubridate)
library(tidytext)
library(viridis)
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(ggrepel)

theme_inflation <- theme_classic() +
  theme(text = element_text(family = "Larsseit", face="bold", color ="#1E8456"),
        plot.title = element_text(family = "Larsseit"),
        plot.subtitle = element_text(family = "Larsseit"),
        plot.caption = element_text(family="Larsseit"),
        strip.text = element_text(family="Larsseit")) +
  theme(panel.grid.major.y = element_line(size=0.5),
        plot.title = element_text(size = 15, face="bold"),
        plot.subtitle = element_text(size = 10),
        plot.title.position = "plot",
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        legend.title = element_blank())
#############
categories_view <- ces_data %>% 
  filter(seasonal == "S") %>% 
  distinct(series_title, series_id)

########
pick_categories <- c("Average hourly earnings of all employees, total private, seasonally adjusted")
start_date <- "2019-01-01"
CES1 <- ces_data %>% 
  filter(series_title %in% pick_categories) %>%
  filter(date >= start_date) %>%
  mutate(wage_growth = value/lag(value,3) - 1) %>%
  mutate(num_label = round(wage_growth*100,1)) 
  

 
  Fig1_CES <- ggplot(CES1, aes(x = date, y = wage_growth)) + 
  geom_bar(stat = 'identity', fill = "#6EA4BF")  +  
  geom_point() + 
  geom_line(color="#2D779C", size = 1) + 
  theme_inflation + theme(legend.position = "none") +
  scale_y_continuous(labels = scales::percent) + 
  scale_x_date(date_labels = "%b-%y", breaks = "4 month") +
  #geom_text(aes(label = num_label), check_overlap = TRUE) +
  geom_text(data = CES1, aes(x = date, y = wage_growth, label = num_label), nudge_y = 0.003, size=2, color="#1E8456") +
  labs(y = NULL,
       x = NULL,
       title = "Average Hourly Earnings, Three-Month Percent Change",
       subtitle = "All employees, total private, seasonally adjusted", 
       caption ="BLS, CES. Seasonally adjusted. Ira Regmi. Author's Analysis.")
   

ggsave("CES_1.png", dpi="retina", width = 8.5, height=4.25, units = "in")

#####
category_3_2 <- c("Average hourly earnings of production and nonsupervisory employees, retail trade, seasonally adjusted",
                  "Average hourly earnings of production and nonsupervisory employees, leisure and hospitality, seasonally adjusted", 
                  "Average hourly earnings of all employees, total private, seasonally adjusted")

# CALCULATE AVERAGE PRE VALUE (WE ARE CLOSE TO JUSTIN'S VALUE)
pre_value <- ces_data %>% filter(series_title == "Average hourly earnings of production and nonsupervisory employees, retail trade, seasonally adjusted") %>%
  select(date, series_title, year, wages = value, supersector_name) %>%
  mutate(wage_growth = wages/lag(wages,3) - 1) %>%
  filter(year == 2018 | year == 2019) %>%
  summarize(avg_pre = mean(wage_growth))
pre_value <- as.numeric(pre_value)

pre_value <- ces_data %>% filter(series_title == "Average hourly earnings of production and nonsupervisory employees, total private, seasonally adjusted") %>%
  select(date, series_title, year, wages = value, supersector_name) %>%
  mutate(wage_growth = pre_value, supersector_name = "Average wage growth in 2018-2019")

ces_data %>% filter(series_title %in% category_3_2) %>%
  select(date, series_title, year, wages = value, supersector_name) %>%
  mutate(wage_growth = wages/lag(wages,3) - 1) %>%
  rbind(pre_value) %>%
  filter(date >= "2021-01-01") %>%
  ggplot(aes(date, wage_growth, color=supersector_name)) + geom_line(size=1.2) +
  theme_inflation +
  labs(y = NULL, x = NULL, title="Average Hourly Earnings, Three-Month Percent Change",
       subtitle="Production and nonsupervisory employees, seasonally adjusted",
       caption = "Source: Bureau of Labor Statistics, Authors' Analysis.") +
  scale_x_date(date_labels = "%b %y", breaks="4 months") +
  scale_y_continuous(labels = percent) +
  theme(legend.position = "bottom")

ggsave("graphics/Figure_3_2.png", dpi="retina", width = 8.5, height=4.25, units = "in")

