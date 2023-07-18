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

source(file = "load_bls_cps_jobs_data.R")
####### VIEW SERIES TITLE AND ID 



categories_view <- cps_jobs_data %>% 
  #filter(seasonal == "S") %>% 
  #filter(periodicity_code == "M") %>%
  distinct(series_title, series_id)



########## Figure 1 : Unemployment Rate - All Four Categories

urate_categories <- c("LNS14000004", "LNS14000005", "LNS14000007", "LNS14000008")

start_date <- "1980-12-01"

U1 <- cps_jobs_data %>% 
  filter(seasonal == "S") %>% 
  filter(periodicity_code == "M") %>%
  filter(series_id %in% urate_categories) %>%
  filter(date >= start_date) %>%
  mutate(value_percent = value/100)%>%
  mutate(num_label = round(100*value_percent, 2))



U1$series_title <- str_replace_all(U1$series_title, "\\(Seas\\) Unemployment Rate - ", "")
U1$series_title <- str_replace_all(U1$series_title, "\\(Seas\\) Unemployment Rate - ", "")



U1 <- U1 %>% mutate(last_value = value_percent*(date == max(date) | date == "2009-02-01" | date == "2020-03-01"))
U1$last_value <- na_if(U1$last_value, 0)



variable_names <- list(
  "LNS14000004" = "White Men",
  "LNS14000005" = "White Women",
  "LNS14000007" = "Black Men",
  "LNS14000008" = "Black Women"
)

variable_labeller <- function(variable, value){
  return (variable_names[value])
}

Fig2 <- ggplot(U1, aes(x = date, y = value_percent)) + 
  geom_bar(stat ='identity', fill="#6EA4BF") +
  geom_point() + facet_wrap(
    facet = "series_id", nrow=2,
    labeller = variable_labeller
  )+ 
  geom_line(color="#2D779C", size = 1.5) + 
  theme_inflation + theme(legend.position = "none") +
  scale_y_continuous(labels = scales::percent) + 
  scale_x_date(date_labels = "%b-%y", breaks = "2 year") +
  #geom_text(aes(label = num_label), check_overlap = TRUE) +
  geom_text(data = U1, aes(x = date, y = value_percent, label = num_label), nudge_y = 0.01, size=2, color="#1E8456")+
  labs(y = NULL,
       x = NULL,
       title = "Unemployment Rate by Race",
       subtitle = "Black Men's unemployment recently saw an uptick", 
       caption ="BLS, CPS. Seasonally adjusted. Author's calculation. Ira Regmi, Roosevelt Institute")

ggsave("Fig a.0.png", dpi="retina", width = 12, height=6.75, units = "in")


### Same Chart 
urate_categories <- c("LNS14000004", "LNS14000005", "LNS14000007", "LNS14000008")

start_date <- "1980-12-01"

U1 <- cps_jobs_data %>% 
  filter(seasonal == "S") %>% 
  filter(periodicity_code == "M") %>%
  filter(series_id %in% urate_categories) %>%
  filter(date >= start_date) %>%
  mutate(value_percent = value/100)%>%
  mutate(num_label = round(100*value_percent, 2))



U1$series_title <- str_replace_all(U1$series_title, "\\(Seas\\) Unemployment Rate - ", "")
U1$series_title <- str_replace_all(U1$series_title, "\\(Seas\\) Unemployment Rate - ", "")



U1 <- U1 %>% mutate(last_value = value_percent*(date == max(date) | date == "2009-02-01" | date == "2020-03-01"))
U1$last_value <- na_if(U1$last_value, 0)



variable_names <- list(
  "LNS14000004" = "White Men",
  "LNS14000005" = "White Women",
  "LNS14000007" = "Black Men",
  "LNS14000008" = "Black Women"
)

variable_labeller <- function(variable, value){
  return (variable_names[value])
}


your_plot3 <- ggplot(U1, aes(x = date, y = value_percent, color=series_title)) + geom_line(size = 1.2) + theme(axis.text.x = element_text (size = 10, angle =90))
your_plot3 <- your_plot3 + theme(legend.title = element_blank(), legend.position = "bottom") + 
  scale_y_continuous(labels = scales::percent) + scale_x_date(date_labels = "%b-%y", breaks = "1 year") + 
  geom_point (color = "black", size = 1)
your_plot3 <- your_plot3 + geom_label_repel ((aes (y = last_value, label = scales::percent (value_percent))), show.legend = FALSE) + 
  theme_inflation


your_plot3


your_plot3 <- your_plot3 + labs(x="",
                                y="",
                                title="Black Unemployment is twice as much as White Unemployment",
                                subtitle="Black and Hispanic Unemployment rates are consistently higher than White and overall rates",
                                caption = "Data: BLS, CPS. Seasonally adjusted. Author's calculations") +
  theme(plot.title = element_markdown(size = 30, face="bold"),
        plot.subtitle = element_markdown(size = 20, margin=margin(9,0,15,0)))
your_plot3
ggsave("a3.png", width = 19, height=10.68, dpi="retina")

##### SUMMARIZE AVERAGES 

urate_categories <- c("LNS14000004", "LNS14000005", "LNS14000007", "LNS14000008")

start_date <- "1980-12-01"

variable_names <- list(
  "LNS14000004" = "White Men",
  "LNS14000005" = "White Women",
  "LNS14000007" = "Black Men",
  "LNS14000008" = "Black Women"
)

variable_labeller <- function(variable, value){
  return (variable_names[value])
}

U5 <- cps_jobs_data %>% 
  filter(seasonal == "S") %>% 
  filter(periodicity_code == "M") %>%
  filter(series_id %in% urate_categories) %>%
  filter(date >= start_date) %>%
  group_by(series_id)%>%
  summarise(mean_unemployment = mean(value))


summary_unemp <- U5 %>% as.data.frame()

##### MARGINALLY ATTACHED WORKERS 
mrate_categories <- c("LNU05092417", "LNU05092421", "LNU05092429", "LNU05092433")

start_date <- "1980-12-01"

M1 <- cps_jobs_data %>% 
  #filter(seasonal == "S") %>% 
 # filter(periodicity_code == "M") %>%
  filter(series_id %in% mrate_categories) %>%
  filter(date >= start_date) %>%
  mutate(value = value)%>%
  mutate(num_label = round(value/100, 2))



M1$series_title <- str_replace_all(M1$series_title, "\\(Seas\\) Unemployment Rate - ", "")
M1$series_title <- str_replace_all(M1$series_title, "\\(Seas\\) Unemployment Rate - ", "")



M1 <- M1 %>% mutate(last_value = value*(date == max(date) | date == "2009-02-01" | date == "2020-03-01"))
M1$last_value <- na_if(M1$last_value, 0)



variable_names <- list(
  "LNU05092417" = "White Men",
  "LNU05092421" = "White Women",
  "LNU05092429" = "Black Men",
  "LNU05092433" = "Black Women"
)

variable_labeller <- function(variable, value){
  return (variable_names[value])
}

Fig2 <- ggplot(M1, aes(x = date, y = value)) + 
  geom_bar(stat ='identity', fill="#6EA4BF") +
  geom_point() + facet_wrap(
    facet = "series_id", nrow=2,
    labeller = variable_labeller
  )+ 
  geom_line(color="#2D779C", size = 1.5) + 
  theme_inflation + theme(legend.position = "none") +
  scale_y_continuous(labels = scales::percent) + 
  scale_x_date(date_labels = "%b-%y", breaks = "2 year") +
  #geom_text(aes(label = num_label), check_overlap = TRUE) +
  geom_text(data = U1, aes(x = date, y = value_percent, label = num_label), nudge_y = 0.01, size=2, color="#1E8456")+
  labs(y = NULL,
       x = NULL,
       title = "Marginally Attached Workers by Race",
       subtitle = "TKTK", 
       caption ="BLS, CPS. Seasonally adjusted. Author's calculation. Ira Regmi, Roosevelt Institute")

ggsave("Fig M.0.png", dpi="retina", width = 12, height=6.75, units = "in")

#### POPULATION 
mrate_categories <- c("LNU00000004", "LNU00000005", "LNU00000007", "LNU00000008")

start_date <- "2014-12-01"

P1 <- cps_jobs_data %>% 
  #filter(seasonal == "S") %>% 
  # filter(periodicity_code == "M") %>%
  filter(series_id %in% mrate_categories) %>%
  filter(date >= start_date) %>%
  mutate(value = value)%>%
  mutate(num_label = round(value/100, 2))



P1$series_title <- str_replace_all(P1$series_title, "\\(Seas\\) Unemployment Rate - ", "")
P1$series_title <- str_replace_all(P1$series_title, "\\(Seas\\) Unemployment Rate - ", "")



P1 <- P1 %>% mutate(last_value = value*(date == max(date) | date == "2009-02-01" | date == "2020-03-01"))
P1$last_value <- na_if(P1$last_value, 0)



variable_names <- list(
  "LNU00000004" = "White Men",
  "LNU00000005" = "White Women",
  "LNU00000007" = "Black Men",
  "LNU00000008" = "Black Women"
)

variable_labeller <- function(variable, value){
  return (variable_names[value])
}

Fig2 <- ggplot(P1, aes(x = date, y = value)) + 
  geom_bar(stat ='identity', fill="#6EA4BF") +
  geom_point() + facet_wrap(
    facet = "series_id", nrow=2,
    labeller = variable_labeller
  )+ 
  geom_line(color="#2D779C", size = 1.5) + 
  theme_inflation + theme(legend.position = "none") +
  scale_y_continuous(labels = scales::percent) + 
  scale_x_date(date_labels = "%b-%y", breaks = "2 year") +
  #geom_text(aes(label = num_label), check_overlap = TRUE) +
  geom_text(data = U1, aes(x = date, y = value_percent, label = num_label), nudge_y = 0.01, size=2, color="#1E8456")+
  labs(y = NULL,
       x = NULL,
       title = "Marginally Attached Workers by Race",
       subtitle = "TKTK", 
       caption ="BLS, CPS. Seasonally adjusted. Author's calculation. Ira Regmi, Roosevelt Institute")

ggsave("Fig P.0.png", dpi="retina", width = 12, height=6.75, units = "in")

#### FIG 2: Want a job now, marginally attached, Discouraged Workers 

u7rate_categories <- c("LNU05092471", "LNU05092475", "LNU05092483", "LNU05092487")

start_date <- "2015-12-01"

U7 <- cps_jobs_data_1 %>% 
  #filter(seasonal == "S") %>% 
  #filter(periodicity_code == "M") %>%
  filter(series_id %in% u7rate_categories) %>%
  filter(date >= start_date) %>%
  mutate(value_percent = value)%>%
  mutate(num_label = round(100000*value_percent, 2))



#U7$series_title <- str_replace_all(U7$series_title, "\\(Seas\\) Unemployment Rate - ", "")
#U7$series_title <- str_replace_all(U7$series_title, "\\(Seas\\) Unemployment Rate - ", "")



U7 <- U7 %>% mutate(last_value = value_percent*(date == max(date) | date == "2009-02-01" | date == "2020-03-01"))
U7$last_value <- na_if(U7$last_value, 0)



variable_names <- list(
  "LNU05092471" = "White Men",
  "LNU05092475" = "White Women",
  "LNU05092483" = "Black Men",
  "LNU05092487" = "Black Women"
)

variable_labeller <- function(variable, value){
  return (variable_names[value])
}

Fig2 <- ggplot(U7, aes(x = date, y = value_percent)) + 
  geom_bar(stat ='identity', fill="#6EA4BF") +
  geom_point() + facet_wrap(
    facet = "series_id", nrow=2,
    labeller = variable_labeller
  )+ 
  geom_line(color="#2D779C", size = 1.5) + 
  theme_inflation + theme(legend.position = "none") +
  scale_y_continuous() + 
  scale_x_date(date_labels = "%b-%y", breaks = "12 month") +
  #geom_text(aes(label = num_label), check_overlap = TRUE) +
  geom_text(data = U7, aes(x = date, y = value_percent, label = num_label), nudge_y = 8, size=2, color="#1E8456")+
  labs(y = NULL,
       x = NULL,
       title = "Discouraged Workers by Race",
       subtitle = "Black Men's unemployment rate fell sharply", 
       caption ="BLS, CPS. Seasonally adjusted. Author's calculation. Ira Regmi, Roosevelt Institute")

ggsave("Fig 11.0.png", dpi="retina", width = 12, height=6.75, units = "in")


#### FIG 1: Want a job now, marginally attached, Discouraged Workers 

u7rate_categories <- c("LNU05092471", "LNU05092475", "LNU05092483", "LNU05092487")

start_date <- "2014-12-01"

U7 <- cps_jobs_data %>% 
  #filter(seasonal == "S") %>% 
  #filter(periodicity_code == "M") %>%
  filter(series_id %in% u7rate_categories) %>%
  filter(date >= start_date) %>%
  mutate(value_percent = value)%>%
  mutate(num_label = round(100000*value_percent, 2))



#U7$series_title <- str_replace_all(U7$series_title, "\\(Seas\\) Unemployment Rate - ", "")
#U7$series_title <- str_replace_all(U7$series_title, "\\(Seas\\) Unemployment Rate - ", "")



U7 <- U7 %>% mutate(last_value = value_percent*(date == max(date) | date == "2009-02-01" | date == "2020-03-01"))
U7$last_value <- na_if(U7$last_value, 0)



variable_names <- list(
  "LNU05092471" = "White Men",
  "LNU05092475" = "White Women",
  "LNU05092483" = "Black Men",
  "LNU05092487" = "Black Women"
)

variable_labeller <- function(variable, value){
  return (variable_names[value])
}

Fig2 <- ggplot(U7, aes(x = date, y = value_percent)) + 
  geom_bar(stat ='identity', fill="#6EA4BF") +
  geom_point() + facet_wrap(
    facet = "series_id", nrow=2,
    labeller = variable_labeller
  )+ 
  geom_line(color="#2D779C", size = 1.5) + 
  theme_inflation + theme(legend.position = "none") +
  scale_y_continuous() + 
  scale_x_date(date_labels = "%b-%y", breaks = "12 month") +
  #geom_text(aes(label = num_label), check_overlap = TRUE) +
  geom_text(data = U7, aes(x = date, y = value_percent, label = num_label), nudge_y = 8, size=2, color="#1E8456")+
  labs(y = NULL,
       x = NULL,
       title = "Discouraged Workers by Race",
       subtitle = "Black Men's unemployment rate fell sharply", 
       caption ="BLS, CPS. Seasonally adjusted. Author's calculation. Ira Regmi, Roosevelt Institute")

ggsave("Fig 11.0.png", dpi="retina", width = 12, height=6.75, units = "in")

#### u6 broad unemployment 

u6rate_categories <- c("LNU05092417", "LNU05092421", "LNU05092429", "LNU05092433")

start_date <- "2019-12-01"

U6 <- cps_jobs_data %>% 
  #filter(seasonal == "S") %>% 
  #filter(periodicity_code == "M") %>%
  filter(series_id %in% u6rate_categories) %>%
  filter(date >= start_date) %>%
  mutate(value = value/100)%>%
  mutate(num_label = round(100*value, 2))



U6$series_title <- str_replace_all(U6$series_title, "\\(Seas\\) Unemployment Rate - ", "")
U6$series_title <- str_replace_all(U6$series_title, "\\(Seas\\) Unemployment Rate - ", "")



U6 <- U6 %>% mutate(last_value = value*(date == max(date) | date == "2009-02-01" | date == "2020-03-01"))
U6$last_value <- na_if(U6$last_value, 0)



variable_names <- list(
  "LNU05092417" = "White Men",
  "LNU05092421" = "White Women",
  "LNU05092429" = "Black Men",
  "LNU05092433" = "Black Women"
)

variable_labeller <- function(variable, value){
  return (variable_names[value])
}

Fig2 <- ggplot(U6, aes(x = date, y = value)) + 
  geom_bar(stat ='identity', fill="#6EA4BF") +
  geom_point() + facet_wrap(
    facet = "series_id", nrow=2,
    labeller = variable_labeller
  )+ 
  geom_line(color="#2D779C", size = 1.5) + 
  theme_inflation + theme(legend.position = "none") +
  scale_y_continuous(labels = scales::percent) + 
  scale_x_date(date_labels = "%b-%y", breaks = "3 month") +
  #geom_text(aes(label = num_label), check_overlap = TRUE) +
  geom_text(data = U6, aes(x = date, y = value, label = num_label), nudge_y = 0.01, size=2, color="#1E8456")+
  labs(y = NULL,
       x = NULL,
       title = "Unemployment Rate Including Marginally Attached (U-6) by Race",
       subtitle = "There is a persistent gap in Black and White unemployment", 
       caption ="BLS, CPS. Seasonally adjusted. Author's calculation. Ira Regmi, Roosevelt Institute")

ggsave("Fig 10.0.png", dpi="retina", width = 12, height=6.75, units = "in")
