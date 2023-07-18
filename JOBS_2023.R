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
  filter(seasonal == "S") %>% 
  filter(periodicity_code == "M") %>%
  distinct(series_title, series_id)
  


########## Figure 1 : Unemployment Rate - All Four Categories

urate_categories <- c("LNS14000004", "LNS14000005", "LNS14000007", "LNS14000008")

start_date <- "2019-12-01"

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
  scale_x_date(date_labels = "%b-%y", breaks = "3 month") +
  #geom_text(aes(label = num_label), check_overlap = TRUE) +
  geom_text(data = U1, aes(x = date, y = value_percent, label = num_label), nudge_y = 0.01, size=2, color="#1E8456")+
  labs(y = NULL,
       x = NULL,
       title = "Unemployment Rate by Race",
       subtitle = "Black Men's unemployment recently saw an uptick", 
       caption ="BLS, CPS. Seasonally adjusted. Author's calculation. Ira Regmi, Roosevelt Institute")

ggsave("Fig 1.0.png", dpi="retina", width = 12, height=6.75, units = "in")




#### Figure 2: BAR CHART - Pick Category
pick_categories <- c("LNS14000004")
start_date <- "2019-01-01"
U2 <- cps_jobs_data %>% 
  filter(seasonal == "S") %>% 
  filter(periodicity_code == "M") %>%
  filter(series_id %in% pick_categories) %>%
  filter(date >= start_date) %>%
  mutate(value_percent = value/100)%>%
  mutate(num_label = round(100*value_percent, 2))


U2$series_title <- str_replace_all(U2$series_title, "\\(Seas\\) Unemployment Rate - ", "")
U2$series_title <- str_replace_all(U2$series_title, "\\(Seas\\) Unemployment Rate - ", "")



U2 <- U2 %>% mutate(last_value = value_percent*(date == max(date) | date == "2009-02-01" | date == "2020-03-01"))
U2$last_value <- na_if(U2$last_value, 0)


Fig2 <- ggplot(U2, aes(x = date, y = value_percent)) + 
  geom_bar(stat = 'identity', fill = "#6EA4BF")  +  
  geom_point() + 
  geom_line(color="#2D779C", size = 1.5) + 
  theme_inflation + theme(legend.position = "none") +
  scale_y_continuous(labels = scales::percent) + 
  scale_x_date(date_labels = "%b-%y", breaks = "4 month") +
  #geom_text(aes(label = num_label), check_overlap = TRUE) +
  geom_text(data = U2, aes(x = date, y = value_percent, label = num_label), nudge_y = 0.005, size=3, color="#1E8456")+
  labs(y = NULL,
       x = NULL,
       title = "Unemployment Rate for White Men",
       subtitle = "TKTK", 
       caption ="BLS, CPS. Seasonally adjusted. Author's calculation. Ira Regmi, Roosevelt Institute")

ggsave("Fig 2.0.png", dpi="retina", width = 12, height=6.75, units = "in")

########## Figure 3 : LABOR FORCE PARTICIPATION RATE

lfp_categories <- c("LNS11300004", "LNS11300005", "LNS11300007", "LNS11300008")

start_date <- "2019-12-01"

L1 <- cps_jobs_data %>% 
  filter(seasonal == "S") %>% 
  filter(periodicity_code == "M") %>%
  filter(series_id %in% lfp_categories) %>%
  filter(date >= start_date) %>%
  mutate(value_percent = value/100)%>%
  mutate(num_label = round(100*value_percent, 1))



L1$series_title <- str_replace_all(L1$series_title, "\\(Seas\\) Labor Force Participation Rate - ", "")
L1$series_title <- str_replace_all(L1$series_title, "\\(Seas\\) Labor Force Participation Rate - ", "")



L1 <- L1 %>% mutate(last_value = value_percent*(date == max(date) | date == "2009-02-01" | date == "2020-03-01"))
L1$last_value <- na_if(L1$last_value, 0)



variable_names <- list(
  "LNS11300004" = "White Men",
  "LNS11300005" = "White Women",
  "LNS11300007" = "Black Men",
  "LNS11300008" = "Black Women"
)

variable_labeller <- function(variable, value){
  return (variable_names[value])
}

Fig3 <- ggplot(L1, aes(x = date, y = value_percent)) + 
  geom_bar(stat ='identity', fill="#6EA4BF") +
  geom_point() + facet_wrap(
    facet = "series_id", nrow=2, ncol=2,
    labeller = variable_labeller
  )+ 
  geom_line(color="#2D779C", size = 1.5) + 
  theme_inflation + theme(legend.position = "none") +
  scale_y_continuous(labels = scales::percent) + 
  scale_x_date(date_labels = "%b-%y", breaks = "3 month") +
  #geom_text(aes(label = num_label), check_overlap = TRUE) +
  geom_text(data = L1, aes(x = date, y = value_percent, label = num_label), nudge_y = 0.05, size=2, angle=45, face="bold", color="#1E8456")+
  labs(y = NULL,
       x = NULL,
       title = "Labor Force Participation Rate",
       subtitle = "TKTK", 
       caption ="BLS, CPS. Seasonally adjusted. Author's calculation. Ira Regmi, Roosevelt Institute")

ggsave("Fig 3.0.png", dpi="retina", width = 12, height=6.75, units = "in")

########## Figure 4 : EPOP

EPOP_categories <- c("LNS12300004", "LNS12300005", "LNS12300007", "LNS12300008")

start_date <- "2019-12-01"

E1 <- cps_jobs_data %>% 
  filter(seasonal == "S") %>% 
  filter(periodicity_code == "M") %>%
  filter(series_id %in% EPOP_categories) %>%
  filter(date >= start_date) %>%
  mutate(value_percent = value/100)%>%
  mutate(num_label = round(100*value_percent, 1))



E1$series_title <- str_replace_all(E1$series_title, "\\(Seas\\) Employment - Population Ratio - ", "")
E1$series_title <- str_replace_all(E1$series_title, "\\(Seas\\) Employment - Population Ratio - ", "")



E1 <- E1 %>% mutate(last_value = value_percent*(date == max(date) | date == "2009-02-01" | date == "2020-03-01"))
E1$last_value <- na_if(E1$last_value, 0)



variable_names <- list(
  "LNS12300004" = "White Men",
  "LNS12300005" = "White Women",
  "LNS12300007" = "Black Men",
  "LNS12300008" = "Black Women"
)

variable_labeller <- function(variable, value){
  return (variable_names[value])
}

Fig4 <- ggplot(E1, aes(x = date, y = value_percent)) + 
  geom_bar(stat ='identity', fill="#6EA4BF") +
  geom_point() + facet_wrap(
    facet = "series_id", nrow=2, ncol=2,
    labeller = variable_labeller
  )+ 
  geom_line(color="#2D779C", size = 1.5) + 
  theme_inflation + theme(legend.position = "none") +
  scale_y_continuous(labels = scales::percent) + 
  scale_x_date(date_labels = "%b-%y", breaks = "3 month") +
  #geom_text(aes(label = num_label), check_overlap = TRUE) +
  geom_text(data = E1, aes(x = date, y = value_percent, label = num_label), nudge_y = 0.05, size=2, angle=45, face="bold", color="#1E8456")+
  labs(y = NULL,
       x = NULL,
       title = "EPOP",
       subtitle = "TKTK", 
       caption ="BLS, CPS. Seasonally adjusted. Author's calculation. Ira Regmi, Roosevelt Institute")

ggsave("Fig 4.0.png", dpi="retina", width = 12, height=6.75, units = "in")



#### Figure 5:  LABOR FORCE PARTICIPATION RATE by AGE 


lfp_categories2 <- c("LNS11300012", "LNS11300024", "LNS11300060", "LNS11324230")

start_date <- "2019-12-01"

L2 <- cps_jobs_data %>% 
  filter(seasonal == "S") %>% 
  filter(periodicity_code == "M") %>%
  filter(series_id %in% lfp_categories2) %>%
  filter(date >= start_date) %>%
  mutate(value_percent = value/100)%>%
  mutate(num_label = round(100*value_percent, 1))



L2$series_title <- str_replace_all(L2$series_title, "\\(Seas\\) Labor Force Participation Rate - ", "")
L2$series_title <- str_replace_all(L2$series_title, "\\(Seas\\) Labor Force Participation Rate - ", "")



L2 <- L2 %>% mutate(last_value = value_percent*(date == max(date) | date == "2009-02-01" | date == "2020-03-01"))
L2$last_value <- na_if(L2$last_value, 0)



variable_names <- list(
  "LNS11300012" = "16-19",
  "LNS11300024" = "20 & Over",
  "LNS11300060" = "25-54",
  "LNS11324230" = "55 & Over"
)

variable_labeller <- function(variable, value){
  return (variable_names[value])
}

Fig5 <- ggplot(L2, aes(x = date, y = value_percent)) + 
  geom_bar(stat ='identity', fill="#6EA4BF") +
  geom_point() + facet_wrap(
    facet = "series_id", nrow=2, ncol=2,
    labeller = variable_labeller
  )+ 
  geom_line(color="#2D779C", size = 1.5) + 
  theme_inflation + theme(legend.position = "none") +
  scale_y_continuous(labels = scales::percent) + 
  scale_x_date(date_labels = "%b-%y", breaks = "3 month") +
  #geom_text(aes(label = num_label), check_overlap = TRUE) +
  geom_text(data = L2, aes(x = date, y = value_percent, label = num_label), nudge_y = 0.05, size=2, angle=45, face="bold", color="#1E8456")+
  labs(y = NULL,
       x = NULL,
       title = "Labor Force Participation Rate by Age",
       subtitle = "TKTK", 
       caption ="BLS, CPS. Seasonally adjusted. Author's calculation. Ira Regmi, Roosevelt Institute")

ggsave("Fig 5.0.png", dpi="retina", width = 12, height=6.75, units = "in")

#### Figure 6:  LABOR FORCE PARTICIPATION RATE by Marriage 


lfp_categories3 <- c("LNS11300000", "LNS11325669", "LNS11300150", "LNS11300315")

start_date <- "2019-12-01"

L3 <- cps_jobs_data %>% 
  filter(seasonal == "S") %>% 
  filter(periodicity_code == "M") %>%
  filter(series_id %in% lfp_categories3) %>%
  filter(date >= start_date) %>%
  mutate(value_percent = value/100)%>%
  mutate(num_label = round(100*value_percent, 1))



L3$series_title <- str_replace_all(L3$series_title, "\\(Seas\\) Labor Force Participation Rate - ", "")
L3$series_title <- str_replace_all(L3$series_title, "\\(Seas\\) Labor Force Participation Rate - ", "")



L3 <- L3 %>% mutate(last_value = value_percent*(date == max(date) | date == "2009-02-01" | date == "2020-03-01"))
L3$last_value <- na_if(L3$last_value, 0)



variable_names <- list(
  "LNS11300000" = "Overall",
  "LNS11325669" = "Married",
  "LNS11300150" = "Married Men",
  "LNS11300315" = "Married Women"
  
)

variable_labeller <- function(variable, value){
  return (variable_names[value])
}

Fig6 <- ggplot(L3, aes(x = date, y = value_percent)) + 
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
  geom_text(data = L3, aes(x = date, y = value_percent, label = num_label), nudge_y = 0.05, size=2, angle=45, face="bold", color="#1E8456")+
  labs(y = NULL,
       x = NULL,
       title = "Labor Force Participation Rate - Childcare",
       subtitle = "TKTK", 
       caption ="BLS, CPS. Seasonally adjusted. Author's calculation. Ira Regmi, Roosevelt Institute")

ggsave("Fig 6.0.png", dpi="retina", width = 12, height=6.75, units = "in")


####### Fig 7 - Employment Level - Types of Employment 

urate_categories3 <- c("LNS12037302", "LNS12034562", "LNS12034568", "LNS12037297")

start_date <- "2019-12-01"

U3 <- cps_jobs_data %>% 
  filter(seasonal == "S") %>% 
  filter(periodicity_code == "M") %>%
  filter(series_id %in% urate_categories3) %>%
  filter(date >= start_date) %>%
  mutate(value_percent = value/100)%>%
  mutate(num_label = round(value_percent, 0))



U3$series_title <- str_replace_all(U3$series_title, "\\(Seas\\) Unemployment Rate - ", "")
U3$series_title <- str_replace_all(U3$series_title, "\\(Seas\\) Unemployment Rate - ", "")



U3 <- U3 %>% mutate(last_value = value_percent*(date == max(date) | date == "2009-02-01" | date == "2020-03-01"))
U3$last_value <- na_if(U3$last_value, 0)



variable_names <- list(
  "LNS12037302" = "Arts & Entertainment ",
  "LNS12034562" = "Construction",
  "LNS12034568" = "Retail",
  "LNS12037297" = "Education"
)

variable_labeller <- function(variable, value){
  return (variable_names[value])
}

Fig7 <- ggplot(U3, aes(x = date, y = value_percent)) + 
  geom_bar(stat ='identity', fill="#6EA4BF") +
  geom_point() + facet_wrap(
    facet = "series_id", nrow=2,
    labeller = variable_labeller
  )+ 
  geom_line(color="#2D779C", size = 1.5) + 
  theme_inflation + theme(legend.position = "none") +
  scale_y_continuous() + 
  scale_x_date(date_labels = "%b-%y", breaks = "3 month") +
  #geom_text(aes(label = num_label), check_overlap = TRUE) +
  geom_text(data = U3, aes(x = date, y = value_percent, label = num_label), nudge_y = 9, angle=90, size=2, color="#1E8456")+
  labs(y = NULL,
       x = NULL,
       title = "Employment Level by Industry",
       subtitle = "TKTK", 
       caption ="BLS, CPS. Seasonally adjusted. Author's calculation. Ira Regmi, Roosevelt Institute")

ggsave("Fig 7.0.png", dpi="retina", width = 12, height=6.75, units = "in")

####### Fig 8 - Employment Level - Race

urate_categories4 <- c("LNS12000007", "LNS12000008")

start_date <- "2019-12-01"

U4 <- cps_jobs_data %>% 
  filter(seasonal == "S") %>% 
  filter(periodicity_code == "M") %>%
  filter(series_id %in% urate_categories4) %>%
  filter(date >= start_date) %>%
  mutate(value_percent = value)%>%
  mutate(num_label = round(value_percent, 0))



U4$series_title <- str_replace_all(U4$series_title, "\\(Seas\\) Unemployment Rate - ", "")
U4$series_title <- str_replace_all(U4$series_title, "\\(Seas\\) Unemployment Rate - ", "")



U4 <- U4 %>% mutate(last_value = value_percent*(date == max(date) | date == "2009-02-01" | date == "2020-03-01"))
U4$last_value <- na_if(U4$last_value, 0)



variable_names <- list(
  "LNS12000007" = "Black Men",
  "LNS12000008" = "Black Women"
)

variable_labeller <- function(variable, value){
  return (variable_names[value])
}

Fig8 <- ggplot(U4, aes(x = date, y = value_percent)) + 
  geom_bar(stat ='identity', fill="#6EA4BF") +
  geom_point() + facet_wrap(
    facet = "series_id",
    labeller = variable_labeller
  )+ 
  geom_line(color="#2D779C", size = 1.5) + 
  theme_inflation + theme(legend.position = "none") +
  scale_y_continuous() + 
  scale_x_date(date_labels = "%b-%y", breaks = "3 month") +
  #geom_text(aes(label = num_label), check_overlap = TRUE) +
  geom_text(data = U4, aes(x = date, y = value_percent, label = num_label), nudge_y = 400, angle=90, size=2, color="#1E8456")+
  labs(y = NULL,
       x = NULL,
       title = "Employment Level by Race",
       subtitle = "TKTK", 
       caption ="BLS, CPS. Seasonally adjusted. Author's calculation. Ira Regmi, Roosevelt Institute")

ggsave("Fig 8.0.png", dpi="retina", width = 12, height=6.75, units = "in")

##### Figure 9 - Labor Force Level 

lforce_categories3 <- c("LNS11000031", "LNS11000032", "LNS11000028", "LNS11000029")

start_date <- "2019-12-01"

L9 <- cps_jobs_data %>% 
  filter(seasonal == "S") %>% 
  filter(periodicity_code == "M") %>%
  filter(series_id %in% lforce_categories3) %>%
  filter(date >= start_date) %>%
  mutate(value_percent = value/100)%>%
  mutate(num_label = round(value_percent, 0))



L9$series_title <- str_replace_all(L9$series_title, "\\(Seas\\) Unemployment Rate - ", "")
L9$series_title <- str_replace_all(L9$series_title, "\\(Seas\\) Unemployment Rate - ", "")



L9 <- L9 %>% mutate(last_value = value_percent*(date == max(date) | date == "2009-02-01" | date == "2020-03-01"))
L9$last_value <- na_if(L9$last_value, 0)



variable_names <- list(
  "LNS11000031" = "Black Men ",
  "LNS11000032" = "Black Women",
  "LNS11000028" = "White Men",
  "LNS11000029" = "White Women"
)

variable_labeller <- function(variable, value){
  return (variable_names[value])
}

Fig9 <- ggplot(L9, aes(x = date, y = value_percent)) + 
  geom_bar(stat ='identity', fill="#6EA4BF") +
  geom_point() + facet_wrap(
    facet = "series_id", nrow=2,
    labeller = variable_labeller
  )+ 
  geom_line(color="#2D779C", size = 1.5) + 
  theme_inflation + theme(legend.position = "none") +
  scale_y_continuous() + 
  scale_x_date(date_labels = "%b-%y", breaks = "3 month") +
  #geom_text(aes(label = num_label), check_overlap = TRUE) +
  geom_text(data = L9, aes(x = date, y = value_percent, label = num_label), nudge_y = 90, angle=90, size=2, color="#1E8456")+
  labs(y = NULL,
       x = NULL,
       title = "Labor Force Level",
       subtitle = "TKTK", 
       caption ="BLS, CPS. Seasonally adjusted. Author's calculation. Ira Regmi, Roosevelt Institute")

ggsave("Fig 9.0.png", dpi="retina", width = 12, height=6.75, units = "in")


