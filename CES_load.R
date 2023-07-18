#### Current Employment Survery 
#### CES JOBS ########
setwd("/Users/iregmii/Documents/Data Projects/JOBS 2023")
library(janitor)
library(tidyverse)
library(ggtext)

ces_data <- GET("https://download.bls.gov/pub/time.series/ce/ce.data.0.AllCESSeries", user_agent("ira.regmi@gmail.com")) %>%
  content(as = "text") %>%
  fread()
  

ces_data <- ces_data %>%
  clean_names()
ces_data$series_id <- str_trim(ces_data$series_id)
ces_data$value <- as.numeric(ces_data$value)
ces_data$date <- paste(substr(ces_data$period, 2,3), "01", ces_data$year, sep="/")
ces_data$date <- as.Date(ces_data$date, "%m/%d/%Y")

ces_series <- GET("https://download.bls.gov/pub/time.series/ce/ce.series", user_agent("ira.regmi@gmail.com")) %>%
  content(as = "text") %>%
  fread()
ces_series <- ces_series %>% 
  clean_names()
ces_series$series_id <- str_trim(ces_series$series_id)

ces_data_type <- GET("https://download.bls.gov/pub/time.series/ce/ce.datatype", user_agent("ira.regmi@gmail.com")) %>%
  content(as = "text") %>%
  fread()
ces_super_sector <- GET("https://download.bls.gov/pub/time.series/ce/ce.supersector", user_agent("ira.regmi@gmail.com")) %>%
  content(as = "text") %>%
  fread()
ces_series <- inner_join(ces_series, ces_data_type, by = "data_type_code")
ces_series <- inner_join(ces_series, ces_super_sector, by = "supersector_code")

ces_data <- inner_join(ces_data, ces_series, by = c("series_id"))
ces_data <- select(ces_data, -c("footnote_codes.x", "footnote_codes.y", "begin_year", "begin_period", "end_year", "end_period"))
rm(ces_series, ces_data_type, ces_super_sector)
######################################################################