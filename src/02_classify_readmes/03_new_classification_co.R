rm(list=ls())
# Load Libraries
library("dplyr")
library("readr")

# Read in readme data
path_for_data = "/project/class/bii_sdad_dspg/ncses_oss_2021/requests_scrape/oss_readme_aggregated/"
setwd(path_for_data)
readme_raw_data <- read_csv("oss_readme_data_061521.csv") %>% 
  filter(status == "Done") %>% 
  distinct(slug, readme_text, batch, as_of, status)

# load function 
source("~/git/dspg21oss/scripts/detect_sw_co.R")

# using function to classify  
chk <- readme_raw_data %>%
  top_n(25, slug) %>% 
  detect_system_sw(slug, readme_text)
# other functions 
chk <- readme_raw_data %>%
  top_n(25, slug) %>% 
  detect_utility_sw(slug, readme_text)

# only one column at a time
# if you only want to develop certain categories 
system_terms <- get_dictionary_terms(summary_type = "System")
sys_os <- get_dictionary_terms(main_type = "Operating Systems")
windows_terms <- get_dictionary_terms(sub_type = "Windows")

chk <- readme_raw_data %>% 
  top_n(25, slug) %>% 
  as_tidytable() %>% 
  tidytable::mutate.(readme_text = tolower(readme_text)) %>% 
  detect_types(slug, readme_text, windows_terms) 
