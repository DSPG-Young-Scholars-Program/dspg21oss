
# load function 
source("~/git/dspg21oss/scripts/detect_sw.R")


# load data 
#rm(list=ls())
library("dplyr")
library("readr")
path_for_data = "/project/class/bii_sdad_dspg/ncses_oss_2021/requests_scrape/oss_readme_aggregated/"
setwd(path_for_data)
readme_raw_data <- read_csv("oss_readme_data_061521.csv") %>% 
  filter(status == "Done") %>% 
  distinct(slug, readme_text, batch, as_of, status)


# test get_dictionary_terms() 
source("~/git/dspg21oss/scripts/detect_sw.R")
chk_dict <- get_dictionary_terms(summary_type = "Application")
chk_dict <- get_dictionary_terms(main_type = "Blockchain")
chk_dict <- get_dictionary_terms(main_type = "Blockchain", sub_type = FALSE)
chk_dict <- get_dictionary_terms(sub_type = "Crytocurrency")
chk_dict <- get_dictionary_terms(summary_type = "Utility", main_type = FALSE, sub_type = FALSE)


# test detect_types() 
setwd("~/git/dspg21oss/docs/")
software_types <- readr::read_csv("oss_software_types - dictionary.csv") 
util_general <- software_types %>% filter(summary_type == "Utility" & is.na(main_type))
util_general <- as.data.frame(util_general$terms)
chk <- readme_raw_data %>% 
  detect_types(slug, readme_text, util_general)


# test detect_utility_sw() 
chk <- readme_raw_data %>% 
  detect_utility_sw(slug, readme_text, sum_only = TRUE)



# still in development 
tmp_df <- readme_raw_data %>% 
  as.data.frame() %>% 
  tidytext::unnest_tokens(word, readme_text) %>%
  as_tidytable() %>% 
  tidytable::count.(slug, name = "n_words")

chk <- chk %>% 
  tidytable::left_join.(tmp_df) %>% 
  tidytable::mutate.(sd_words = scale(n_words, center = TRUE, scale = TRUE)) %>% 
  tidytable::mutate.(utility_prob = as.numeric(utility_all / n_words)) 

chk_chk <- readme_raw_data %>% 
  tidytable::distinct.(readme_text) %>% 
  tidytable::mutate.(unique_text = 1) 

chk_chk_chk <- readme_raw_data %>% 
  tidytable::left_join.(chk_chk)