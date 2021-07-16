

# create training sets 

#### load the data and functions #####################################################################################

rm(list=ls())
library("tidyverse")
library("dplyr")
library("readr")
library("RPostgreSQL")
#source("~/git/dspg21oss/scripts/detect_sw_sz.R")
source("~/git/dspg21oss/scripts/detect_sw_co.R")
source("~/git/dspg21oss/scripts/detect_sw.R")

# readme data 
path_for_data = "/project/class/bii_sdad_dspg/uva_2021/dspg21oss/"
setwd(path_for_data)
readme_raw_data <- read_csv("oss_readme_data_071221.csv") %>% 
  filter(status == "Done") %>% 
  distinct(slug, readme_text, batch, as_of, status)

# repos/commits data 
conn <- dbConnect(drv = PostgreSQL(),
                  dbname = "sdad",
                  host = "10.250.124.195",
                  port = 5432,
                  user = Sys.getenv("db_userid"),
                  password = Sys.getenv("db_pwd"))
repos_table <- dbGetQuery(conn, "SELECT slug, description, primarylanguage as language, commits 
                          FROM gh_2007_2020.repos WHERE commits > 349;")
dbDisconnect(conn)

# repo_stats data 
setwd(path_for_data)
repo_stats <- read_csv("repo_stats_0714.csv")

readme_subset_data <- repos_table  %>% 
  left_join(repo_stats, by = "slug") %>% 
  left_join(readme_raw_data, by = "slug") %>% 
  select(slug, description, readme_text, language, 
         topics, commits, forks, stars, watchers) %>%
  arrange(-stars) %>% 
  # clean the topics column 
  mutate(topics = str_replace_all(topics, "\\[", ""),
         topics = str_replace_all(topics, "\\]", ""),
         topics = str_replace_all(topics, "',", ""),
         topics = str_replace_all(topics, "'", "")) %>% 
  drop_na(description) %>% 
  slice(1:10000) 


#### classify with descriptions #####################################################################################

classified_with_desciptions <- readme_subset_data %>% 
  # programming languages 
  detect_prog_gen_sw(slug, description) %>% 
  detect_prog_stat_sw(slug, description) %>% 
  detect_prog_web_sw(slug, description) %>% 
  # system software 
  detect_system_sw(slug, description) %>% 
  # blockchain technologies
  detect_blockchain_sw(slug, description, sum_only = TRUE) %>%
  # ai/machine learning 
  detect_ai_sw(slug, description, sum_only = TRUE) %>%
  # business/office management
  detect_business_sw(slug, description, sum_only = TRUE) %>% 
  # database management
  detect_database_sw(slug, description, sum_only = TRUE) %>% 
  # parse the variables down to what we have finished 
  select(slug, description, readme_text, language, topics, commits, forks, stars, watchers, 
         prog_python, prog_rlang, prog_java, prog_javascript, prog_php, prog_clang, prog_stat_all,
         sys_windows, sys_linux, sys_mac, sys_android, sys_virtual, starts_with("app_")) %>% 
  rename(readme = readme_text)

colSums(classified_with_desciptions %>% select(-slug, -description, -readme, -language, -topics))

#### subset labeled data #####################################################################################

output_data <- classified_with_desciptions %>% 
  arrange(-commits) %>% 
  select(-readme)

setwd(path_for_data)
write_csv(output_data, "oss_software_tolabel_071421.csv")






