

rm(list=ls())
library("tidyverse")
library("dplyr")
library("readr")
library("RPostgreSQL")


# readme data 
path_for_data = "/project/class/bii_sdad_dspg/uva_2021/dspg21oss/"
setwd(path_for_data)
readme_raw_data <- read_csv("oss_readme_data_071521.csv") %>% 
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
                          FROM gh_2007_2020.repos WHERE commits > 10;")
dbDisconnect(conn)

# repo_stats data 
setwd(path_for_data)
repo_stats <- read_csv("repo_stats_0714.csv")

combined_data <- repos_table  %>% 
  inner_join(repo_stats, by = "slug") %>% 
  inner_join(readme_raw_data, by = "slug") %>% 
  select(slug, description, readme_text, language, 
         topics, commits, forks, stars, watchers) %>% 
  drop_na(description, readme_text) %>%
  rename(readme = readme_text) %>% 
  arrange(-stars) 

top_2_percent <- 10288063 * 0.02 
combined_data <- combined_data %>% 
  top_n(top_2_percent, commits)

# repos/commits data 
conn <- dbConnect(drv = PostgreSQL(),
                  dbname = "sdad",
                  host = "10.250.124.195",
                  port = 5432,
                  user = Sys.getenv("db_userid"),
                  password = Sys.getenv("db_pwd"))
dbWriteTable(conn, c("gh_2007_2020", "repos_data"), combined_data, row.names = FALSE)
dbDisconnect(conn)








