
<!-- README.md is generated from README.Rmd. Please edit that file -->
#### Functions in Development

    ├── Detect Application Software 
        ├── detect_blockchain_sw()
        ├── detect_database_sw()
    ├── Detect Programming Software 
        ├── detect_programming_sw()
    ├── Detect System Software 
        ├── detect_system_sw()
    ├── Detect Utility Software 
        ├── detect_utility_sw()
    ├── Detect Software Topics 

``` r
library("dplyr")
library("readr")
source("~/git/dspg21oss/scripts/detect_sw.R")

setwd("/project/class/bii_sdad_dspg/ncses_oss_2021/requests_scrape/oss_readme_aggregated/")
readme_raw_data <- read_csv("oss_readme_data_061521.csv") %>% 
  filter(status == "Done") %>% 
  distinct(slug, readme_text, batch, as_of, status)

readme_raw_classified <- readme_raw_data %>% 
  detect_blockchain_sw(readme_text) %>% 
  detect_system_sw(readme_text)
```
