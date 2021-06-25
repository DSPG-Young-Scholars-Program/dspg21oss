# Loading ####
library(collapsibleTree)
library(tidyverse)
library(BBmisc)

# import data
df <- read.csv("~/oss_software_types - dictionary.csv")
sw_types <- select(df,"summary_type", "main_type", "sub_type")

# get order of summary type ####
df %>% 
  filter(!is.na(sourceforge_count)) %>% 
  group_by(summary_type) %>% 
  summarise(sourceforge_count = sum(sourceforge_count)) %>% 
  arrange(desc(sourceforge_count))

df$summary_type <- as.factor(df$summary_type)
summary_order <- c("System", "Application", "Programming", "Topics", "Utility", "Other/Nonlisted Topic")
df_ordered <- df[order(factor(df$summary_type, levels = summary_order)),]

# fix no main type on system
df_ordered <- df_ordered %>% 
  mutate(main_type = replace(main_type, main_type=="" & summary_type=="System", "Other"))

# order of main type ####

main_counts <- df_ordered %>% 
  filter(!is.na(sourceforge_count)) %>% 
  group_by(main_type) %>% 
  summarise(sourceforge_count = sum(sourceforge_count)) %>% 
  arrange(desc(sourceforge_count))

main_order <- pull(main_counts, main_type)

df_ordered$main_type <- as.factor(df_ordered$main_type)
df_ordered2 <- df_ordered[order(factor(df$main_type, levels = main_order)),]


# make tree ####
collapsibleTree(df_ordered,
                hierarchy = c("summary_type", "main_type", "sub_type"),
                width=800, height = 850, 
                root = "Software Types")

collapsibleTree(df_ordered,
                hierarchy = c("summary_type", "main_type", "sub_type"),
                width=800, height = 850, 
                root = "Software Types", 
                nodeSize = "sourceforge_count")

# change the size of bubbles

a <- df_ordered %>% 
      filter(!is.na(sourceforge_count)) %>% 
      normalize(df_ordered$sourceforge_count, method="standardize", 
                range=c(0,1000))

