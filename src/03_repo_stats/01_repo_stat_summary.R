library(RPostgres)
library(data.table)
library(tidytable)
library(ggplot2)
library(stringr)

repo_stats<-fread("repo_stats_bt_10000_11000.csv")
repo_stats_mean<-repo_stats %>% filter.(stars<=300, watchers<=20)
repo_stats_topic<-repo_stats %>% filter.(topics!="[]")

# quick stats
summary(repo_stats %>% select.(stars, watchers))

summary(repo_stats_topic %>% select.(stars, watchers))

# some visuals
ggplot(repo_stats, aes(x=stars))+geom_histogram()
ggplot(repo_stats, aes(x="", y=watchers))+geom_boxplot()
ggplot(repo_stats, aes(stars, watchers))+geom_point()

ggplot(repo_stats_mean, aes(stars, watchers))+geom_point()

# keywords
To_List<-function(string){
  temp<-str_replace_all(string, "[\\[\\s\\]]", "")
  temp<-str_split(temp, ",")
  
  return(temp)
}

repo_stats_topic$topics<-map.(repo_stats_topic$topics,To_List)

topic_list<-as.data.table(unlist(repo_stats_topic$topics, recursive = TRUE))
freq<-as.data.table(table(topic_list)) %>% arrange.(freq, desc(N))
head(freq)

freq_temp<-filter.(freq, N>1) %>% arrange.(freq_temp, desc(N))

# popular repos + keywords
repo_stats_popular<-repo_stats %>% filter.(stars>=300, watchers>=20) #cutoff based on means
repo_stats_popular$topics<-map.(repo_stats_popular$topics, To_List)
topic_list_popular<-as.data.table(unlist(repo_stats_popular$topics, recursive=TRUE))
freq_popular<-as.data.table(table(topic_list_popular)) %>% arrange.(desc(N))
head(freq_popular)
#keywords that occur in the same list the most frequently?
