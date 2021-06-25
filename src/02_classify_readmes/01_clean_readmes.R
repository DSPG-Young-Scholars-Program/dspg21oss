# load packages
library(tidyverse)
library(tidytext)
library(wordcloud)

# read in File
#readmes <- read_csv("~/test/oss/readme_test_data.csv")
readmes <- read_csv("/project/class/bii_sdad_dspg/ncses_oss_2021/requests_scrape/oss_readme_aggregated/oss_readme_data_062121.csv")

# filtering
readmes_filtered <- readmes[readmes$status == "Done", ]
readmes_filtered <- readmes_filtered[readmes_filtered$readme_text != "404 ERROR - NO README", ]

# tokenizing single words ####
tidy_readmes <- readmes_filtered %>% 
  unnest_tokens(word, readme_text)

# remove stop words
data(stop_words)

tidy_readmes <- tidy_readmes %>% 
  anti_join(stop_words)

#remove numbers
nums <- tidy_readmes %>% 
  filter(str_detect(word, "^[0-9]")) %>% 
  select(word) %>% 
  unique()

tidy_readmes <- tidy_readmes %>% 
  anti_join(nums, by = "word")

# word counts
word_counts <- tidy_readmes %>%
  count(word, sort = T) 

# clean up a few words
drop_words <- c("https", "github.com", "http")
word_counts <- word_counts[!(word_counts$word %in% drop_words),]

word_counts_10 <- word_counts[1:10,]
# visualization
word_counts_10 %>% 
  mutate(word = reorder(word,n)) %>% 
  ggplot(aes(n, word)) +
  geom_col()

# a lil word cloud action
wordcloud(words=word_counts$word, freq = word_counts$n, max.words = 150, 
          random.order = F)


# tokenizing bigrams ####
readmes_bigrams <- readmes_filtered %>% 
  unnest_tokens(bigram, readme_text, token="ngrams", n=2)

# remove numbers 
nums <- readmes_bigrams %>% 
  filter(str_detect(word, "^[0-9]")) %>% 
  select(word) %>% 
  unique()

readmes_bigrams <- readmes_bigrams %>% 
  anti_join(nums, by = "word")

readmes_bigrams <- readmes_bigrams %>% 
  separate(bigram, c("word1", "word2"), sep = " ")

readmes_bigrams <- readmes_bigrams %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

readmes_bigrams %>% 
  count(word1, word2, sort=T)