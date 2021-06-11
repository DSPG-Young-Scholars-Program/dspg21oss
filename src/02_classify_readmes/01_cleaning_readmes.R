##### Read in File
df <- read_csv("~/test/oss/readme_test_data.csv")

# playing around with regex in R
str_replace(df$readme_text[2], "\n", " ")
str_replace(df$readme_text[2], "^\\S*\n", " ")

