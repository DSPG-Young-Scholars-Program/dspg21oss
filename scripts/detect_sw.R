
# get_dictionary_terms pulls in the oss_software_types dictionary terms 

get_dictionary_terms <- function(summary_type, main_type, sub_type){
  
  library("dplyr")
  library("readr")
  library("tidyr")
  
  if( missing(main_type) & missing(sub_type) ){ 
    
    # this is to pull all of the terms for a summary_type 
    
    summary_type <- enquo(summary_type)
    setwd("~/git/dspg21oss/docs/")
    software_types <- readr::read_csv("oss_software_types - dictionary.csv") 
    software_terms <- software_types %>% 
      filter(summary_type == {{ summary_type }})
    software_terms <- na.omit(software_terms$terms)
    software_terms
    
  } else if (missing(summary_type) & missing(sub_type) ){
    
    # this is to pull all of the terms for a main_type  
    
    main_type <- enquo(main_type)
    setwd("~/git/dspg21oss/docs/")
    software_types <- readr::read_csv("oss_software_types - dictionary.csv") 
    software_terms <- software_types %>% 
      filter(main_type == {{ main_type }}) 
    software_terms <- na.omit(software_terms$terms)
    software_terms
    
  } else if (missing(summary_type) & missing(main_type)) {
    
    sub_type <- enquo(sub_type)
    setwd("~/git/dspg21oss/docs/")
    software_types <- readr::read_csv("oss_software_types - dictionary.csv")
    software_terms <- software_types %>% 
      filter(sub_type == {{ sub_type }})
    software_terms <- na.omit(software_terms$terms)
    software_terms
    
  } else if ( missing(summary_type) & sub_type == FALSE ){
    
    # this is to pull all of the terms for main_type general category   
    
    main_type <- enquo(main_type)
    setwd("~/git/dspg21oss/docs/")
    software_types <- readr::read_csv("oss_software_types - dictionary.csv") 
    software_terms <- software_types %>% 
      filter(main_type == {{ main_type }} & is.na(sub_type)) 
    software_terms <- na.omit(software_terms$terms)
    software_terms
    
  } else if ( main_type == FALSE & sub_type == FALSE ){ 
    
    # this is to pull all of the terms for a summary_type when all else is NA  
    
    summary_type <- enquo(summary_type)
    setwd("~/git/dspg21oss/docs/")
    software_types <- readr::read_csv("oss_software_types - dictionary.csv") 
    software_terms <- software_types %>% 
      filter(summary_type == {{ summary_type }} & is.na(main_type))
    software_terms <- na.omit(software_terms$terms)
    software_terms
    
  } else {
    
    "ERROR: Variables were not correctly. Please try again."
    
  }
  
}

# detect_subcategory is a function embedded within the software_type detector that 
# classifies all of the subcategories by aggregating the total terms in that category 

detect_types <- function(df, id, input, terms){
  
  library("dplyr")
  library("readr")
  library("tidyr")
  library("readr")
  library("stringr")
  library("data.table")
  library("tidytext")
  library("tidytable")
  
  input <- enquo(input)
  output <- enquo(terms)
  id <- enquo(id)
  
  tmp_df <- df %>% 
    as.data.frame() %>% 
    tidytext::unnest_tokens(word, !!input) %>%
    as_tidytable() %>% 
    tidytable::filter.(word %in% terms) %>% 
    tidytable::mutate.("{{output}}" := 1) %>% 
    tidytable::select.(-word) %>% 
    tidytable::summarize.("{{output}}" := sum(!!output), .by = !!id) 
  
  df <- df %>% 
    tidytable::left_join.(tmp_df) %>% 
    tidytable::mutate.("{{output}}" := replace_na.(!!output, 0)) %>% 
    as.data.frame()
  
  df
}



detect_utility_sw <- function(df, id, input, sum_only = FALSE, prob = FALSE){
  
  library("dplyr")
  library("readr")
  library("tidyr")
  library("readr")
  library("stringr")
  library("data.table")
  library("tidytext")
  library("tidytable")
  
  util_general <- get_dictionary_terms(summary_type = "Utility", main_type = F, sub_type = F)
  util_security <- get_dictionary_terms(main_type = "Security")
  util_crypto <- get_dictionary_terms(sub_type = "Cryptography")
  util_pwdmgr <- get_dictionary_terms(sub_type = "Password Manager")
  util_malware <- get_dictionary_terms(sub_type = "Anti-Malware")
  util_virus <- get_dictionary_terms(sub_type = "Anti-Virus")
  util_spam <- get_dictionary_terms(sub_type = "Anti-Spam")

  id <- enquo(id)
  input <- enquo(input)
  
  df <- df %>% 
    as_tidytable() %>% 
    tidytable::mutate.("{{input}}" := tolower({{ input }})) %>% 
    detect_types(!!id, !!input, util_general) %>% 
    detect_types(!!id, !!input, util_security) %>% 
    detect_types(!!id, !!input, util_crypto) %>% 
    detect_types(!!id, !!input, util_pwdmgr) %>% 
    detect_types(!!id, !!input, util_malware) %>% 
    detect_types(!!id, !!input, util_virus) %>% 
    detect_types(!!id, !!input, util_spam) 
  
  df <- df %>% 
    as.data.frame() %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(utility_all = sum(across(contains("util_")), na.rm = TRUE))
  
  if( sum_only == TRUE ){
    
    df <- df %>% select(-starts_with("util_"))
    
  } else { df }
  
  if( prob == TRUE ){
    
    tmp_df <- readme_raw_data %>% 
      as.data.frame() %>% 
      tidytext::unnest_tokens(word, !!input) %>%
      as_tidytable() %>% 
      tidytable::count.(!!id, name = "n_words")
    
  } else { df }
  
  df
  
}


  


