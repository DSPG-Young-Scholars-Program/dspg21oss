
# get_dictionary_terms pulls in the oss_software_types dictionary terms 

get_dictionary_terms <- function(summary_type, main_type, sub_type){
  
  library("dplyr")
  library("readr")
  library("tidyr")
  
  if( missing(main_type) & missing(sub_type) ){ 
    
    # this is to pull all of the terms for a summary_type 
    
    summary_type <- enquo(summary_type)
    setwd("~/git/dspg21oss/docs/")
    software_types <- readr::read_csv("oss_software_types - dictionary.csv", col_types = cols()) %>% 
      unnest_legacy(terms = strsplit(terms, "\\|")) 
    software_terms <- software_types %>% 
      filter(summary_type == {{ summary_type }})
    software_terms <- na.omit(software_terms$terms)
    software_terms
    
  } else if (missing(summary_type) & missing(sub_type) ){
    
    # this is to pull all of the terms for a main_type  
    
    main_type <- enquo(main_type)
    setwd("~/git/dspg21oss/docs/")
    software_types <- readr::read_csv("oss_software_types - dictionary.csv", col_types = cols()) %>% 
      unnest_legacy(terms = strsplit(terms, "\\|")) 
    software_terms <- software_types %>% 
      filter(main_type == {{ main_type }}) 
    software_terms <- na.omit(software_terms$terms)
    software_terms
    
  } else if (missing(summary_type) & missing(main_type)) {
    
    sub_type <- enquo(sub_type)
    setwd("~/git/dspg21oss/docs/")
    software_types <- readr::read_csv("oss_software_types - dictionary.csv", col_types = cols()) %>% 
      unnest_legacy(terms = strsplit(terms, "\\|")) 
    software_terms <- software_types %>% 
      filter(sub_type == {{ sub_type }})
    software_terms <- na.omit(software_terms$terms)
    software_terms
    
  } else if ( missing(summary_type) & sub_type == FALSE ){
    
    # this is to pull all of the terms for main_type general category   
    
    main_type <- enquo(main_type)
    setwd("~/git/dspg21oss/docs/")
    software_types <- readr::read_csv("oss_software_types - dictionary.csv", col_types = cols()) %>% 
      unnest_legacy(terms = strsplit(terms, "\\|")) 
    software_terms <- software_types %>% 
      filter(main_type == {{ main_type }} & is.na(sub_type)) 
    software_terms <- na.omit(software_terms$terms)
    software_terms
    
  } else if ( main_type == FALSE & sub_type == FALSE ){ 
    
    # this is to pull all of the terms for a summary_type when all else is NA  
    
    summary_type <- enquo(summary_type)
    setwd("~/git/dspg21oss/docs/")
    software_types <- readr::read_csv("oss_software_types - dictionary.csv", col_types = cols()) %>% 
      unnest_legacy(terms = strsplit(terms, "\\|")) 
    software_terms <- software_types %>% 
      filter(is.na(main_type) & is.na(sub_type))
    software_terms <- na.omit(software_terms$terms)
    software_terms
    
  } else {
    
    "ERROR: Variables were not correctly. Please try again."
    
  }
  
}


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
    as_tidytable() %>% 
    tidytext::unnest_tokens(word, !!input) %>%
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
    as_tidytable() %>% 
    tidytext::unnest_tokens(word, !!input) %>%
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


# detect_blockchain_sw

detect_blockchain_sw <- function(df, id, input, sum_only = FALSE, prob = FALSE){
  
  library("dplyr")
  library("readr")
  library("tidyr")
  library("readr")
  library("stringr")
  library("data.table")
  library("tidytext")
  library("tidytable")
  
  # load dictionaries for top-50 languages based on: 
  # https://madnight.github.io/githut/#/pull_requests/2021
  
  # top statistical languages 
  app_blockchain <- get_dictionary_terms(main_type = "Blockchain")
  app_cryptocurrency <- get_dictionary_terms(sub_type = "Cryptocurrency")
  
  id <- enquo(id)
  input <- enquo(input)
  
  df <- df %>% 
    as_tidytable() %>% 
    tidytable::mutate.("{{input}}" := tolower({{ input }})) %>% 
    detect_types(!!id, !!input, app_blockchain) %>% 
    detect_types(!!id, !!input, app_cryptocurrency) 
  
  df <- df %>% 
    as.data.frame() %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(app_blockchain_all = sum(across(contains("app_")), na.rm = TRUE))
  
  if( sum_only == TRUE ){
    
    df <- df %>% select(-starts_with("app_"))
    
  } else { df }
  
  if( prob == TRUE ){
    
    tmp_df <- readme_raw_data %>% 
      as_tidytable() %>% 
      tidytext::unnest_tokens(word, !!input) %>%
      tidytable::count.(!!id, name = "n_words")
    
  } else { df }
  
  df
  
}

# databases 

detect_database_sw <- function(df, id, input, sum_only = FALSE, prob = FALSE){
  
  library("dplyr")
  library("readr")
  library("tidyr")
  library("readr")
  library("stringr")
  library("data.table")
  library("tidytext")
  library("tidytable")
  
  # load dictionaries for top-50 languages based on: 
  # https://madnight.github.io/githut/#/pull_requests/2021
  
  # top statistical languages 
  app_db_general <- get_dictionary_terms(main_type = "Security")
  app_db_frontends <- get_dictionary_terms(sub_type = "Front-Ends")
  app_db_servers <- get_dictionary_terms(sub_type = "Database Engines/Servers")
  
  id <- enquo(id)
  input <- enquo(input)
  
  df <- df %>% 
    as_tidytable() %>% 
    tidytable::mutate.("{{input}}" := tolower({{ input }})) %>% 
    detect_types(!!id, !!input, app_db_general) %>% 
    detect_types(!!id, !!input, app_db_frontends) %>% 
    detect_types(!!id, !!input, app_db_servers) 
  
  df <- df %>% 
    as.data.frame() %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(app_database_all = sum(across(contains("app_db_")), na.rm = TRUE))
  
  if( sum_only == TRUE ){
    
    df <- df %>% select(-starts_with("app_db_"))
    
  } else { df }
  
  if( prob == TRUE ){
    
    tmp_df <- readme_raw_data %>% 
      as_tidytable() %>% 
      tidytext::unnest_tokens(word, !!input) %>%
      tidytable::count.(!!id, name = "n_words")
    
  } else { df }
  
  df
  
}

# business software 

detect_business_sw <- function(df, id, input, sum_only = FALSE, prob = FALSE){
  
  library("dplyr")
  library("readr")
  library("tidyr")
  library("readr")
  library("stringr")
  library("data.table")
  library("tidytext")
  library("tidytable")
  
  # load dictionaries for top-50 languages based on: 
  # https://madnight.github.io/githut/#/pull_requests/2021
  
  # top statistical languages 
  app_bus_general <- get_dictionary_terms(main_type = "Office/Business")
  app_bus_enterprise <- get_dictionary_terms(sub_type = "Enterprise")
  app_bus_financial <- get_dictionary_terms(sub_type = "Financial")
  app_bus_scheduling <- get_dictionary_terms(sub_type = "Scheduling")
  app_bus_projmgmt <- get_dictionary_terms(sub_type = "Project Management")
  app_bus_timetrack <- get_dictionary_terms(sub_type = "Time Tracking")
  app_bus_todolists <- get_dictionary_terms(sub_type = "To-Do Lists")
  app_bus_offsuites <- get_dictionary_terms(sub_type = "Office Suites")
  app_bus_ecomm <- get_dictionary_terms(sub_type = "E-Commerce/Shopping")
  app_bus_knowmgmt <- get_dictionary_terms(sub_type = "Knowledge Management")
  app_bus_dsktoppub <- get_dictionary_terms(sub_type = "Desktop Publishing")
  app_bus_reportgen <- get_dictionary_terms(sub_type = "Report Generators")
  app_bus_modeling <- get_dictionary_terms(sub_type = "Modelling")
  app_bus_insurance <- get_dictionary_terms(sub_type = "Insurance")
  app_bus_mrktauto <- get_dictionary_terms(sub_type = "Marketing Automation")
  
  id <- enquo(id)
  input <- enquo(input)
  
  df <- df %>% 
    as_tidytable() %>% 
    tidytable::mutate.("{{input}}" := tolower({{ input }})) %>% 
    detect_types(!!id, !!input, app_bus_general) %>% 
    detect_types(!!id, !!input, app_bus_enterprise) %>% 
    detect_types(!!id, !!input, app_bus_financial) %>% 
    detect_types(!!id, !!input, app_bus_scheduling) %>% 
    detect_types(!!id, !!input, app_bus_projmgmt) %>% 
    detect_types(!!id, !!input, app_bus_timetrack) %>% 
    detect_types(!!id, !!input, app_bus_todolists) %>% 
    detect_types(!!id, !!input, app_bus_offsuites) %>% 
    detect_types(!!id, !!input, app_bus_ecomm) %>% 
    detect_types(!!id, !!input, app_bus_knowmgmt) %>% 
    detect_types(!!id, !!input, app_bus_dsktoppub) %>% 
    detect_types(!!id, !!input, app_bus_reportgen) %>% 
    detect_types(!!id, !!input, app_bus_modeling) %>% 
    detect_types(!!id, !!input, app_bus_insurance) %>% 
    detect_types(!!id, !!input, app_bus_mrktauto) 
  
  df <- df %>% 
    as.data.frame() %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(app_business_all = sum(across(contains("app_bus_")), na.rm = TRUE))
  
  if( sum_only == TRUE ){
    
    df <- df %>% select(-starts_with("app_bus_"))
    
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
















# save this for later 


detect_programming_sw <- function(df, id, input, sum_only = FALSE, prob = FALSE){
  
  library("dplyr")
  library("readr")
  library("tidyr")
  library("readr")
  library("stringr")
  library("data.table")
  library("tidytext")
  library("tidytable")
  
  # load dictionaries for top-50 languages based on: 
  # https://madnight.github.io/githut/#/pull_requests/2021
  # general languages 
  prog_general <- get_dictionary_terms(summary_type = "Programming")
  prog_go <- get_dictionary_terms(sub_type = "Go")
  prog_ruby <- get_dictionary_terms(sub_type = "Ruby")
  prog_cpp <- get_dictionary_terms(sub_type = "C++")
  prog_csharp <- get_dictionary_terms(sub_type = "C#")
  prog_clang <- get_dictionary_terms(sub_type = "C")
  prog_objc <- get_dictionary_terms(sub_type = "Objective-C")
  prog_objcpp <- get_dictionary_terms(sub_type = "Objective-C++")
  prog_rust <- get_dictionary_terms(sub_type = "Rust")
  prog_perl <- get_dictionary_terms(sub_type = "Perl")
  prog_swift <- get_dictionary_terms(sub_type = "Swift") # from apple 
  prog_haskell <- get_dictionary_terms(sub_type = "Haskell")
  prog_groovy <- get_dictionary_terms(sub_type = "Groovy") 
  prog_clojure <- get_dictionary_terms(sub_type = "Clojure") 
  prog_ocaml <- get_dictionary_terms(sub_type = "OCaml")
  prog_dotnet <- get_dictionary_terms(sub_type = ".NET")
  prog_vbdotnet <- get_dictionary_terms(sub_type = "Virtual Basic .NET")
  prog_fsharp <- get_dictionary_terms(sub_type = "F#")
  prog_fortran <- get_dictionary_terms(sub_type = "Fortran") # from ibm 
  prog_commonlisp <- get_dictionary_terms(sub_type = "Common Lisp")
  prog_coq <- get_dictionary_terms(sub_type = "Coq") # formal proof mgmt system
  # top web, app and gaming development languages 
  prog_javascript <- get_dictionary_terms(sub_type = "JavaScript")
  prog_java <- get_dictionary_terms(sub_type = "Java")
  prog_typescript <- get_dictionary_terms(sub_type = "TypeScript")
  prog_php <- get_dictionary_terms(sub_type = "PHP")
  prog_html <- get_dictionary_terms(sub_type = "HTML")
  prog_css <- get_dictionary_terms(sub_type = "CSS")
  prog_dart <- get_dictionary_terms(sub_type = "Dart") # from google 
  prog_kotlin <- get_dictionary_terms(sub_type = "Kotlin")
  prog_lua <- get_dictionary_terms(sub_type = "Lua")
  prog_coffeescript <- get_dictionary_terms(sub_type = "CoffeeScript")
  prog_dm <- get_dictionary_terms(sub_type = "DM") # used for games 
  prog_erlang <- get_dictionary_terms(sub_type = "Erlang") # ecommerce, banking, IMing
  prog_elixer <- get_dictionary_terms(sub_type = "Elixir") # used with erlang 
  prog_jsonnet <- get_dictionary_terms(sub_type = "Jsonnet") # structures json data 
  prog_elm <- get_dictionary_terms(sub_type = "Elm")
  prog_webassembly <- get_dictionary_terms(sub_type = "WebAssembly")
  prog_purescript <- get_dictionary_terms(sub_type = "PureScript")
  # top statistical languages 
  prog_python <- get_dictionary_terms(sub_type = "Python")
  prog_scala <- get_dictionary_terms(sub_type = "Scala")
  prog_rlang <- get_dictionary_terms(sub_type = "R")
  prog_matlab <- get_dictionary_terms(sub_type = "MATLAB")
  prog_stata <- get_dictionary_terms(sub_type = "Stata")
  prog_sas <- get_dictionary_terms(sub_type = "SAS")
  prog_julia <- get_dictionary_terms(sub_type = "Julia")
  # shell languages 
  prog_shell <- get_dictionary_terms(sub_type = "Shell")
  prog_powershell <- get_dictionary_terms(sub_type = "PowerShell")
  # text editing and language formatting 
  prog_vim <- get_dictionary_terms(sub_type = "Vim Script")
  prog_emacslisp <- get_dictionary_terms(sub_type = "Emacs Lisp")
  prog_roff <- get_dictionary_terms(sub_type = "Roff") # lang formatting
  # top database 
  prog_sql <- get_dictionary_terms(sub_type = "SQL")
  prog_tsql <- get_dictionary_terms(sub_type = "T-SQL")
  # apis 
  prog_raml <- get_dictionary_terms(sub_type = "RAML")
  # hardware describers/file organizers   
  prog_sysverilog <- get_dictionary_terms(sub_type = "SystemVerilog")
  prog_verilog <- get_dictionary_terms(sub_type = "Verilog")
  prog_puppet <- get_dictionary_terms(sub_type = "Puppet")
  
  id <- enquo(id)
  input <- enquo(input)
  
  df <- df %>% 
    as_tidytable() %>% 
    tidytable::mutate.("{{input}}" := tolower({{ input }})) %>% 
    
    
    # web, apps, and games 
    detect_types(!!id, !!input, prog_javascript) %>% 
    detect_types(!!id, !!input, prog_java) %>% 
    detect_types(!!id, !!input, prog_typescript) %>% 
    detect_types(!!id, !!input, prog_php) %>% 
    detect_types(!!id, !!input, prog_html) %>% 
    detect_types(!!id, !!input, prog_css) %>% 
    detect_types(!!id, !!input, prog_dart) %>% 
    detect_types(!!id, !!input, prog_kotlin) %>% 
    detect_types(!!id, !!input, prog_lua) %>% 
    detect_types(!!id, !!input, prog_coffeescript) %>% 
    detect_types(!!id, !!input, prog_dm) %>% 
    detect_types(!!id, !!input, prog_erlang) %>% 
    detect_types(!!id, !!input, prog_elixer) %>% 
    detect_types(!!id, !!input, prog_jsonnet) %>% 
    detect_types(!!id, !!input, prog_elm) %>% 
    detect_types(!!id, !!input, prog_webassembly) %>% 
    detect_types(!!id, !!input, prog_purescript) %>% 
    # statistical 
    detect_types(!!id, !!input, prog_python) %>% 
    detect_types(!!id, !!input, prog_scala) %>% 
    detect_types(!!id, !!input, prog_rlang) %>% 
    detect_types(!!id, !!input, prog_matlab) %>% 
    detect_types(!!id, !!input, prog_stata) %>% 
    detect_types(!!id, !!input, prog_sas) %>% 
    detect_types(!!id, !!input, prog_julia)
  
  df <- df %>% 
    as.data.frame() %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(system_all = sum(across(contains("sys_")), na.rm = TRUE))
  
  if( sum_only == TRUE ){
    
    df <- df %>% select(-starts_with("sys_"))
    
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




  


