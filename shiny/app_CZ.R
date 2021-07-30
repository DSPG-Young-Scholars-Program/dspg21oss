if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(rsconnect)) install.packages("rsconnect", repos = "http://cran.us.r-project.org")
if(!require(shinycssloaders)) install.packages("shinycssloaders", repos = "http://cran.us.r-project.org")
if(!require(shinyjs)) install.packages("shinyjs", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")

if(!require(sjmisc)) install.packages("sjmisc", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(DT)) install.packages("DT", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(collapsibleTree)) install.packages("collapsibleTree", repos = "http://cran.us.r-project.org")


prettyblue <- "#232D4B"
navBarBlue <- '#427EDC'
#options(spinner.color = prettyblue, spinner.color.background = '#ffffff', spinner.size = 3, spinner.type = 7)

colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")

# data -----------------------------------------------------------

# make tree ####
df = read_csv("data_shiny/oss_software_types - dictionary.csv")
df_no_na <- df %>% 
  filter(!is.na(sourceforge_count)) %>% 
  filter(!is.na(fleming_primary)) %>%
  filter(!is.na(fleming_secondary)) %>%
  filter(main_type!="Undeveloped") %>% 
  mutate(main_type = replace(main_type, main_type=="" & summary_type!="Programming" & summary_type!="Other/Nonlisted Topic", "Other")) %>% 
  mutate(sub_type = replace(sub_type, sub_type=="" & summary_type!="Programming" & summary_type!="Other/Nonlisted Topic", "Other"))


# user -------------------------------------------------------------
ui <- navbarPage(title = "OSS",
                 selected = "overview",
                 theme = shinytheme("lumen"),
                 tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
                 useShinyjs(),
                 # main -----------------------------------------------------------
                 # tabPanel("Home", value = "home",
                 #          fluidRow(style = "margin: 6px;",
                 #                   align = "center",
                 #                   br("", style = "padding-top:10px;"),
                 #                   img(src = "uva-dspg-logo.jpg", class = "topimage", width = "20%", style = "display: block; margin-left: auto; margin-right: auto;"),
                 #                   br(""),
                 #                   h2(strong("Addressing Barriers to Health in Patrick County, Virginia"),
                 #                   br(""),
                 #                   h4("Data Science for the Public Good Program"),
                 #                   h4("University of Virginia"),
                 #                   h4("Biocomplexity Insititute"),
                 #                   br(),
                 #                   br(),
                 #                   br(),
                 #                   br(),
                 #                   br(),
                 #                   p(tags$small(em('Last updated: August 2020')))
                 #                   )
                 #          )
                 # ),
                 
                 # main -----------------------------------------------------------
                 tabPanel("Overview", value = "overview",
                          fluidRow(style = "margin: 2px;",
                                   align = "center",
                                   # br("", style = "padding-top:2px;"),
                                   # img(src = "uva-dspg-logo.jpg", class = "topimage", width = "20%", style = "display: block; margin-left: auto; margin-right: auto;"),
                                   br(""),
                                   h1(strong("Defining and Measuring the Universe of Open Source Software Innovation"),
                                      br(""),
                                      h4("Data Science for the Public Good Program"),
                                      h4("University of Virginia"),
                                      h4("Biocomplexity Insititute"),
                                      br()
                                   )
                          ),
                          fluidRow(style = "margin: 6px;",
                                   column(4,
                                          h2(strong("Big Picture")),
                                          p("Since the advent of the internet, software has become integral part of our 
                                            lived social realities. From the rise of mobile phones to social media apps, software 
                                            shapes how we interact with those around us as well as driving much of the econonic growth seen around the world. 
                                            Federal economic indicators developed by the National Center for Science & Engineering Statistics (NCSES) 
                                            do not currently do well in measuring the value of goods and services that do not have market transactions 
                                            (i.e., they are not captured in surveys nor are they in economic measures such as the Gross Domestic Product or GDP). 
                                            Although the NCSES does track some types of software development, 
                                            it is challenging to account for software that is developed outside of traditional business contexts. Moreover, while 
                                            current measures of innovation tend to rely on survey data, patent issues, trademarks approvals, intangible asset data, 
                                            or estimates of total factor productivity growth, these measures are either incomplete or 
                                            fail to capture innovation that is freely available to the public.")
                                          ),
                                   column(4,
                                          h2(strong("Project Overview")),
                                          p("To address this gap, the NCSES is interested in evaluating the economic and social impact of 
                                            Open Source Software (OSS) through the use of public administrative data. OSS refers to computer software with
                                            its source code shared with a license in which the copyright holder provides the rights to study,
                                            change, or distribute the software to anyone and for any purpose. Over the past few years, our team has aimed 
                                            to measure how much OSS is in use (stock), how much is created (flow), who is developing these tools (based on sectors, institutions, 
                                            and organizations) as well as how OSS is shared across these various institutions. In past Data Science for the Public Good projects, we developed procedures to 
                                            classify users into sectors, but very little work to date examines how different types of software are used within and across these sectors. 
                                            In this year’s OSS DSPG project, we will be collecting data from GitHub OSS repositories, classifying these repos into different OSS types, 
                                            and evaluating how these different types of software are used within and across economic sectors. 
                                            Developing a procedure to classify repositories into categories will allow the NCSES to better determine the effect that variations in software 
                                            may have on OSS contribution activity, collaboration tendencies in networked ecosystems, or on the overall cost of OSS projects.")
                                    ),
                                   column(4,
                                          h2(strong("Our Approach")),
                                          p("In this project, we study GitHub - the world's largest code hosting repository platform. The platform has roughly 40 million 
                                            users and 190 million repositories - many of which we have scraped using our own open source tools. Our dataset includes descriptive statistics on 
                                            10.2 million repositories as well as more detailed text data (READMEs) on around 157,000 projects (see Data)."), 
                                          tags$li("To classify GitHub projects into software types, we developed term-matching algorithms to allocate repos into various categories. This typology was borrowed from the Bureau of Economic Analysis and 
                                            Martin Fleming's (2021) proposed software categories that are generally useful for economists as well as more nuanced software categories. 
                                                  that we extracted from ", a(href = "https://sourceforge.net/", "SourceForge", target = "_blank"),", which are designed by and more arguably more useful for computer programmers to organize existing projects (see Software Types). 
                                                  For the 2021 DSPG summer project, we focused on 10 prominent OSS categories "), 
                                          tags$li("Third, we worked to validate that our approach generated accurate results by comparing manually validated repos to untagged repos through the use of Bidirectional Encodings 
                                                  through Representational Transformers (or ", a(href = "https://huggingface.co/transformers/model_doc/bert.html", "BERT", target = "_blank"),"). Generally, this approach allows us to embed sentences' meanings within a vector space to compare how similar 
                                            the content is using a cosine similarity metric."), 
                                          tags$li("Finally, we used node embeddings (or ", a(href = "https://snap.stanford.edu/node2vec/", "node2vec", target = "_blank"),") to compare the similarity of repository collaboration 
                                            networks based on common contributors. Like word embeddings, node embeddings place repos as vectors within a vector space with those that are 
                                            most similar closest to one another. By combing these strategies, we hope to infer software types based on their collaboration networks and improve our abilities to 
                                            classify GitHub repositories through a combination of these computational methods.")
                                          )
                                          ),
                          fluidRow(align = "center",
                                   p(tags$small(em('Last updated: July 2021'))))
                                   ),
                 
                 # data -----------------------------------------------------------
                 tabPanel("Data",
                          value = "socio",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Data Source and Collection"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   column(4,
                                          h4(strong("Data Source")),
                                          #h5(strong("GitHub")),
                                          p("Our data comes from", a(href = "https://www.github.com/", "GitHub", target = "_blank"),  
                                            "- the world's largest code hosting repository platform in the world. People develop, store, 
                                            and share programming projects as repositories on GitHub. As of January 2020, GitHub had more 
                                            than 40 million users and 190 million repositories - 28 million of which are in the public domain. 
                                            Other prominent code hosting platforms include GitLab (32M users), BitBucket (5M users), LaunchPad (4M users),
                                            and SourceForge (3.7M users). While other research projects, such as GHTorrent, GitHub Archive, and Software Heritage 
                                            have aimed to collect code development activity more broadly, we are interested in examining a subset of GitHub repositories 
                                            with licenses that explicitly designate them as open source. OSS licenses allow software to be freely used, modified, and shared, 
                                            providing value for that software to be replicated and repurposed for a wide variety of purposes.")
                                          ),
                                   column(6,
                                          
                                          h4(strong("Overaching Methodology")),
                                          p("To classify and examine GitHub repositories, we aimed to first collect a survey of all OSS repositories on GitHub in addition to 
                                            summaries of their commit activities and descriptive information such as repo descriptions and READMEs (see below). We then 
                                            developed algorithms that classified the repos into software types. This typology was borrowed from the Bureau of Economic Analysis and 
                                            Martin Fleming's (2021) proposed software categories that are generally useful for economists as well as more nuanced software categories 
                                            that we extracted from ", a(href = "https://sourceforge.net/", "SourceForge", target = "_blank"),", which are designed by and more arguably more useful for computer programmers to organize existing projects. Our initial classification effort was 
                                            predicated on a term-matching approach where we curated a nested dictionary with terms like 'tensorflow' or 'pytorch' mapping onto broader 
                                            cateogores like 'artificial intelligence/machine learning' and then aggregating up to 'software developmental tools' in our summary categories (see Software Types page 
                                            for more detils). For the 2021 DSPG summer project, we limited our curation of terms to 10 categories and then sought to validate 
                                            that our approach generated accurate results by comparing manually validated repos to untagged repos through the use of Bidirectional Encodings 
                                            through Representational Transformers (or ", a(href = "https://huggingface.co/transformers/model_doc/bert.html", "BERT", target = "_blank"),"). Generally, this approach allows us to embed sentences' meanings within a vector space to compare how similar 
                                            the content is using a cosine similarity metric. Similarly, we also decided to use node embeddings (or ", a(href = "https://snap.stanford.edu/node2vec/", "node2vec", target = "_blank"),") to compare the similarity of repository collaboration 
                                            networks based on common contributors. Like word embeddings, node embeddings place repos as vectors within a vector space with those that are 
                                            most similar closest to one another. By combing these strategies, we hope to infer software types based on their collaboration networks and improve our abilities to 
                                            classify GitHub repositories."),
                                          
                                          h4(strong("Repository Descriptive Data")),
                                          p("To establish our universe of OSS, we used a list of 29 Open-Source Initiative (OSI)-approved licenses 
                                            derived from the Ruby Gem Licensee on GitHub. Next, we developed an open source Julia package called", a(href = "https://uva-bi-sdad.github.io/GHOST.jl/dev/", "GHOST.jl", target = "_blank"), 
                                            "to scrape all the repos with these licenses, including the repository slug, license, description, primary language,
                                            and date created for 10,288,063 distinct repositories. Though this paper focuses on repository data, 
                                            GHOST.jl also has the capacity to collect targeted user and activity data for all OSS repos on GitHub, 
                                            which we hope to continue to collecting to monitor the impact of open source in the 
                                            years to come. For this project, our main goal was to use the repository slug (owner/repo name), descriptions and 
                                            commit histories to learn more about the types of software being developed on GitHub's platform by classifying the projects 
                                            through the use of term-matching, sentence embeddings (i.e. BERT), and node embeddings (i.e. node2vec)."),
                               
                                            
                                          h4(strong("Repository Popularity and READMEs Data")),
                                          p("To supplement these repository descriptive data, we also developed two Python scripts to scrape repository ", 
                                            a(href = "https://github.com/DSPG-Young-Scholars-Program/dspg21oss/blob/main/src/01_scrape_readmes/03_scrape_repo_stats.ipynb", "popularity statistics", target = "_blank"),
                                            " and ", a(href = "https://github.com/DSPG-Young-Scholars-Program/dspg21oss/blob/main/src/01_scrape_readmes/02_scrape_readmes_requests_final.ipynb", "READMEs", target = "_blank"), 
                                            " for the top 250,000 repos ranked by the number of commits. The collection of
                                            repository popularity statistics (stars, watchers, forks, and topics) was aided by the use of the", a(href = "https://pygithub.readthedocs.io/en/latest/introduction.html", "pyGithub.", target = "_blank"),
                                            "These data tell us more about high-impact projects and allow us to compare them with those that have the most commit activity.
                                            To collect repository READMEs, we used Python's", a(href = "https://docs.python-requests.org/en/master/", "requests", target = "_blank"), 
                                            " and ", a(href = "https://www.crummy.com/software/BeautifulSoup/bs4/doc/", "Beautiful Soup", target = "_blank"), " modules to scrape the raw text of READMEs from 
                                            these repositories before joining these data sources together. Once we merged these data,
                                            our sample shrunk to 157,538 entries that had valid description and README entries. These data, in turn, were used to 
                                            classify software types using our term-matching and sentence embeddings."),
                                          
                                          h4(strong("Repository Network Data")),
                                          p("Finally, we decided to use a form of graph representational learning called node embeddings (specifically node2vec) to
                                            examine the similarity of projects based on their collaboration networks. Drawing from the commit activity data scraped 
                                            using the GHOST.jl package, we constructed a repository collaboration network where the nodes represent repositories and 
                                            the edges correspond to the number of common collaborators between those repos. Given the size of the full network and the 
                                            computational limitations it presents for conducting node embedding, these collaboration networks were limited to the 
                                            repositories in the 157K subset mentioned above. After isolate nodes were removed, this network ended up being comprised of 416 nodes and 5,237 edges.")
                                   ))
                                   ),
                 
                 # software type, Cierra-----------------------------------------------------------
                 tabPanel("Software Types", value = "data",
                          h1(strong("Software Types"), align = "center"),
                          fluidRow(style = "margin: 6px;",
                                   column(4,
                                          h4(strong("Classification")),
                                          p("The main objective of our project is to classify GitHub repositories
                                            into software types so that the NCSES and other federal statisticians 
                                            can better understand the economic evaluation of labor costs and 
                                            software impact. To classify GitHub projects into software types, we 
                                            drew from two primary schemas integrating them into general “summary 
                                            types” and more nuanced “main types” and “sub types.”"),
                                          p("The summary types 
                                            include Application Software, Programming Software, System Software, 
                                            Utility Software, and General Topics (Fleming 2021), which are meant 
                                            to inform broader understandings of economic evaluation for federal 
                                            statisticians. The more nuanced main and sub types, on the other hand, 
                                            derive from categorizations provided by another prominent open source 
                                            code hosting platform named", 
                                            a(href = "https://sourceforge.net/", " SourceForge. ", target = "_blank"),
                                            "While GitHub repos do, at times, have topics listed, the site 
                                            does not organize projects by topics. SourceForge’s more 
                                            sophisticated and wide-ranging classification schema allowed us 
                                            to focus on smaller subcategories that could be aggregated into 
                                            the broader summary types later on."), 
                                          p("Using both schemas offers insight to 
                                            economists interested in evaluating the costs of broader, more general categories 
                                            as well as researchers interested in specialized, more specific categories. 
                                            Below, we used R’s collapsibleTree
                                            package to visualize how these three levels of categorization fit together."),
                                          h5(strong("Project Focus Software Categories")),
                                          p("We chose to focus in on some of the top programming language software on GitHub and a
                                            number of Application and Topic software categories. Below is a list of the specific 
                                            software categories we focused on:"),
                                          tags$ul(
                                            tags$li("R"), 
                                            tags$li("Python"), 
                                            tags$li("C"),
                                            tags$li("Javascript"),
                                            tags$li("Java"),
                                            tags$li("PHP"),
                                            tags$li("Data Visualization"),
                                            tags$li("Database Management"),
                                            tags$li("Artificial Intelligence/Machine Learning"),
                                            tags$li("Blockchain")
                                          )
                                          
                                          ),
                                   column(8, 
                                          h4(strong("Collapsible Trees")),
                                          h6(strong("Fleming Classification Schema")),
                                          collapsibleTreeOutput("tree_flemming"),
                                          h6(strong("Source Forge Classification Schema")),
                                          collapsibleTreeOutput("tree_sf")
                                   )
                                          )
                          ),
                                            
                 
                 
                 # Classification Method-----------------------------------------------------------
                 tabPanel("Classification Methods", value = "data",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Classification Methods"), align = "center"),
                                   br()
                          ),
                          tabsetPanel(
                            tabPanel("Supervised Text Mining",
                                     h3("", align = "center"),
                                     br("")),
                            tabPanel("Sentence Embeddings Estimation",  
                                     h3(strong(""), align = "center"),
                                     fluidRow(style = "margin: 6px;",
                                              column(6,
                                                     h4(strong("Methods")),
                                                     h5(strong("I. Formulate Software Type Sentence Corpus")),
                                                     p("For each software type, we manually validated repository classifications. We prioritize the repositories
                                                       with large number of stars for validation and obtained about fifty validated repositories for each software type.
                                                       And we use the descriptions of these validated repositories as our corpus. We have one sentence corpus for each 
                                                       software type."),
                                                     h5(strong("II. Calculate Sentence Embedding")),
                                                     p("We computed sentence embeddings using",
                                                       a(href = "https://www.sbert.net/", "Sentence BERT (SBERT)", target = "_blank"), 
                                                       "There are many pretrained models. We used ", 
                                                        a(href = "https://www.sbert.net/docs/pretrained_models.html", " paraphrase-mpnet-base-v2", target = "_blank"), 
                                                          "to embed repository descriptions in our corpus and repository descriptions of unlabelled repositories. 
                                                       We chose this model because it has the highest quality."),
                                                     
                                                     h5(strong("III. Compare Repo Descriptions to Sentence Corpus")),
                                                    p("For each software type, we compared the similarity between sentence embeddings of our corpus to the remaining repository descriptions using cosine-similarity. 
                                                        Cosine-similarity score ranges from 0 to 1, higher the score, more similar two sentences are to each 
                                                        other. For repository (with a one-sentence repository description), we identified the top ten most similar 
                                                        sentences from our sentence corpus and obtained their cosine-similarity scores. We then took the median of the ten scores
                                                        and obtained an embedding score for each repository, indicating how similar the repository is to the corresponding software type. We 
                                                        classify a repository with an embedding score that is 2 standard deviation above the mean as the corresponding software type." )
                                                     
                                                  ),
                                       column(6, 
                                              h4(strong("Results")),
                                              img(src = "bert_classification.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "800px")
                                              
                                        )
                                    )
                                     
                            ),
                            tabPanel("Network Embeddings",  
                                     h3(strong(""), align = "center")
                            )
                          )
                 ),
                 # contact -----------------------------------------------------------
                 tabPanel("Contact", value = "contact",
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   h1(strong("Contact"), align = "center"),
                                   br(),
                                   h4(strong("UVA Data Science for the Public Good")),
                                   p("The", a(href = 'https://biocomplexity.virginia.edu/social-decision-analytics/dspg-program', 'Data Science for the Public Good (DSPG) Young Scholars program', target = "_blank"), 
                                     "is a summer immersive program held at the", a(href = 'https://biocomplexity.virginia.edu/social-decision-analytics', 'University of Virginia Biocomplexity Institute’s Social and Decision Analytics division (SDAD).'), 
                                     "In its seventh year, the program engages students from across the country to work together on projects that address state, federal, and local government challenges around 
                                     critical social issues relevant in the world today. DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences 
                                     to determine how information generated within every community can be leveraged to improve quality of life and inform public policy. For more information on program 
                                     highlights, how to apply, and our annual symposium, please visit", a(href = 'https://biocomplexity.virginia.edu/social-decision-analytics/dspg-program', 'the official Biocomplexity DSPG website.', target = "_blank")),
                                   p("", style = "padding-top:10px;")
                                   ),
                          fluidRow(style = "margin-left: 20px; margin-right: 20px;",
                                   column(6, align = "center",
                                          h4(strong("DSPG Team Members")),
                                          img(src = "Crystal.jpeg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "Cierra.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "Stephanie.png", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          p(a(href = 'https://www.linkedin.com/in/crystal-zang', 'Crystal Zang', target = '_blank'), "(University of Pittsburgh Graduate School of Public Health, Biostatistics);",
                                            a(href = '', 'Cierra Oliveira', target = '_blank'), "(Clemson University, Computing and Applied Sciences);" ,
                                            a(href = '', 'Stephanie Zhang', target = '_blank'), "(University of Virginia, Mathematics (Probability/Statistics), Sociology);"),
                                          p("", style = "padding-top:10px;")
                                   ),
                                   column(6, align = "center",
                                          h4(strong("UVA SDAD Team Members")),
                                          img(src = "Brandon.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "Gizem.png", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          p(a(href = "https://biocomplexity.virginia.edu/person/brandon-kramer", 'Brandon Kramer', target = '_blank'), "(Postdoctoral Research Associate);",
                                            a(href = "https://biocomplexity.virginia.edu/person/gizem-korkmaz", 'Gizem Korkmaz', target = '_blank'), "(Research Associate Professor);"),
                                          p("", style = "padding-top:10px;")
                                   )
                          ),
                          fluidRow(#style = "margin-left: 100px; margin-right: 100px;",
                                        #style = "center",
                                       column(12, align = "center",
                                       h4(strong("Project Stakeholders")),
                                       img(src = "Carol.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                       img(src = "Ledia.png", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                       p(a(href = '', 'Carol Robbins' , target = '_blank'), "(NCSES, Senior Analyst);"),
                                       p(a(href = '', 'Ledia Guci', target = '_blank'), "(NCSES, Science Resource Analyst);"),
                                       p("", style = "padding-top:10px;")
                                        )
                                   ),
                          fluidRow(style = "margin-left: 20px; margin-right: 20px;",
                                   style = "center",
                                   h4(strong("Acknowledgments")),
                                   p(" In addition to our appreciation to Carol Robbins, Ledia Guci, and Bayoán Santiago Calderón for their continued support of work on OSS,
                                     we also want to thank Martin Fleming for joining us to talk about software classification.")
                          )), 
                 inverse = T)



# server -----------------------------------------------------------
server <- function(input, output, session) {
  # Run JavaScript Code
  #runjs(jscode)
  
  # Tab: Software Type, collapsible tree -----------------------------------------------------
  output$tree_flemming <- renderCollapsibleTree({
    collapsibleTreeSummary(df,
                           hierarchy = c("fleming_primary", "fleming_secondary"),
                           width=800, height = 800, 
                           root = "Software Types", 
                           fontSize = 12,
                           zoomable = FALSE,
                           fillFun = colorspace::heat_hcl)
  })
  
  output$tree_sf <- renderCollapsibleTree({
    collapsibleTreeSummary(df_no_na,
                           hierarchy = c("summary_type", "main_type", "sub_type"),
                           width=800, height = 1000,  
                           root = "Software Types", 
                           fontSize = 12,
                           zoomable = FALSE,
                           attribute = "sourceforge_count",
                           fillFun = colorspace::heat_hcl)
  })
  
  var <- reactive({
    input$sociodrop
  })
  
}

shinyApp(ui = ui, server = server)