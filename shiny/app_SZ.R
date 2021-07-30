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
options(spinner.color = prettyblue, spinner.color.background = '#ffffff', spinner.size = 3, spinner.type = 7)

colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")

# data -----------------------------------------------------------

# make tree ####
df = read_csv("data_shiny/oss_software_types - dictionary.csv")
df_no_na <- df %>% 
  filter(!is.na(sourceforge_count)) %>% 
  filter(!is.na(fleming_primary)) %>%
  filter(!is.na(fleming_secondary)) %>%
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
                                          h2(strong("Project Background")),
                                          p(strong("The problem."), "Rural counties often face challenges in providing health care access to their residents given limited", a(href = "https://www.ruralhealthinfo.org/topics/hospitals", "health facilities", target = "_blank"), 
                                            "available, lack of broadband infrastructure that makes it difficult to provide", a(href = "https://www.ruralhealthinfo.org/topics/telehealth", "telemedicine access", target = "_blank"), "or communicate health information, and individual-level", 
                                            a(href = "https://www.ruralhealthinfo.org/topics/social-determinants-of-health", "inequalities", target = "_blank"), "that pose barriers to health care use and health 
                                            behaviors. Identifying areas of high need or potential solutions may also be difficult for rural areas without adequate resources to acquire, analyze, and interpret 
                                            relevant data."),
                                          p(),
                                          p(strong("The setting."), a(href = "https://www.co.patrick.va.us/", "Patrick County", target = "_blank"), "is a rural area in Virginia’s Central Piedmont, bordering North Carolina, 
                                            with a declining population of approximately 17,600 people. Like many other rural areas in the United States, Patrick County is having difficulty meeting its residents’ health and quality of life needs. 
                                            The county’s", a(href = "https://www.countyhealthrankings.org/app/virginia/2019/rankings/patrick/county/outcomes/overall/snapshot", "doctor to patient ratios", target = "_blank"), 
                                            "of 3,530 to 1 for primary care providers, 8,840 to 1 for dentists, and 2,520 to 1 for mental health providers are 3- 
                                            to 8-times higher than statewide, and the county’s only hospital closed in 2017. At the same time, the median income for Patrick County residents is $42,900, 
                                            46% of children living in the county are eligible for free or reduced-price school lunch, and 12% of residents are food insecure."),
                                          p(),
                                          p(strong("The project."), "This University of Virginia", a(href = "https://biocomplexity.virginia.edu/social-decision-analytics", "Biocomplexity Institute", target = "_blank"), 
                                            "Data Science for Public Good (DSPG) project aimed to build local capacity, leverage social and data science to address current and future resident well-being, and enhance 
                                            data-driven decision making about rural health in Patrick County, Virginia.")
                                          ),
                                   column(4,
                                          h2(strong("Our Work")),
                                          p("Our research team worked closely with Patrick County Extension Office, Virginia Department of Health, and Healthy Patrick County coalition stakeholders 
                                            to identify the county’s priority challenges in the area of health. The research team reviewed a prior", a(href = "https://www.vdh.virginia.gov/west-piedmont/2020/05/27/patrick-county-health-needs-improvement-plan-completed/", 
                                                                                                                                                       "community health assessment,", target = "blank"), a(href = "https://www.pubs.ext.vt.edu/VCE/VCE-596/VCE-596-75/VCE-1002-75.html", "situation analysis", target = "_blank"),
                                            "relevant funding applications, and held a listening meeting with stakeholders to identify these challenges. Lack of 
                                            data on health care access, food access as related to diabetes and heart disease prevalence, older adult health, and digital connectivity that would facilitate 
                                            access to telemedicine emerged as key problems where providing actionable insights could address barriers to Patrick County residents’ health."),
                                          p(),
                                          p("We implemented the", a(href = "https://doi.org/10.1162/99608f92.2d83f7f5", "data science framework", target = "_blank"), "and identified, acquired, profiled, and used 
                                            publicly available data to provide Patrick County with data-driven resources in each of the four priority areas. We:"),
                                          tags$li("Provided census tract- and census block group-level maps of Patrick County residents'", strong("sociodemographic and socioeconomic characteristics,"), " highlighting underprivileged areas."),
                                          tags$li("Created census tract-level maps on", strong("older adult health"), "to show the geographic distribution of older adults in the county by gender and
                                                  type of disability, identifying areas where providing telehealth or travelling preventive care services may be particularly important."),
                                          tags$li("Mapped residents'", strong("computing device and internet access"), "at census block group level, and constructed 10- and 15-minute isochrones (areas of equal travel time) from households to free
                                                  wifi hotspots to highlight internet gaps that could suggest where new wi-fi hotspots could be optimally placed to provide internet access to more residents."),
                                          tags$li("Calculated and mapped", strong("emergency medical service (EMS) station coverage"), "of households within 8-, 10-, and 12-minute travel times, identifying areas difficult to reach within 
                                                  standard EMS travel thresholds."),
                                          tags$li("Constructed", strong("food access"), "maps by census tract, 10- and 15-minute isochrones from households to grocery stores and farmers markets, and maps of food security resources in the county,
                                                  highlighting food deserts and areas that could benefit from programs facilitating access to fresh produce."),
                                          p(),
                                          p("This dashboard compiles our findings and allows extension professionals, stakeholders, and other users to explore the information interactively.")
                                          ),
                                   column(4,
                                          h2(strong("Dashboard Aims")),
                                          p("Our dashboard is aimed at:"),
                                          p(strong("Patrick County extension professionals and the communities they serve."), "Information available through the interface helps extension 
                                            agents identify areas where residents may not have access to internet, or areas with a high smartphone ownership share, suggesting what channels agents may 
                                            want to use to disseminate health-related information most effectively. Information on older adult populations and grocery store access can help extension agents 
                                            better understand where underserved populations live and how to advocate on their behalf."),
                                          p(strong("Local health-related agencies and departments seeking data insights to inform their decision-making."), "For local stakeholders, identifying broadband 
                                            access gaps that limit access to telemedicine, grocery store access gaps, and areas with high proportions of older adults with independent living difficulty can suggest 
                                            optimal locations for placing free wifi hotspots, providing grocery delivery services, devising mobile health unit routes, or can inform other solutions that would benefit 
                                            a broad population base."),
                                          p(strong("State government representatives in the Virginia Department of Health and the State Office of Rural Health."), "These and similar stakeholders may 
                                            need small or rural area-specific insights that Centers for Disease Control and other county-level datasets cannot provide.")
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
                                          h5(strong("GitHub")),
                                          p("Our data source is Github,which is the largest and most advanced development
                                            platform in the world. People develop, store, 
                                            and share programming packages as repositories on GitHub.")
                                         ),
                                   column(6,
                                            h4(strong("Repository Descriptive Data")),
                                          p("We first established the universe of OSS as any GitHub repository with one of 29
                                            Open-Source Initiative (OSI)-approved licenses. Next, we developed an open source Julia package called GHOST.jl  
                                            to scrape all the repos with these licenses, including the repository slug, license, description, primary language,
                                              and date created for 10,288,063 distinct repositories (as of June 1, 2021). While this paper focuses on repository 
                                              data, GHOST.jl also has the capacity to collect targeted user and activity data for all OSS repos on GitHub 
                                        (Santiago-Calderon et al. 2021). "),
                               
                                            
                                          h4(strong("Repository Popularity Statistics and READMEs")),
                                          p("To supplement these repository descriptive data, we also developed two Python scripts to scrape repository popularity 
statistics and READMEs from the top 250,000 repos ranked by the number of commits. The collection of
                                            repository popularity statistics (stars, watchers, forks, and topics) was aided by the use of the pyGitHub (Jacques 2021). 
                                            On the other hand, we used the requests and BeautifulSoup modules to scrape the raw text of READMEs from these repositories 
                                            (Reitz 2020; Richardson 2007).  ")
                                   ))
                                   ),
                 
                 # software type, Cierra-----------------------------------------------------------
                 tabPanel("Software Types", value = "data",
                          h1(strong("Software Types"), align = "center"),
                          fluidRow(style = "margin: 6px;",
                                   column(6,
                                          h4(strong("Classification")),
                                          p("The main objective of our project is to classify GitHub repositories
                                            into software types so that the NCSES and other federal statisticians 
                                            can better understand the economic evaluation of labor costs and 
                                            software impact. To classify GitHub projects into software types, we 
                                            drew from two primary schemas integrating them into general “summary 
                                            types” and more nuanced “main types” and “sub types.” The summary types 
                                            include Application Software, Programming Software, System Software, 
                                            Utility Software, and General Topics (Fleming YEAR), which are meant 
                                            to inform broader understandings of economic evaluation for federal 
                                            statisticians. The more nuanced main and sub types, on the other hand, 
                                            derive from categorizations provided by another prominent open source 
                                            code hosting platform named SourceForge (https://sourceforge.net/).
                                            While GitHub repos do, at times, have topics listed, the site 
                                            does not organize projects by topics. SourceForge’s more 
                                            sophisticated and wide-ranging classification schema allowed us 
                                            to focus on smaller subcategories that could be aggregated into 
                                            the broader summary types later on. Below, we used R’s collapsibleTree
                                            package to visualize how these three levels of categorization fit together.")
                                   ),
                                   column(6, 
                                          h4(strong("Collapsible tree")),
                                          plotOutput("collapsible_tree")
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
                                     h3("Supervised Text Mining", align = "center"),
                                     br(""),
                                     p("To classify the repositories scraped from GitHub, a nested dictonary approach was adopted following the classificaiton of software types. To do so, a subset of the most popular programming languages on GitHub (Python, C, PHP, Java, Javascript) and applications (Blockchain, AI, Databases) were selected for further research. 
                                       Each was assigned a series of keywords, ranging from the name of the programming language or topic to popular topics tagged on scraped repositories, as well as popular packages for programming languages, interfaces or applications. From this, term matching was used to 'flag' repository descriptions that contained these keywords, and thus, potentially belonged to the corresponding category. 
                                       The figure below shows the results of this initial classification based on keyword."),
                                     img(src='classificationBreakdown.png')),
                            tabPanel("Sentence Embeddings Estimation",  
                                     h3(strong(""), align = "center")
                            ),
                            tabPanel("Node2Vec Embeddings",  
                                     h3(strong("Node2Vec Embeddings"), align = "center"), 
                                     br(),
                                     p("Based on a network of repositories (nodes) and collaberators (edges), the Node2Vec algorithm was used to generate embeddings.
                                       As opposed to the sentence embedding approach based on the degree of similarity in content, node embedding focuses on similarity in collaberators. To visualize the results, t-distributed stochastic neighbor embedding (tSNE) is used to reduce the dimensions. As seen below
                                       [image], repositories with the same language are clustered together [generate image again + add centrality/page rank info?]")
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
                                            a(href = '', 'Cierra Oliveira', target = '_blank'), "(Clemson University, Computing and Applied Sciences);",
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
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   h4(strong("Project Stakeholders")),
                                   p(a(href = '', 'Carol Robbins' , target = '_blank'), "(NCSES);"),
                                   p(a(href = '', 'Ledia Guci', target = '_blank'), "(NCSES);"),
                                   p("", style = "padding-top:10px;"),
                                   h4(strong("Acknowledgments")),
                                   p("We would like to thank xxxxxxx for their input to this project.")
                          )
                          ),
                 inverse = T)



# server -----------------------------------------------------------
server <- function(input, output, session) {
  # Run JavaScript Code
  #runjs(jscode)
  
  # collapsible tree -----------------------------------------------------
  output$collapsible_tree <- renderPlot({
      collapsibleTreeSummary(df_no_na,
      hierarchy = c("summary_type", "main_type", "sub_type"),
      width=800, height = 1000,  
      root = "Software Types", 
      fontSize = 20,
      zoomable = FALSE,
      attribute = "sourceforge_count",
      fillFun = colorspace::heat_hcl)
  })
  
  var <- reactive({
    input$sociodrop
  })
  
}

shinyApp(ui = ui, server = server)