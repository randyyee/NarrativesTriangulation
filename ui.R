library(shiny)
library(shinythemes)
library(shiny)
library(readxl)
library(ICPIHelpers)
library(tidyverse)
library(rpivotTable)
library(tidytext)
library(DT)
library(reshape2)
library(wordcloud)

shinyUI(navbarPage(theme = shinytheme("simplex"),
                   
                   "Narratives & Visualizations",
                   
                   tabPanel(
                     "Home",
                     sidebarLayout(
                       sidebarPanel(fileInput("import", "Choose Narratives File (xlsx)",
                                              multiple = FALSE),
                                    fileInput("import1", "Choose MSD File (txt)",
                                              multiple = FALSE),
                                    selectInput("ou_list", "Choose Operating Unit",
                                                choices = NULL),
                                    selectInput("indicator_list", "Choose Indicator",
                                                choices = NULL),
                                    actionButton("display","Analyze!")),
                       mainPanel(
                         tags$h1("Narratives Triangulation Tool"),
                         tags$h3("Introduction"),
                         p("This tool triangulates Monitoring, Evaluation, and Reporting (MER) narratives with indicator results.
                                  The Narratives Traingulation Tool takes in MER Narratives and MER Structured Datasets (MSD) providing cross-filtering between the two data sources.
                                  Once the required data sources have been imported, the pivot table visualizer can be used to investigate the content of the selected narrative using the MER indicators.
                                  This tool also has advanced features for textual analysis of the narratives including sentiment analysis, tf-idf, n-grams and correlations."),
                         hr(),
                         tags$h3("Instructions"),
                         p("1. Upload your narratives file (.xlsx)"),
                         p("2. Upload your MSD file (.txt)"),
                         p(tags$i("Note: Any type of MSD can be used, but results will be limited to the level of the MSD")),
                         p("3. Click Analyze!"),
                         p("4. Choose a narrative in Narratives Explorer"),
                         p("5. Use Narratives Trends to see any reported narratives from previous quarters"),
                         p("6. Explore different text mining analyses under Narrative Analysis"),
                         p("6. Visualize the narrative with MER indicators using under Result"),
                         hr(),
                         tags$h3("Questions?"),
                         p("Randy Yee (pcx5@cdc.gov)"),
                         p("CDC/GDIT | CGH | DGHT | HIDMSB | DUAT-ICPI")
                       )
                     )
                     
                   ),
                   navbarMenu("Narratives",
                              tabPanel("Narratives Explorer",
                                       DT::dataTableOutput('narrativesdt')),
                              tabPanel("Narrative Trends"),
                              tabPanel("Resources",
                                       DT::dataTableOutput('bingdt'))
                   ),
                   navbarMenu("Narrative Analysis",
                              tabPanel("Sentiment Analysis",
                                       tags$h2("Introduction"),
                                       p("Sentiment analysis is performed using a modified lexicon of Bing Liu and collaborators. 
                                         Unigrams with sentiment designations are joined to the narrative datasets and scored according to pre-defined sentiments of positive or negative.
                                         Modification have been made to the Bing lexicon so that the sentiments are in agreement with the language of PEPFAR (i.e. suppression which is commonly understood to be positive, as in viral suppression)."),
                                       tags$h2("Global Sentiments"),
                                       p("This section shows the general sentiment of the PEPFAR program across all operating units by indicator bundles."),
                                       plotOutput("sentiment_ous", height = "800px"),
                                       tags$h2("Sentiments of the Operating Unit"),
                                       p("If the following sections are empty, please go to the Narratives page and select a narrative in the table."),
                                       plotOutput("sentiment_ou", height = "400px"),
                                       tags$h2("Contribution of Each Word to Sentiments"),
                                       p("The following plots show the relative contribution of each word to the overall indicator bundle."),
                                       plotOutput("sentiment_ou_contribution", height = "800px"),
                                       plotOutput("sentiment_ou_contribution_ind")
                              ),
                              tabPanel("Wordclouds",
                                       tags$h2("Introduction"),
                                       p("Word clouds showing top positive and negative words. 
                                         Size of word indicates frequency.
                                         If the following sections are empty, please go to the Narratives page and select a narrative in the table."),
                                       tags$h2("Operating Unit Comparison Cloud: Positive vs. Negative Sentiment"),
                                       p("Comparison word cloud showing the top positive and negative sentiments of the selected operating unit."),
                                       plotOutput("compare_cloud_ou"),
                                       tags$h2("Indicator Comparison Cloud: Positive vs. Negative Sentiment"),
                                       p("Comparison word cloud showing the top positive and negative sentiments of the selected operating unit and indicator."),
                                       plotOutput("compare_cloud_ouind")
                                       ),
                              tabPanel("tf-idf"),
                              tabPanel("N-grams and Correlations"),
                              tabPanel("Latent Dirichlet Allocation")
                   ),
                   tabPanel("Results",
                            tags$h2("Summary"),
                            textOutput("title"),
                            p(),
                            textOutput("content"),
                            hr(),
                            tags$h2("Pivot Table"),
                            rpivotTableOutput("msd_df"))
)
)
