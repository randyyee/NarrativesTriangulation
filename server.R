library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(readxl)
library(googlesheets4)
library(tidyverse)
library(rpivotTable)
library(tidytext)
library(DT)
library(reshape2)
library(wordcloud2)
library(ggplot2)
library(igraph)
library(ggraph)
library(leaflet)
library(crayon)
if (!require("shinysky")) devtools::install_github("AnalytixWare/ShinySky")
library(shinysky)

# Diagnostics library(profvis) library(reactlog)

options(shiny.maxRequestSize = 4000 * 1024^2)
# options(shiny.reactlog = TRUE)

##### Functions ####
source("./functions/data_management.R")
source("./functions/sentiments_management.R")
source("./functions/bigrams_management.R")
source("./functions/get_s3_choices.R")

##### Modules ####
source("./modules/mod_home.R")
source("./modules/mod_dashboard.R")
source("./modules/submod_ui_updater.R")
source("./modules/mod_mapboard.R")
source("./modules/mod_impacttable.R")
source("./modules/mod_resources.R")
source("./modules/mod_triangulation.R")

#### Test Data #### load('narratives_data.RData')

#### Text Resources ####

gs4_deauth()

bing <- read_sheet("https://docs.google.com/spreadsheets/d/1QAhceme-y-dJrsV4WrCk5lqzFPhLfnNfjcn9xuTHiGQ/edit?usp=sharing", 
    sheet = "HIV Sentiments (based on Bing et al.)")

stopwords <- read_sheet("https://docs.google.com/spreadsheets/d/1QAhceme-y-dJrsV4WrCk5lqzFPhLfnNfjcn9xuTHiGQ/edit?usp=sharing", 
    sheet = "Stopwords")

negationwords <- read_sheet("https://docs.google.com/spreadsheets/d/1QAhceme-y-dJrsV4WrCk5lqzFPhLfnNfjcn9xuTHiGQ/edit?usp=sharing", 
    sheet = "Negation")

covidwords <- read_sheet("https://docs.google.com/spreadsheets/d/1QAhceme-y-dJrsV4WrCk5lqzFPhLfnNfjcn9xuTHiGQ/edit?usp=sharing", 
    sheet = "Covidwords")

nar <- get_s3_choices(type = "narratives")

mer <- get_s3_choices(type = "mer")

#--------------------------------------------------------------------------------------------#

server <- function(input, output, session) {
    
    
             # Server Modules
            dashboard_server("d", narratives_df(), prepared_dfs())

            impact_server("i", narratives_df())

            resources_server("r", bing, stopwords, covidwords)

            triangulation_server("t", narratives_df(), msd_df())
    
            # Import Datasets
            msd_df <- reactive({
                req(input$m_path)
                msd_import(mer[mer$file_names %in% input$m_path, "path_names"])
            })

            narratives_df <- reactive({
                req(input$n_path)
                if (grepl("merged", input$n_path)) {
                  merged_file = T
                } else {
                  merged_file = NULL
                }
                nar_import(nar[nar$file_names %in% input$n_path, "path_names"], 
                  merged_file = merged_file)
            })

            prepared_dfs <- reactive({
                bigrams <- prepare_bigrams(narratives_df(), bing, stopwords, 
                  negationwords)
                sents <- prepare_sentiments(bigrams)
                # sents_c <- prepare_sent_contributes(bigrams)
                return(list(bigrams, sents))

            })
    
    user_input <- reactiveValues(authenticated = FALSE, status = "", d2_session = NULL)
    
    observeEvent(input$login_button, {

        tryCatch({
            datimutils::loginToDATIM(base_url = "https://www.datim.org/", 
                username = input$user_name, password = input$password, 
                d2_session_envir = parent.env(environment()))
        }, error = function(e) {
            shinyWidgets::sendSweetAlert(session, title = "Login failed", 
                text = "Please check your username/password!", type = "error")
        })

        if (exists("d2_default_session")) {
            
            if (user_input$authenticated == FALSE) {print("here")}

            user_input$authenticated <- T
        }
        
        return(user_input)
    })

    output$ui <- renderUI({
        if (user_input$authenticated == FALSE) {
            ##### UI code for login page
            fluidPage(fluidRow(column(width = 2, offset = 5, br(), br(), 
                br(), br(), uiOutput("uiLogin"))))
        } else {

            #### UI: Sidebar ####
            sidebar <- dashboardSidebar(sidebarMenu(menuItem("Home", tabName = "home", 
                icon = icon("home")), menuItem("Dashboard", tabName = "analyses", 
                icon = icon("line-chart")), menuItem("Ngrams Explorer", 
                tabName = "impacttable", icon = icon("compass")), menuItem("MER Triangulation", 
                tabName = "triangulate", icon = icon("puzzle-piece")), 
                menuItem("Resources", tabName = "resources", icon = icon("book")), 
                selectizeInput("n_path", shiny::HTML("<span style='color: white'>Choose Narratives File (xlsx)</span>"), 
                  choices = list(narratives = nar$file_names), options = list(placeholder = "Please select an option", 
                    onInitialize = I("function() { this.setValue(\"\"); }"))), 
                selectizeInput("m_path", shiny::HTML("<span style='color: white'>Choose MSD File (txt)</span>"), 
                  choices = list(mer = mer$file_names), options = list(placeholder = "Please select an option", 
                    onInitialize = I("function() { this.setValue(\"\"); }")))), 
                busyIndicator())


            #### UI: Body ####
            body <- dashboardBody(shinyDashboardThemes(theme = "onenote"), 
                tabItems(tabItem(tabName = "home", home_ui()), tabItem(tabName = "analyses", 
                  dashboard_ui("d")), tabItem(tabName = "impacttable", 
                  impact_ui("i")), tabItem(tabName = "triangulate", triangulation_ui("t")), 
                  tabItem(tabName = "resources", resources_ui("r"))))


            dashboardPage(title = "Narrator", dashboardHeader(title = "Narratives Application"), 
                sidebar, body)
        }
    })
    
    output$uiLogin <- renderUI({
        wellPanel(fluidRow(img(src = "pepfar.png", align = "center"), h4("Welcome to the MER-Narratives App. Please login with your DATIM credentials:")), 
            fluidRow(textInput("user_name", "Username: ", width = "600px"), 
                passwordInput("password", "Password:", width = "600px"), 
                actionButton("login_button", "Log in!")))
    })

    

}
#--------------------------------------------------------------------------------------------#

