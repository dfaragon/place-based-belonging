##################################
# Place Based Belonging App.     #
# by Anwesha Guha.               #
# ui.R file                      #
##################################

library(leaflet)
library(shinydashboard)
library(collapsibleTree)
library(shinycssloaders)
library(DT)
library(tigris)
library(reactable)

path <- "/Users/aguha/Desktop/r_projects/oar/pbb_app"
setwd(path)

###########
# LOAD UI #
###########

shinyUI(fluidPage(
    
    # load custom stylesheet
    includeCSS("www/style.css"),
    
    # load google analytics script -- this is so you can track who is viewing the dashboard. cool! but will hold off
   #  tags$head(includeScript("www/google-analytics-bioNPS.js")),
    
    # remove shiny "red" warning messages on GUI
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    
    # load page layout
    dashboardPage(
        
        skin = "blue",
        
        dashboardHeader(title="University of Oregon Place Based Belonging", titleWidth = 300),
        
        dashboardSidebar(width = 300,
                         sidebarMenu(
                             HTML(paste0(
                                 "<br>",
                                 "<a href='https://studentlife.uoregon.edu/research' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='uo_stacked_gray.svg' width = '186'></a>",
                                 "<br>"
                             )),
                             menuItem("Who is SWaSI?", tabName = "about", icon = icon("users")),
                             menuItem("Summary", tabName = "summary", icon = icon("thumbtack")),
                             menuItem("Where?", tabName = "table", icon = icon("table")),
                             menuItem("Campus Belonging", tabName = "campus", icon = icon("random", lib = "glyphicon")),
                             menuItem("Erb Memorial Union Belonging", tabName = "emu", icon = icon("stats", lib = "glyphicon")),
                             menuItem("Why There? Wordnets & Wordclouds", tabName = "why", icon = icon("dashboard")),
                             menuItem("Why There? Emotions", tabName = "emotions", icon = icon("dashboard")),
                             menuItem("Where for Whom?", tabName = "whom", icon = icon("question")),
                             menuItem("Between Here and Where?", tabName = "between", icon = icon("question")),
                             menuItem("Supplemental Method", tabName = "method", icon = icon("question")),
                             HTML(paste0("<br>",
                                         "<br>",
                                     "<script>",
                                     "var today = new Date();",
                                     "var yyyy = today.getFullYear();",
                                     "</script>",
                                     "<p style = 'text-align: center;'><small>&copy; - <a href='https://github.com/UOSLAR' target='_blank'>https://github.com/UOSLAR</a> - <script>document.write(yyyy);</script></small></p>")
                         ))
                         
        ), # end dashboardSidebar
        
        dashboardBody(
            
            tabItems(
                
                tabItem(tabName = "about",
                        
                        # about section
                        includeMarkdown("www/pbb-about.md")
                        
                ),
                
                tabItem(tabName = "summary",
                        
                        # summary section
                        includeMarkdown("www/summary.md")
                        
                ),
                
                tabItem(tabName = "table", 
                    
                        # summary section
                        # includeMarkdown("www/summary.md"),    
                        fluidRow(
                            column(3, uiOutput("dynamicFilter")),
                            column(3, reactableOutput("table")) %>% withSpinner(color = "navy"))
                    # reactable table should just go in here directly 
                    # dataTableOutput("speciesDataTable") %>% withSpinner(color = "lightseagreen")
                    
                ),
                
                tabItem(tabName = "campus", 
                        
                        # summary section
                        includeMarkdown("www/summary.md")
                        
                        # campus needs ... 
                        # includeMarkdown("www/tree.md"),
                        # column(3, uiOutput("parkSelectComboTree")),
                        # column(3, uiOutput("categorySelectComboTree")),
                        # collapsibleTreeOutput('tree', height='700px') %>% withSpinner(color = "lightseagreen")
                        
                ),
                
                tabItem(tabName = "emu",
                        
                        # summary section
                        includeMarkdown("www/summary.md")
                        
                        # ggplot2 species charts section
                        # includeMarkdown("www/charts.md"),
                        # fluidRow(column(3, uiOutput("categorySelectComboChart"))),
                        # column(6, plotOutput("ggplot2Group1") %>% withSpinner(color = "lightseagreen")),
                        # column(6, plotOutput("ggplot2Group2") %>% withSpinner(color = "lightseagreen"))
                        
                ), 
                
                tabItem(tabName = "why",
                        
                        # summary section
                        includeMarkdown("www/summary.md")
                        
                        # # choropleth species map section
                        # includeMarkdown("www/choropleth.md"),
                        # fluidRow(
                        #     column(3, uiOutput("statesSelectCombo")),
                        #     column(3, uiOutput("categorySelectComboChoro"))
                        # ),
                        # fluidRow(
                        #     column(3,tableOutput('stateCategoryList') %>% withSpinner(color = "lightseagreen")),
                        #     column(9,leafletOutput("choroplethCategoriesPerState") %>% withSpinner(color = "lightseagreen"))
                        # )
                        
                ),
                
                tabItem(tabName = "emotions",
                        
                        # summary section
                        includeMarkdown("www/summary.md")
                        
                        # # choropleth species map section
                        # includeMarkdown("www/choropleth.md"),
                        # fluidRow(
                        #     column(3, uiOutput("statesSelectCombo")),
                        #     column(3, uiOutput("categorySelectComboChoro"))
                        # ),
                        # fluidRow(
                        #     column(3,tableOutput('stateCategoryList') %>% withSpinner(color = "lightseagreen")),
                        #     column(9,leafletOutput("choroplethCategoriesPerState") %>% withSpinner(color = "lightseagreen"))
                        # )
                        
                ),
                
                tabItem(tabName = "whom", 
                        
                        includeMarkdown("www/whom.md")
                        
                ),
                tabItem(tabName = "between", 
                        
                        includeMarkdown("www/between.md")
                ),
                
                tabItem(tabName = "method", 
                        
                        includeMarkdown("www/method.md")
                )
                
            ) #end tabItems
            
        ) # end dashboardBody
        
    )# end dashboardPage
    
))