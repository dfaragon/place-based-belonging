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
library(markdown)

path <- "/Users/aguha/Desktop/r_projects/oar/place-based-belonging/app"
setwd(path)

col1 <- c(rep("Undergraduate", 17), rep("International", 2), rep("Graduate", 2))
col2 <- c(rep("2022",5), rep("2020",5), rep("2019",5), "2018", "Overall",
          "Undergrad and Grad 2022", "Undergrad 2020",
          "2022", "Overall")
col3 <- c(rep(c("All Years", "4th Year", "3rd Year", "2nd Year", "1st Year"),3), #2022-2019
          "All Years", #2018
          NA, #Overall UG
          rep(NA, 4)
          )

dynamic_filter <- data.frame(col1, col2, col3)

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

    dashboardHeader(title="University of Oregon Place Based Belonging", titleWidth = 200),

    dashboardSidebar(width = 300,
                     sidebarMenu(
                       HTML(paste0(
                         "<br>",
                         "<a href='https://studentlife.uoregon.edu/research' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='uo_stacked_gray.svg' width = '186'></a>",
                         "<br>"
                       )),
                       menuItem("Who is SWaSI?", tabName = "about", icon = icon("users")),
                       menuItem("Summary", tabName = "summary", icon = icon("thumbtack")),
                       menuItem("Where? Campus Belonging", tabName = "campus", icon = icon("table")),
                       menuItem("Where? EMU Belonging", tabName = "emu", icon = icon("random", lib = "glyphicon")),
                       menuItem("Where? Inclusiveness", tabName = "inclusiveness", icon = icon("stats", lib = "glyphicon")),
                       menuItem("Why There? Wordnets & Wordclouds", tabName = "words", icon = icon("dashboard")),
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

    dashboardBody( #startdashboardBody

      tabItems( #start all tabItems

        tabItem(tabName = "about",

                # about section
                # uiOutput("aboutContent")
                includeMarkdown("www/pbb-about.md")
                # I am losing my mind with this section

        ),

        tabItem(tabName = "summary",

                # summary section
                includeMarkdown("www/summary.md")

        ),

        tabItem(tabName = "campus",

                fluidRow(
                  column(width = 6,
                         box(width = NULL, uiOutput("dynamicFilter")),
                         box(width = NULL, background = "black",
                             "Some text here.")),
                  column(width = 6,
                         box(width = NULL, title = "Belong", solidHeader = TRUE),
                         box(width = NULL, title = "Don't Belong", solidHeader = TRUE)))

        ),

        tabItem(tabName = "emu",

                fluidRow(
                  column(4, uiOutput("typeSelect")),
                  column(4, uiOutput("yearSelect")),
                  column(4, uiOutput("cohortSelect"))),
                fluidRow(
                  reactableOutput("table") %>% withSpinner(color = "green"))

                # fluidRow(
                #   column(width = 6,
                #          box(width = NULL, uiOutput("dynamicFilter")),
                #          box(width = NULL, background = "black",
                #              "Some text here.")),
                #   column(width = 6,
                #          box(width = NULL, title = "Belong", solidHeader = TRUE),
                #          box(width = NULL, title = "Don't Belong", solidHeader = TRUE)))
        ),

        tabItem(tabName = "inclusiveness",

                fluidRow(
                  column(width = 6,
                         box(width = NULL, title = "Campus Inclusiveness", solidHeader = TRUE),
                         box(width = NULL, title = "EMU Inclusiveness", solidHeader = TRUE))),
                  column(width = 6,
                       box(width = NULL, uiOutput("dynamicFilter")),
                       box(width = NULL, background = "black",
                           "Some text here."))

        ),

        tabItem(tabName = "words",

                fluidRow(uiOutput("dynamicFilter")),
                fluidRow(
                  column(width = 6,
                         box(width = NULL, title = "Campus Inclusiveness", solidHeader = FALSE),
                         box(width = NULL, title = "EMU Inclusiveness", solidHeader = FALSE))),
                  column(width = 6,
                       box(width = NULL, title = "Campus Inclusiveness", solidHeader = FALSE),
                       box(width = NULL, title = "EMU Inclusiveness", solidHeader = FALSE))

        ),

        tabItem(tabName = "emotions",

                fluidRow(
                column(width = 6,
                         box(width = NULL, title = "Plutchik's Wheel of Emotions", solidHeader = TRUE),
                         box(width = NULL, background = "black", "text about emo."))),
                column(width = 6,
                       box(width = NULL, uiOutput("dynamicFilter")),
                       box(width = NULL, background = "black",
                           "Bar graphs here."))

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
