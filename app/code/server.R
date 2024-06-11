##################################
# Place Based Belonging App.     #
# by Anwesha Guha.               #
# server.R file                  #
##################################

library(shiny)
library(tidyverse)
library(rvest)
library(leaflet.extras)
library(reactable)

path <- "/Users/aguha/Desktop/r_projects/oar/place-based-belonging/app"
setwd(path)

pbb_tables_for_rt <- readRDS("~/Desktop/r_projects/oar/place-based-belonging/app/data/separated/pbb_tables_for_rt.rds")

df_names <- names(pbb_tables_for_rt)

for (name in df_names) {
  assign(name, pbb_tables_for_rt[[name]])
}

#####################
# SUPPORT FUNCTIONS #
#####################

source("code/helpers.R")


################
# SERVER LOGIC #
################

shinyServer(function(input, output, session) {

  # filters
  output$typeSelect <- renderUI({
    selectInput("selectedType", "Select:", c("Undergraduate", "International", "Graduate"))
  })

  output$yearSelect <- renderUI({
    selectInput("selectedYear", "Select Year:", sort(as.character(unique(dynamic_filter[dynamic_filter$col1==input$selectedType, c("col2")]))))
  })

  output$cohortSelect <- renderUI({
    selectInput("selectedCohort", "Select Cohort (if applicable):", sort(as.character(unique(dynamic_filter[dynamic_filter$col1==input$selectedType & dynamic_filter$col2==input$selectedYear, c("col3")]))))
  })


  output$table <- renderReactable({

    #Undergrad
    if(input$selectedType == "Undergraduate") {
      year <- input$selectedYear

      if(input$selectedYear == "Overall") {
        reactable_fun(us_ug)
      } else if (input$selectedYear == "2022") {
        if(input$selectedCohort == "All Years") {reactable_fun(us_us_ay2122)
        } elseif(input$selectedCohort == "4th Year") { reactable_fun(us_ug_ay2122_c2122)
        } elseif(input$selectedCohort == "3th Year") { reactable_fun(us_ug_ay2122_c2021)
        } elseif(input$selectedCohort == "2nd Year") { reactable_fun(us_ug_ay2122_c1920)
        } elseif(input$selectedCohort == "1st Year") { reactable_fun(us_ug_ay2122_c1819)}
      } else if (input$selectedYear == "2020") {
        if(input$selectedCohort == "All Years") {reactable_fun(us_us_ay1920)
        } elseif(input$selectedCohort == "4th Year") { reactable_fun(us_ug_ay1920_c1920)
        } elseif(input$selectedCohort == "3th Year") { reactable_fun(us_ug_ay1920_c1819)
        } elseif(input$selectedCohort == "2nd Year") { reactable_fun(us_ug_ay1920_c1718)
        } elseif(input$selectedCohort == "1st Year") { reactable_fun(us_ug_ay1920_c1617)}
      } else if (input$selectedYear == "2019") {
        if(input$selectedCohort == "All Years") {reactable_fun(us_us_ay1819)
        } elseif(input$selectedCohort == "4th Year") { reactable_fun(us_ug_ay1819_c1819)
        } elseif(input$selectedCohort == "3th Year") { reactable_fun(us_ug_ay1819_c1718)
        } elseif(input$selectedCohort == "2nd Year") { reactable_fun(us_ug_ay1819_c1617)
        } elseif(input$selectedCohort == "1st Year") { reactable_fun(us_ug_ay1819_c1516)}
      }
    }
    # International
    else if(input$selectedType == "International") {
      if (input$selectedYear == "Overall") {
        reactable_fun(i)
      } else if (input$selectedYear == "Undergrad and Grad 2022") {
        reactable_fun(i_ay2122)
      } else if (input$selectedYear == "Undergrad 2020") {
        reactable_fun(i_ug_ay1920)
      }
    }
    # Graduate
    else if(input$selectedType == "Graduate" && input$selectedYear == "2022") {
      reactable_fun(gr_ay2122)
    }
    else if(input$selectedType == "Graduate" && input$selectedYear == "Overall") {
      box("Idk")
    }
 else {
      HTML("<p>No data available for the selected options.</p>")
    }
  })

})
