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

path <- "/Users/aguha/Desktop/r_projects/oar/pbb_app"
setwd(path)
load("pbb2.RData")

#####################
# SUPPORT FUNCTIONS #
#####################


################
# SERVER LOGIC #
################

shinyServer(function(input, output, session) {

    # Dynamic UI for additional filters
    output$dynamicFilter <- renderUI({
        if(input$typeSelect == "Undergraduate") {
            selectInput("yearSelect", "Select Year:",
                        choices = c("2018", "2019", "2020", "2022", "Overall"))
        } else if(input$typeSelect == "International") {
            selectInput("intSelect", "Select Category:",
                        choices = c("Overall", "Undergrad and Grad 2022", "Undergrad 2020"))
        } else {
            return()
        }
    })


    # Render the correct table based on the input selection
    output$table <- renderReactable({
        if(input$typeSelect == "Undergraduate" && input$yearSelect == "Overall") {
            rt_cam_us_ug
        } else if(input$typeSelect == "Undergraduate" && input$yearSelect == "2022") {
            rt_cam_us_ug_ay2122
        } else if(input$typeSelect == "Undergraduate" && input$yearSelect == "2020") {
            rt_cam_us_ug_ay1920
        } else if(input$typeSelect == "Undergraduate" && input$yearSelect == "2019") {
            rt_cam_us_ug_ay1819
        } else if(input$typeSelect == "Undergraduate" && input$yearSelect == "2018") {
            rt_cam_us_ug_ay1718
        } else if(input$typeSelect == "International" && input$intSelect == "Overall") {
            rt_cam_i
        } else if(input$typeSelect == "International" && input$intSelect == "Undergrad and Grad 2022") {
            rt_cam_i_ay2122
        } else if(input$typeSelect == "International" && input$intSelect == "Undergrad 2020") {
            rt_cam_i_ug_ay1920
        } else if(input$typeSelect == "Graduate") {
            rt_cam_gr_ay2122
        }
    })

})
