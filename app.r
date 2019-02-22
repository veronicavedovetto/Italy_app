
library(tidyverse)
library(shiny)
library(rgdal)
library(raster)
library(sf)
library(tmap)


load("Italy_maps_with_features.RData")




ui <- fluidPage(
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      # Select variable for y-axis
      selectInput(inputId = "x", 
                  label = "Socio-demographic features:",
                  choices = c("Region name" = "Region",
                              "Total population" = "Pop_tot_31dec",
                              "Male population (abs)" = "Pop_male_31dec",
                              "Female population (abs)" = "Pop_female_31dec",
                              "Male population (perc)" = "Pop_male_31dec_perc",
                              "Female population (perc)" = "Pop_female_31dec_perc",
                              "Number of families" = "Number_of_families",
                              "Number of cohabitations" = "Number_of_cohabitations",
                              "Avg number of family components" = "Avg_num_components",
                              "Male life expextancy" = "male_life_expectancy_2017",
                              "Female life expectancy" = "female_life_expectancy_2017",
                              "Life expectancy" = "tot_life_expectancy_2017",
                              "Birth rate" = "quoz_nat_2017",
                              "Death rate" = "quoz_death_2017",
                              "Marriage rate" = "quoz_marriages_2017",
                              "Fertility rate" = "fertility_rate_2017",
                              "Age avg at childbirth" = "age_avg_childbirth_2017"), 
                  selected = "Region"),
      
      conditionalPanel(
        condition = "input.x != 'Region'",
      # Select variable for colour
      selectInput(inputId = "colour", 
                  label = "COLOUR:",
                  choices = c("Blues", "Greens","Greys","Oranges","Purples","Reds","Accent"), 
                  selected = "Accent")
      ),
      checkboxInput(inputId = "show_data",
                    label = "Show data table",
                    value = FALSE),
      
      conditionalPanel(
        condition = "input.show_data == true",
      
      # Select variables for datatable
      checkboxGroupInput(inputId = "selected_var",
                         label = "Please select variables to be shown in the table:",
                         choices = c("Total population" = "Pop_tot_31dec",
                                     "Male population (abs)" = "Pop_male_31dec",
                                     "Female population (abs)" = "Pop_female_31dec",
                                     "Male population (perc)" = "Pop_male_31dec_perc",
                                     "Female population (perc)" = "Pop_female_31dec_perc",
                                     "Number of families" = "Number_of_families",
                                     "Number of cohabitations" = "Number_of_cohabitations",
                                     "Avg number of family components" = "Avg_num_components",
                                     "Male life expextancy" = "male_life_expectancy_2017",
                                     "Female life expectancy" = "female_life_expectancy_2017",
                                     "Life expectancy" = "tot_life_expectancy_2017",
                                     "Birth rate" = "quoz_nat_2017",
                                     "Death rate" = "quoz_death_2017",
                                     "Marriage rate" = "quoz_marriages_2017",
                                     "Fertility rate" = "fertility_rate_2017",
                                     "Age avg at childbirth" = "age_avg_childbirth_2017"),
                         selected = c("Pop_tot_31dec"))
        ,
      actionButton(inputId = "button",
                   label = "Show")
      )
      
                   
      ),
    # Outputs
    mainPanel(
      plotOutput(outputId = "ItalyMaps"),
      DT::dataTableOutput(outputId = "ItalyTable")
    )
  )

  
  )

server <- function(input, output,session) {

  output$ItalyMaps <- renderPlot({
      tm_shape(italy_maps) +
      tm_borders() +
      tm_fill(col = input$x, palette = input$colour) +
      tm_legend(legend.outside = TRUE) 
      })
      
    
   
  df <- eventReactive(input$button, {
    req(input$selected_var)
    italy_maps_red <- italy_maps@data %>%
      dplyr::select(Region,one_of(input$selected_var))

  })

  output$ItalyTable <- DT::renderDataTable({
    if (input$show_data) {
      if (input$button) {
      DT::datatable(data = df(),
                  options = list(pageLength = 10),
                  rownames = FALSE)
        
      } 
      
    }
  })
  
  observeEvent(input$show_data == FALSE, {
    updateCheckboxGroupInput(session, "selected_var",label = "Please select variables to be shown in the table:",
                             choices = c("Total population" = "Pop_tot_31dec",
                                         "Male population (abs)" = "Pop_male_31dec",
                                         "Female population (abs)" = "Pop_female_31dec",
                                         "Male population (perc)" = "Pop_male_31dec_perc",
                                         "Female population (perc)" = "Pop_female_31dec_perc",
                                         "Number of families" = "Number_of_families",
                                         "Number of cohabitations" = "Number_of_cohabitations",
                                         "Avg number of family components" = "Avg_num_components",
                                         "Male life expextancy" = "male_life_expectancy_2017",
                                         "Female life expectancy" = "female_life_expectancy_2017",
                                         "Life expectancy" = "tot_life_expectancy_2017",
                                         "Birth rate" = "quoz_nat_2017",
                                         "Death rate" = "quoz_death_2017",
                                         "Marriage rate" = "quoz_marriages_2017",
                                         "Fertility rate" = "fertility_rate_2017",
                                         "Age avg at childbirth" = "age_avg_childbirth_2017"),
                             selected = c("Pop_tot_31dec"))
  })
  
  observeEvent(input$x == "Region", {
    updateSelectInput(session, "colour",label = "COLOUR:",
                             choices = c("Blues", "Greens","Greys","Oranges","Purples","Reds","Accent"), 
                             selected = "Accent")
   
  })
}

# Create the Shiny app object
shinyApp(ui = ui, server = server)


