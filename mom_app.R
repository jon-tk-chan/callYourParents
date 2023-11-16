library(tidyverse)
library(ggplot2)
library(shinythemes)
# library(geojsonio)
library(sp)
# library(broom)
library(rnaturalearth)

init_country <- "Canada"
# countries_list <- c("Canada", "US", "Hong Kong", "Thailand")

##
## LOAD DATA
world <- ne_countries(scale = "small", returnclass = "sf")

data_df <- read.csv("data/lex.csv")
colnames(data_df)<-gsub("X","",colnames(data_df)) #clean column names - has an X

countries_list <- unique(world$name_long)
# User interface ----
ui <- fluidPage(theme=shinytheme("journal"),
                navbarPage("Call Your Parents",
                           tabPanel("Home",
                                    sidebarPanel(
                                      HTML("<h3>Input parameters </h3>"),
                                      numericInput("age", "Enter your parent's age (Years)", value = 50),
                                      numericInput("year", "Enter the year of life expectancy data used", value = 2021),
                                      selectInput("country", "Select Country", countries_list, selected = init_country),
                                      actionButton("submitbutton","Submit",class="btn btn-primary")
                                    ), #sidebarPanel
                                    mainPanel(
                                      h3("Output"),
                                      p("Output visualizations - choropleth determining displaying how many years/weeks/months you have left to call your parents. "),
                                      # br(),
                                      # textOutput("final_text"),
                                      br(),
                                      plotOutput("map_plot"),

                                      # h6("Map"),
                                      # plotOutput("plot"),
                                      # h6("Data Table"),
                                      # tableOutput('table')
                                    )
                           ), #tabPanel - Map
                           tabPanel("About"
                           ) #tabPanel - About
                ) #navbarPage
)

# Server logic
server <- function(input, output, session) {

  observeEvent(input$submitbutton, {



    selected_col <- input$year
    selected_age <- input$age
    final_colname <- "life_exp"

    #select data column,, rename, fill values with 0
    life_data <- data_df[ ,c("country", selected_col)] #colname is number
    names(life_data)[2] <- final_colname
    life_data<- as.data.frame(life_data)
    final_df <- merge(world, life_data, by.x = "name_long", by.y = "country", all.x = TRUE)
    final_df <- final_df %>% replace_na(list(life_exp = 0))
    final_df <- final_df %>%
      mutate(years_left_raw = life_exp - {{selected_age}},
             years_left = case_when( years_left_raw < 0 ~ 0,
                                     TRUE ~ years_left_raw),
             weeks_left = years_left*51.25)

    #CHECK VARIABLES
    validate(
      need(is.numeric(input$age) && floor(input$age) == input$age, "Age must be an integer."),
      need(is.numeric(input$year) && floor(input$year) == input$year, "Year must be an integer."),
      need(input$country %in% final_df$name_long, "Selected country not found in the dataset.")
    )
    #make output map
    map_plot <- final_df %>%
      ggplot() +
      geom_sf(aes(fill = years_left)) +
      theme_void() +
      theme(legend.position = "top") +
      labs(fill = "Life expectancy minus current age")

    output$map_plot <- renderPlot({map_plot})



  })

}

# Run the app
shinyApp(ui, server)
