library(shiny)
library(tidyverse)
library(rlang)


# Load dataset objects
load("objs")

# Create a test set
set.seed(1)
test <- movies %>% 
  slice(sample(194521, 100)) %>% 
  gather(key = "genre", value = "bool", -c(1:8)) %>% 
  filter(bool == 1) %>% 
  arrange(tconst) %>% 
  select(-bool) %>% 
  mutate(genre = as.factor(genre),
         isAdult = as.logical(isAdult), 
         releaseBin = cut(release, 
                          breaks = seq(1890, 2020, 10),
                          labels = seq(1890, 2010, 10)),
         runtimeBin = cut(runtimeMinutes,
                          breaks = seq(30, 210, 10),
                          labels = seq(40, 210, 10)),
         avgRatingBin = cut(averageRating,
                            breaks = seq(1, 10, 1),
                            labels = seq(2, 10, 1)))

# Check test column types
lapply(test, class) %>% unlist()


continuous_var <- colnames(test)[5:8]
categorical_var <- colnames(test)[9:12]
genre_var <- unique(test$genre)


# UI ----------------------------------------------------------------------
ui <- navbarPage(title = "Movie Browser", 
                 
                 tabPanel(title = "Plots",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                
                                textInput(inputId = "plot_title_top", 
                                          label = "Top Plot Title", 
                                          placeholder = "Enter text for plot title."),
                                
                                textInput(inputId = "plot_title_bottom", 
                                          label = "Bottom Plot Title", 
                                          placeholder = "Enter text for plot title."),
                                
                                selectInput(inputId = "x1", 
                                            label = "X axis: ", 
                                            choices = continuous_var,
                                            selected = "averageRating"),
                                
                                selectInput(inputId = "y1", 
                                            label = "Y axis: ", 
                                            choices = continuous_var,
                                            selected = "numVotes"),
                                
                                selectInput(inputId = "facet1", 
                                            label = "Facet: ", 
                                            choices = categorical_var),
                                
                                selectInput(inputId = "color1", 
                                            label = "Color: ", 
                                            choices = categorical_var,
                                            selected = "genre"), 
                                
                                sliderInput(inputId = "size1",
                                            label = "Size: ",
                                            min = 0,
                                            max = 5, 
                                            value = 3,
                                            step = 0.1, 
                                            animate = TRUE),
                                
                                sliderInput(inputId = "alpha1",
                                            label = "Alpha: ",
                                            min = 0,
                                            max = 1, 
                                            value = 0.3, 
                                            animate = TRUE)
                                
                              ),
                              
                              mainPanel(
                                
                                plotOutput(outputId = "facetted_plot"),
                                
                                br(), 
                                
                                plotOutput(outputId = "plot1")
                              )
                            )
                          )
                 ), 
                 
                 tabPanel(title = "Plotty Plot",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(),
                              mainPanel()
                            )
                          )
                 )
                 )



# Server ------------------------------------------------------------------
server <- function(input, output) {
  
  output$facetted_plot <- renderPlot({
    test %>% 
      ggplot(aes_string(x = input$x1, y = input$y1)) + 
      geom_point(size = input$size1, 
                 alpha = input$alpha1) +
      facet_wrap(reformulate(input$facet1)) +
      # think about the below - hasn't worked yet 
      # facet_wrap(eval(expr(~ !!ensym(input$facet1)))) 
      # https://stackoverflow.com/questions/21588096/pass-string-to-facet-grid-ggplot2
      labs(title = input$plot_title_top)
  })
  
  
  output$plot1 <- renderPlot({
    test %>% 
      ggplot(aes_string(x = input$x1, y = input$y1, color = input$color1)) + 
      geom_point(size = input$size1, 
                 alpha = input$alpha1, 
                 position = "jitter") +
      labs(title = input$plot_title_bottom)
  })
  
}


# Run the application ----------------------------------------------------
shinyApp(ui = ui, server = server)