# Packages ----------------------------------------------------------------
library(shiny)
library(tidyverse)
library(rlang)
library(DT)


# Objects -----------------------------------------------------------------
load("objs")


# Test set ----------------------------------------------------------------
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
         ratingBin = cut(averageRating,
                            breaks = seq(1, 10, 1),
                            labels = seq(2, 10, 1)))


# Check test column types
lapply(test, class) %>% unlist()


# Change averageRating column name for future convenience
colnames(test)[7] <- "rating"


continuous_var <- colnames(test)[5:8]
categorical_var <- colnames(test)[9:12]
genre_var <- unique(test$genre)
release_var <- unique(test$releaseBin)
runtime_var <- unique(test$runtimeBin)
rating_var <- unique(test$ratingBin)


# UI ----------------------------------------------------------------------
ui <- navbarPage(title = "Movie Browser", 

# Tab 1 -------------------------------------------------------------------
                 tabPanel(title = "Plots",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                
                                textInput(inputId = "plot_title_top1", 
                                          label = "Top Plot Title", 
                                          placeholder = "Enter text for plot title.",
                                          value = "Facetted on genre"),
                                
                                textInput(inputId = "plot_title_bottom1", 
                                          label = "Bottom Plot Title", 
                                          placeholder = "Enter text for plot title.",
                                          value = "Colored by genre"),
                                
                                selectInput(inputId = "x1", 
                                            label = "X axis: ", 
                                            choices = continuous_var,
                                            selected = "rating"),
                                
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
                                
                                plotOutput(outputId = "facetted_plot1"),
                                
                                br(), 
                                
                                # Explanatory text
                                HTML(paste0("The above plot facetted on genre.", br(), br(), 
                                            "The below plot combines facets and shows by colours.")),
                                
                                
                                plotOutput(outputId = "plot1", 
                                           brush = "plot1_brush"),
                                
                                plotOutput(outputId = "plotbrush_temp")
                              )
                            )
                          )
                 ), 
                 

# Tab 2 -------------------------------------------------------------------
                 tabPanel(title = "Plotty Plot",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                
                                textInput(inputId = "plot_title_top2", 
                                          label = "Top Plot Title", 
                                          placeholder = "Enter text for plot title.",
                                          value = "Facetted on genre"),
                                
                                textInput(inputId = "plot_title_bottom2", 
                                          label = "Bottom Plot Title", 
                                          placeholder = "Enter text for plot title.", 
                                          value = "Filtered to Drama, 1920s, 90 mins & 6.1-7 scores"),
                                
                                selectInput(inputId = "x2", 
                                            label = "X axis: ", 
                                            choices = continuous_var,
                                            selected = "rating"),
                                
                                selectInput(inputId = "y2", 
                                            label = "Y axis: ", 
                                            choices = continuous_var,
                                            selected = "numVotes"),
                                
                                selectInput(inputId = "facet2", 
                                            label = "Facet: ", 
                                            choices = categorical_var),
                                
                                # Select genre
                                selectInput(inputId = "genre2",
                                            label = "Select Genre:",
                                            choices = genre_var,
                                            selected = "Drama", 
                                            selectize = TRUE,
                                            multiple = TRUE), 
                                
                                # Select release bin
                                selectInput(inputId = "release2",
                                            label = "Select release interval: ",
                                            choices = release_var, 
                                            selected = 1920, 
                                            selectize = TRUE, 
                                            multiple = TRUE),
                                
                                # Select runtime bin 
                                selectInput(inputId = "runtime2", 
                                            label = "Select runtime interval: ",
                                            choices = runtime_var,
                                            selected = 90, 
                                            selectize = TRUE, 
                                            multiple = TRUE),
                                
                                # Select rating bin
                                selectInput(inputId = "rating2", 
                                            label = "Select rating interval: ",
                                            choices = rating_var,
                                            selectize = TRUE, 
                                            selected = 7, 
                                            multiple = TRUE), 
                                
                                sliderInput(inputId = "size2",
                                            label = "Size: ",
                                            min = 0,
                                            max = 5, 
                                            value = 3,
                                            step = 0.1, 
                                            animate = TRUE),
                                
                                sliderInput(inputId = "alpha2",
                                            label = "Alpha: ",
                                            min = 0,
                                            max = 1, 
                                            value = 0.3, 
                                            animate = TRUE)
                              ),
                              
                              mainPanel(
                                
                                plotOutput(outputId = "facetted_plot2"),
                                
                                br(), 
                                
                                # Explanatory text
                                HTML(paste0("The above plot facetted on genre.", br(), br())),  
                                            
                                
                                plotOutput(outputId = "plot2")
                                
                              )
                            )
                          )
                 ),


# Tab 3 -------------------------------------------------------------------
                 tabPanel(title = "Movie Table",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                # Numeric input for number of rows to show
                                numericInput(inputId = "n_rows",
                                             label = "How many rows do you want to see?",
                                             value = 10),
                                
                                # Action button to show
                                actionButton(inputId = "button", 
                                             label = "Show")
                              ),
                              mainPanel(
                                DT::dataTableOutput(outputId = "movietable")
                              )
                            )
                          )
                 )
)



# Server ------------------------------------------------------------------
server <- function(input, output) {

# Tab 1 -------------------------------------------------------------------
  output$facetted_plot1 <- renderPlot({
    test %>% 
      ggplot(aes_string(x = input$x1, y = input$y1)) + 
      geom_point(size = input$size1, 
                 alpha = input$alpha1) +
      facet_wrap(reformulate(input$facet1)) +
      # think about the below - hasn't worked yet 
      # facet_wrap(eval(expr(~ !!ensym(input$facet1)))) 
      # https://stackoverflow.com/questions/21588096/pass-string-to-facet-grid-ggplot2
      labs(title = input$plot_title_top1)
  })
  
  
  output$plot1 <- renderPlot({
    test %>% 
      ggplot(aes_string(x = input$x1, y = input$y1, color = input$color1)) + 
      geom_point(size = input$size1, 
                 alpha = input$alpha1, 
                 position = "jitter") +
      labs(title = input$plot_title_bottom1)
  })
  
  # Temp Brush Plot
  output$plotbrush_temp <- renderPlot({
    brushedPoints(test, input$plot1_brush) %>% 
      ggplot(aes(x = rating, y = release)) + 
      geom_point()
  })

  
# Tab 2 -------------------------------------------------------------------
  output$facetted_plot2 <- renderPlot({
    test %>% 
      ggplot(aes_string(x = input$x2, y = input$y2)) + 
      geom_point(size = input$size2, 
                 alpha = input$alpha2) +
      facet_wrap(reformulate(input$facet2)) +
      labs(title = input$plot_title_top2)
  })
  
  
  output$plot2 <- renderPlot({
    # Make sure values are valuable otherwise raise a silence
    req(input$genre2, input$release2, input$rating2, input$size2)
    
    test %>% 
      # Here might cause no points to be plotted - 
      # add a table or think about the alternative 
      filter(genre %in% input$genre2,
             releaseBin %in% input$release2,
             runtimeBin %in% input$runtime2,
             ratingBin %in% input$rating2) %>% 
      ggplot(aes_string(x = input$x2, y = input$y2)) +
      geom_point(size = input$size2, 
                 alpha = input$alpha2) +
      labs(title = input$plot_title_bottom2)
  })

  
# Tab 3 -------------------------------------------------------------------
  
}


# Run the application ----------------------------------------------------
shinyApp(ui = ui, server = server)