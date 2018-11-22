# Packages ----------------------------------------------------------------
library(shiny)
library(tidyverse)
library(rlang)
library(DT)
library(plotly)
library(htmlwidgets)
library(tools)


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
  select(-bool, -originalTitle) %>% 
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
colnames(test)[6] <- "rating"


# Further adjustment on the data set 
test <- test %>% select(-isAdult) # Remove isAdult column

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
                                          placeholder = "Enter text for plot title."),
                                
                                textInput(inputId = "plot_title_bottom1", 
                                          label = "Bottom Plot Title", 
                                          placeholder = "Enter text for plot title."),
                                
                                actionButton(inputId = "show_title1",
                                             label = "Show"),
                                br(), br(), 
                                
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
                                            value = 3.5,
                                            step = 0.1),
                                
                                sliderInput(inputId = "alpha1",
                                            label = "Alpha: ",
                                            min = 0,
                                            max = 1, 
                                            value = 1)
                                
                              ),
                              
                              mainPanel(
                                
                                plotOutput(outputId = "facetted_plot1"),
                                
                                br(),  
                                
                                # Explanatory text
                                HTML(paste0("The plot above is facetted on ", 
                                            "input$___ ", 
                                            "whereas the plot below combines facets and shows ",  
                                            "input$___ ", 
                                            "by colors.")),
                                
                                br(), br(), 
                                
                                plotlyOutput(outputId = "plot1"), 
                                              # brush = "plot1_brush_coord"), 
                                              # temporarily disabled the brush feature in the second plot
                                
                                # Add a visual separation
                                hr(), 
                                
                                # Explanatory text
                                HTML(paste0("The plot below shows the brushed area in the plot above.", 
                                            br(),  
                                            "It's possible to see fewer points in the plot below
                                            since we have added some random noises to the plot above
                                            for visual clarity - to avoid the same movie labelled with
                                            different genres overlappong. 
                                            (Each movie can be labelled from zero to three genres.)")),
                                
                                br(), br(), 
                                
                                plotOutput(outputId = "plot_brush_temp1")
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
                                          placeholder = "Enter text for plot title."),
                                
                                textInput(inputId = "plot_title_bottom2", 
                                          label = "Bottom Plot Title", 
                                          placeholder = "Enter text for plot title."),
                                
                                actionButton(inputId = "show_title2",
                                             label = "Show"),
                                br(), br(), 
                                
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
                                            step = 0.1),
                                
                                sliderInput(inputId = "alpha2",
                                            label = "Alpha: ",
                                            min = 0,
                                            max = 1, 
                                            value = 0.3)
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
                                numericInput(inputId = "n_rows3",
                                             label = "How many rows do you want to see?",
                                             value = 15, 
                                             min = 1),
                                
                                # Action button to show
                                actionButton(inputId = "show_button3", 
                                             label = "Show")
                              ),
                              mainPanel(
                                DT::dataTableOutput(outputId = "movietable3")
                              )
                            )
                          )
                 ),
                 
                 
# Tab 4 -------------------------------------------------------------------
                 tabPanel(title = "Table Info to Plot",
                          fluidPage(
                            splitLayout(
                              DT::dataTableOutput(outputId = "movietable4"),
                              plotOutput(outputId = "plot4")
                            )
                          )
                  )
                 
                 
                 
)



# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  
# Tab 1 -------------------------------------------------------------------
  # Two different values to be updated via the same button
  # by referring to the same action button Id in reactive values
  update_top_plot_title1 <- eventReactive(
    input$show_title1, {
      toTitleCase(input$plot_title_top1)
    },
    ignoreNULL = FALSE
  )
  
  update_bottom_plot_title1 <- eventReactive(
    input$show_title1, {
      toTitleCase(input$plot_title_bottom1)
    },
    ignoreNULL = FALSE
  )
  
  
  output$facetted_plot1 <- renderPlot({
    test %>% 
      ggplot(aes_string(x = input$x1, y = input$y1)) + 
      geom_point(size = input$size1, 
                 alpha = input$alpha1) +
      facet_wrap(reformulate(input$facet1)) +
      # think about the below - hasn't worked yet 
      # facet_wrap(eval(expr(~ !!ensym(input$facet1)))) 
      # https://stackoverflow.com/questions/21588096/pass-string-to-facet-grid-ggplot2
      labs(title = update_top_plot_title1())
  })
  
  
  output$plot1 <- renderPlotly({
    p <- test %>% 
      ggplot(aes_string(x = input$x1, y = input$y1, color = input$color1)) + 
      geom_point(size = input$size1, 
                 alpha = input$alpha1, 
                 position = "jitter") +
      labs(title = update_bottom_plot_title1()) 
    ggplotly(p)
  })
  
  # Temp Brush Plot
  output$plot_brush_temp1 <- renderPlot({
    brushedPoints(test, input$plot1_brush_coord) %>% 
      ggplot(aes(x = rating, y = release)) + 
      geom_point()
  })
  
  
# Tab 2 -------------------------------------------------------------------
  # Update plot title when action button is clicked
  update_top_plot_title2 <- eventReactive(
    input$show_title2, {
      toTitleCase(input$plot_title_top2)
    },
    ignoreNULL = FALSE
  )
  
  update_bottom_plot_title2 <- eventReactive(
    input$show_title2, {
      toTitleCase(input$plot_title_bottom2)
    },
    ignoreNULL = FALSE
  )
  
  
  output$facetted_plot2 <- renderPlot({
    test %>% 
      ggplot(aes_string(x = input$x2, y = input$y2)) + 
      geom_point(size = input$size2, 
                 alpha = input$alpha2) +
      facet_wrap(reformulate(input$facet2)) +
      labs(title = update_top_plot_title2())
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
      labs(title = update_bottom_plot_title2())
  })
  
  
# Tab 3 -------------------------------------------------------------------
  # For more DT info - https://rstudio.github.io/DT/shiny.html
  # output$movietable3 <- DT::renderDataTable({
  #   datatable(data = test,
  #             options = list(pageLength = isolate({input$n_rows3})),
  #             rownames = FALSE)
  # })
  
  
  # Check whether the input row number is truthy and cached for later use (render) 
  n_rows_to_show <- eventReactive(input$show_button3, {
    req(input$n_rows3)
    }, 
    ignoreNULL = FALSE
  )
  
  output$movietable3 <- DT::renderDataTable(
    DT::datatable(
      data = test,
      options = list(bLengthChange = FALSE, # Removes the pageLength drop down
                     pageLength = n_rows_to_show())
    )
  )
  
  
# Tab 4 -------------------------------------------------------------------
  output$movietable4 <- DT::renderDataTable(
    DT::datatable(
      data = test,
      options = list(
        pageLength = 20
      )
    )
  )
  
  output$plot4 <- renderPlot({
    indicies <- input$movietable4_rows_selected
    req(indicies)
    test %>% 
      slice(indicies) %>% 
      ggplot(aes(x = rating, y = numVotes, 
                 color = ratingBin, shape = runtimeBin)) +
      geom_point(size = 4)
  })
  
}


# Run the application ----------------------------------------------------
shinyApp(ui = ui, server = server)