# Packages ----------------------------------------------------------------
library(shiny)
library(tidyverse)
library(rlang)
library(DT)
library(plotly)
library(htmlwidgets)


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
                         labels = seq(2, 10, 1)),
         genre = recode(genre, noGenre = "No Genre")) %>% 
  select(-isAdult) %>% # Discard isAdult column
  # Rename cols to allow plot label automation
  rename(id = tconst, title = primaryTitle, runtime = runtimeMinutes,
         rating = averageRating, numberOfVotes = numVotes) 

  # Change genre levels - move "No Genre" to the last
test <- test %>%
  mutate(genre = factor(test$genre, 
                        levels = c(levels(test$genre)[-17], levels(test$genre)[17]), 
                        labels = c(levels(test$genre)[-17], levels(test$genre)[17])))



# Check test column types
lapply(test, class) %>% unlist()


# Ui labels
genre_var <- levels(droplevels(unique(test$genre)))
release_var <- levels(droplevels(unique(test$releaseBin)))
runtime_var <- levels(droplevels(unique(test$runtimeBin)))
rating_var <- levels(droplevels(unique(test$ratingBin)))


# Clean up colnames for labels
clean_label <- function(x) {str_replace_all(x, "([:upper:])", " \\1") %>% str_to_title}


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
                                
                                # Visual separation
                                hr(), 
                                
                                selectInput(inputId = "x1", 
                                            label = "X axis: ", 
                                            choices = c("Release" = "release",
                                                        "Runtime" = "runtime",
                                                        "Rating" = "rating", 
                                                        "Number of Votes" = "numberOfVotes"),
                                            selected = "rating"),
                                
                                selectInput(inputId = "y1", 
                                            label = "Y axis: ", 
                                            choices = c("Release" = "release",
                                                        "Runtime" = "runtime",
                                                        "Rating" = "rating", 
                                                        "Number of Votes" = "numberOfVotes"),
                                            selected = "numberOfVotes"),
                                
                                # Visual separation
                                hr(), 
                                
                                selectInput(inputId = "facet1", 
                                            label = "Facet: ", 
                                            choices = c("Genre" = "genre",
                                                        "Release Bin" = "releaseBin",
                                                        "Runtime Bin" = "runtimeBin",
                                                        "Rating Bin" = "ratingBin"),
                                            selected = "runtimeBin"),
                                
                                # Visual separation 
                                hr(), 
                                
                                selectInput(inputId = "color1", 
                                            label = "Color: ", 
                                            choices = c("Genre" = "genre",
                                                        "Release Bin" = "releaseBin",
                                                        "Runtime Bin" = "runtimeBin",
                                                        "Rating Bin" = "ratingBin"),
                                            selected = "genre"), 
                                
                                # Visual separation
                                hr(), 
                                
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
                                
                                br(), 
                                
                                plotOutput(outputId = "facetted_plot1"),
                                
                                br(),  
                                
                                # Explanatory text
                                uiOutput(outputId = "text1"),
                                
                                hr(), 
                                
                                # Static explanatory text
                                HTML(paste0("You can select a region below
                                            (and drag it around) to have further
                                            investigation on the data set.")),
                                
                                br(), br(), 
                                
                                plotOutput(outputId = "plot1",
                                           brush = "plot1_brush_coord"),
                                
                                # Visual separation
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
                                
                                plotOutput(outputId = "plot_brush1")
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
                                
                                # Visual separation
                                hr(),  
                                
                                selectInput(inputId = "x2", 
                                            label = "X axis: ", 
                                            choices = c("Release" = "release",
                                                        "Runtime" = "runtime",
                                                        "Rating" = "rating", 
                                                        "Number of Votes" = "numberOfVotes"),
                                            selected = "rating"),
                                
                                selectInput(inputId = "y2", 
                                            label = "Y axis: ", 
                                            choices = c("Release" = "release",
                                                        "Runtime" = "runtime",
                                                        "Rating" = "rating", 
                                                        "Number of Votes" = "numberOfVotes"),
                                            selected = "numberOfVotes"),
                                
                                selectInput(inputId = "facet2", 
                                            label = "Facet: ", 
                                            choices = c("Genre" = "genre",
                                                        "Release Bin" = "releaseBin",
                                                        "Runtime Bin" = "runtimeBin",
                                                        "Rating Bin" = "ratingBin")),

                                # Visual separation
                                hr(), 
                                
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
                                
                                # Visual separation
                                hr(), 
                                
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
                                
                                br(), 
                                
                                plotOutput(outputId = "facetted_plot2"),
                                
                                br(), 
                                
                                # Explanatory text
                                uiOutput(outputId = "text2"),
                                
                                # Visual separation
                                hr(), 
                                
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
                              plotlyOutput(outputId = "plot4")
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
      str_to_title(input$plot_title_top1)
    },
    ignoreNULL = FALSE
  )
  
  update_bottom_plot_title1 <- eventReactive(
    input$show_title1, {
      str_to_title(input$plot_title_bottom1)
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
      labs(title = update_top_plot_title1(), 
           x = clean_label(input$x1),
           y = clean_label(input$y1)) 
  })
  
  # Explanatory test
  output$text1 <- renderUI({paste0("The plot above is facetted on ", 
                                   '"', clean_label(input$facet1), '"',
                                   " whereas the plot below combines facets and shows ",  
                                   '"', clean_label(input$color1), '"', 
                                   " by colors.")})
  
  output$plot1 <- renderPlot({
    test %>% 
      ggplot(aes_string(x = input$x1, y = input$y1, color = input$color1)) + 
      geom_point(size = input$size1, 
                 alpha = input$alpha1, 
                 position = "jitter") +
      labs(title = update_bottom_plot_title1(),
           x = clean_label(input$x1), 
           y = clean_label(input$y1)) 
  })
  
  # Brush Plot
  output$plot_brush1 <- renderPlot({
    req(input$plot1_brush_coord)
    brushedPoints(test, input$plot1_brush_coord) %>% 
      ggplot(aes_string(x = input$x1, y = input$y1)) + 
      geom_point(size = input$size1,
                 alpha = input$alpha1) +
      labs(x = clean_label(input$x1),
           y = clean_label(input$y1))
  })
  
  
# Tab 2 -------------------------------------------------------------------
  # Update plot title when action button is clicked
  update_top_plot_title2 <- eventReactive(
    input$show_title2, {
      str_to_title(input$plot_title_top2)
    },
    ignoreNULL = FALSE
  )
  
  update_bottom_plot_title2 <- eventReactive(
    input$show_title2, {
      str_to_title(input$plot_title_bottom2)
    },
    ignoreNULL = FALSE
  )
  
  
  output$facetted_plot2 <- renderPlot({
    test %>% 
      ggplot(aes_string(x = input$x2, y = input$y2)) + 
      geom_point(size = input$size2, 
                 alpha = input$alpha2) +
      facet_wrap(reformulate(input$facet2)) +
      labs(title = update_top_plot_title2(),
           x = clean_label(input$x2),
           y = clean_label(input$y2))
  })
  
  # Explanatory text
  output$text2 <- renderUI({
    paste0("The above plot is faceted on ",
           '"', clean_label(input$facet2), '".')
  })
  
  # Will only draw plot background when filter result is no points
  output$plot2 <- renderPlot({
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
      labs(title = update_bottom_plot_title2(),
           x = clean_label(input$x2),
           y = clean_label(input$y2))
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
      colnames = clean_label(colnames(test)), 
      options = list(bLengthChange = FALSE, # Removes the pageLength drop down
                     pageLength = n_rows_to_show())
    )
  )
  
  
# Tab 4 -------------------------------------------------------------------
  output$movietable4 <- DT::renderDataTable(
    DT::datatable(
      data = test,
      colnames = clean_label(colnames(test)), 
      options = list(
        pageLength = 20
      )
    )
  )
  
  output$plot4 <- renderPlotly({
    indicies <- input$movietable4_rows_selected
    req(indicies)
    p <- test %>% 
      slice(indicies) %>% 
      ggplot(aes(x = rating, y = numberOfVotes, 
                 color = ratingBin, shape = runtimeBin)) +
      geom_point(size = 4) 
    ggplotly(p)
  })
  
}


# Run the application ----------------------------------------------------
shinyApp(ui = ui, server = server)