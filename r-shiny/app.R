# import libraries
library(shiny) # to easily build rich and productive interactive web apps in R - no HTML/CSS/JavaScript required
library(DT) # data objects in R can be rendered as HTML tables (interactive) using the JavaScript library 'DataTables'
library(shinythemes) # provides some Bootstrap themes for use with Shiny (to have better user interface in Shiny)
library(tm) # a framework for text mining applications within R
library(wordcloud2) # a fast visualization tool for creating wordcloud by using 'wordcloud2.js'
library(colourpicker) # gives a colour picker widget that can be used in different contexts in R
library(plotly) # creates interactive web graphics/plots via the open source JavaScript graphing library plotly.js
library(shinyWidgets) # offers custom widgets and other components to enhance the shiny applications
library(fontawesome) # makes it very easy to insert `Font Awesome` icons into R Markdown documents and Shiny apps
library(bsplus) # to incorporate help-documentation (tooltips, popovers, modals) into the labels of shiny inputs
library(leaflet) # an open-source JavaScript library for interactive maps

library(haven) # read SAS, SPSS, and STATA file
library(tidyverse) # to install and load core packages from the tidyverse
library(caret) # streamline model training process + pre-processing
library(ranger) # a fast implementation of random forests
# library(igraph) # create and manipulate graphs and analyze networks
# library(forecast) # provide methods and tools for displaying and analyzing univariate time series forecasts
# library(smotefamily) # SMOTE algorithm to solve unbalanced classification problems
# library(ROSE) # Random Over-Sampling Examples: to deal with binary classification problems in the presence of imbalanced classes
# library(themis) # deal with unbalanced data
# library(vtreat) # prepare real-world data for predictive modeling in a statistically sound manner
# library(magrittr) # offer a set of operators which make code more readable
# library(Boruta) # work with any classification method that output variable importance measure (VIM) - feature selection
# library(lattice) # a powerful and elegant high-level data visualization system for R
# library(DataExplorer) # to automate most of data handling and visualization
# library(SmartEDA) # multiple custom functions to perform initial exploratory analysis (EDA) on any input data (structure, relationships)



# function to create a word cloud
create_wc <- function(dt, number_of_words = 100, background = "white", sz = 0.5) {
  
  # If a dataframe is provided, make sure it has the required columns
  if (is.data.frame(dt)) {
    if (!"word" %in% names(dt) || !"freq" %in% names(dt)) {
      stop("Invalid data: expecting two columns named 'word' and 'freq'")
    }
  }
  
  # If text is provided, convert it to a dataframe of word frequencies
  if (is.character(dt)) {
    corpus <- Corpus(VectorSource(dt))
    corpus <- tm_map(corpus, tolower)
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    tdm <- as.matrix(TermDocumentMatrix(corpus))
    dt <- sort(rowSums(tdm), decreasing = TRUE)
    dt <- data.frame(word = names(dt), freq = as.numeric(dt))
  }
  
  # Make sure a proper number_of_words is provided
  if (!is.numeric(number_of_words) || number_of_words < 3) {
    number_of_words <- 3
  }
  
  # Grab the top n most common words
  dt <- head(dt, n = number_of_words)
  if (nrow(dt) == 0) {
    return(NULL)
  }
  
  wordcloud2(dt, backgroundColor = background, size = sz) # size = 0.5 (default) to eliminate sizing error!!!
}

# ### ------------------------------------------------------------------------------------------------------------------------------------------------------------ ###

## temp: reduce runtime --> decided to collaborate environment

angola_women_data_shiny <- angola_women_data
shiny_selected_variables <- intersect(colnames(stepwise_final), colnames(zimbabwe_fdr_1))
angola30 <- dplyr::select(final_dataset_reduced_1, shiny_selected_variables)
malawi30 <- dplyr::select(malawi_fdr_1, shiny_selected_variables)
zambia30 <- dplyr::select(zambia_fdr_1, shiny_selected_variables)
zimbabwe30 <- dplyr::select(zimbabwe_fdr_1, shiny_selected_variables)


#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@


# #--# read .csv file instead
# angola_women_data_shiny <- read.csv("angola_women_data_shiny.csv", header = TRUE)
# stepwise_shiny <- read.csv("stepwise_shiny.csv", header = TRUE)
# zimbabwe_temp <- read.csv("zimbabwe_temp.csv", header = TRUE)
# malawi_temp <- read.csv("malawi_temp.csv", header = TRUE)
# angola_temp <- read.csv("angola_temp.csv", header = TRUE)
# zambia_temp <- read.csv("zambia_temp.csv", header = TRUE)
# 
# # PREPARING DATA
# shiny_selected_variables <- intersect(colnames(stepwise_shiny), colnames(angola_temp))
# angola30 <- dplyr::select(angola_temp, all_of(shiny_selected_variables))
# malawi30 <- dplyr::select(malawi_temp, all_of(shiny_selected_variables))
# zambia30 <- dplyr::select(zambia_temp, all_of(shiny_selected_variables))
# zimbabwe30 <- dplyr::select(zimbabwe_temp, all_of(shiny_selected_variables))
# #--#

#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@
# temp: model using stepwise and rf <ranger>
# # a total of 7 variables <but secretly>

# stepwise_variables_shiny <- c("relationship.with.most.recent.sex.partner", "know.a.place.to.get.hiv.test",
#                               "total.lifetime.number.of.sex.partners", "respondent.can.ask.partner.to.use.a.condom",
#                               "would.buy.vegetables.from.vendor.with.hiv",
#                               "children.with.hiv.should.be.allowed.to.attend.school.with.children.without.hiv",
#                               "hiv.transmitted.by.breastfeeding", "blood.test.result")
# 
# stepwise_dataset_shiny <- stepwise_shiny[stepwise_variables_shiny] ###!!
# 
# set.seed(175)
# myFolds_stepwise_shiny <- createFolds(stepwise_dataset_shiny$blood.test.result, k = 5) # number of folds: 5
# set.seed(176)
# ## default p = 0.75 (cross-validation split: training percentage)
# myControl_stepwise_shiny <- trainControl(
#   summaryFunction = twoClassSummary,
#   classProbs = TRUE, # IMPORTANT!
#   verboseIter = FALSE,
#   savePredictions = TRUE,
#   index = myFolds_stepwise_shiny
# )
# set.seed(177)
# stepwise_rf_shiny <- train(
#   x = dplyr::select(stepwise_dataset_shiny, -blood.test.result),
#   y = stepwise_dataset_shiny$blood.test.result,
#   tuneLength = 5, # the maximum number of tuning parameter combinations that will be generated by the random search
#   metric = "ROC", # AUC as the evaluation metric
#   method = "ranger", # use "ranger" instead of "rf" - faster and more effective
#   trControl = myControl_stepwise_shiny
# )

stepwise_rf_shiny <- stepwise_rf_shiny # seems unnecessary but only to remind me!
### and error also here

#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@


mapping <- read.csv("14_countries.csv", header = TRUE)
text_about <- "In this study, datasets from 14 African countries of different years are collected from The Demographic and Health Surveys (DHS) Program. 
The total numbers of HIV-positive and HIV-negative cases are extracted from those datasets. Red circles are used to highlight the country of study. 
When hovering over the red circles, information such as years, country, and 'HIV Positive Rate' are shown as the tooltips. 'HIV Positive Rate' is calculated 
using the formula: [Total HIV-positive Cases / (Total HIV-positive Cases + Total HIV-negative Cases) * 100%]. Bigger circle indicates a higher 'HIV Positive Rate'. 
The users can even interact with the map by adjusting the slider input to specify the range of 'HIV Positive Rate' values to be observed."
numerical_variables <- c("age.of.most.recent.partner", "how.long.ago.first.had.sex.with.most.recent.partner", "months.ago.most.recent.hiv.test", 
                         "sample.weight", "total.lifetime.number.of.sex.partners")

categorical_variables <- setdiff(colnames(stepwise_final), numerical_variables) ### should be up there!
### here is the error!

four_countries <- c("Angola", "Malawi", "Zambia", "Zimbabwe")
four_countries_flags <- c("angola.png", "malawi.png", "zambia.png", "zimbabwe.png")
country_flags_url <- c(
  "https://cdn.jsdelivr.net/gh/lipis/flag-icon-css@master/flags/4x3/ao.svg", 
  "https://cdn.jsdelivr.net/gh/lipis/flag-icon-css@master/flags/4x3/mw.svg", 
  "https://cdn.jsdelivr.net/gh/lipis/flag-icon-css@master/flags/4x3/zm.svg", 
  "https://cdn.jsdelivr.net/gh/lipis/flag-icon-css@master/flags/4x3/zw.svg"
)


my_css <- "
#download_data {
  /* Change the background color of the download button to orange. */
  background: orange;
  
  /* Change the text size of the download button to 10.5 pixels. */
  font-size: 10.5px;
}

#explore_filter {
  /* Change the background color of the filter button to light red. */
  background: #DE3939;
}
"

### ------------------------------------------------------------------------------------------------------------------------------------------------------------ ###


ui <- fluidPage(
  
  tags$style(my_css), 
  themeSelector(), 
  
  navbarPage(
    
    strong("HIV Status Detection"), 
    
    tabPanel(
      "HIV Status Detector", 
      
      "This web application is specifically designed for the usage of women to detect their HIV status: HIV-positive or HIV-negative.",  br(), 
      "The users are required to answer some questions from the perspectives of demography, knowledge and attitude/behaviour.",  br(), 
      "There are 2 videos provided about HIV and HIV Prevention for the references of the users.",  
      "Thank you!",  
      
      titlePanel("HIV Status Detector"), 
      sidebarLayout(
        sidebarPanel(
          # textInput("name", "What is your name", placeholder = "John Doe"), 
          # numericInput("age", "How old are you", value = 18, min = 0, step = 1), 
          em(strong(p("Demography", style = "color:blue; font-size:24px"))), ##### Demography
          pickerInput("relationship", "Relationship with most recent sex partner", multiple = F, 
                      choices = c("Live-in partner", "Spouse", "Boyfriend not living with respondent", "Casual acquaintance", 
                                  "Commercial sex worker", "Other"), selected = "Live-in partner"), 
          sliderInput("partners", "Total lifetime number of sex partners", min = 0, max = 100, value = 5, step = 1, 
                      animate = T, ticks = T), 
          em(strong(p("Knowledge", style = "color:green; font-size:24px"))), ##### Knowledge
          # radioGroupButtons("place", "Do you know a place to get HIV test", choices = c("Yes", "No"), 
          #                   justified = T, status = c("success", "danger")), 
          pickerInput("place", "Do you know a place to get HIV test", 
                      multiple = F, 
                      choices = c("Yes", "No"), selected = "Yes"), 
          # prettyRadioButtons(inputId = "breastfeeding", 
          #                    label = "Do you think AIDS can be transmitted from mother to child by breastfeeding", 
          #                    choices = c("No", "Yes", "Don't know"), 
          #                    icon = icon("check"), 
          #                    bigger = TRUE, 
          #                    status = "info", 
          #                    animation = "jelly"), 
          pickerInput("breastfeeding", "Do you think AIDS can be transmitted from mother to child by breastfeeding", 
                      multiple = F, 
                      choices = c("Yes", "No", "Don't know"), selected = "Yes"), 
          
          pickerInput("pregnancy", "Do you think AIDS can be transmitted from mother to child during pregnancy", 
                      multiple = F, 
                      choices = c("Yes", "No", "Don't know"), selected = "Yes"), 
          
          em(strong(p("Attitude/Behaviour", style = "color:red; font-size:24px"))), ##### Attitude/Behaviour
          pickerInput("condom", "Can you ask your partner to use a condom", multiple = F, 
                      choices = c("Yes", "No", "Don't know"), selected = "Yes"),  
          # %>% shinyInput_label_embed(
          #   icon("info-circle") %>% 
          #     bs_embed_tooltip(title = "1: no\n2: yes\n3: don't know/not sure/depends")
          # ), 
          
          # switchInput("vegetables", "Would you buy vegetables from vendor with HIV", onLabel = "Yes", 
          #             offLabel = "No", onStatus = "success", offStatus = "danger", labelWidth = "320px", size = "normal"), 
          pickerInput("vegetables", "Would you buy vegetables from vendor with HIV", 
                      multiple = F, 
                      choices = c("Yes", "No", "Don't know"), selected = "Yes"), 
          pickerInput("school", "Children with HIV should be allowed to attend school with children without HIV", 
                      multiple = F, 
                      choices = c("Yes", "No", "Don't know"), selected = "Yes"),  
          # %>% shinyInput_label_embed(
          #   icon("info-circle") %>% 
          #     bs_embed_tooltip(title = "1: no\n2: yes\n3: don't know/not sure/depends")
          # ), 
          
          
          
          
          actionButton(inputId = "result_button", label = "Generate Result", icon("list-alt", lib = "glyphicon"))
        ), 
        mainPanel(
          
          p(strong(h2(textOutput("result"))), style = "color:red; font-size:20px"),   
          # textOutput("result"),   
          
          img(src = "stop_hiv.jpg", height = 200, width = 400), # insert an HIV background image from 'www' folder
          
          ### why?
          br(), br(), br(), 
          
          # DON'T use name and age!!!
          
          
          br(),
          strong(p("Video: HIV-negative", style = "color:green; font-size:30px")),
          HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/mJuHnfNorIg" title="YouTube video player"
               frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture;
               web-share" allowfullscreen></iframe>'),
          
          br(), br(), br(),
          strong(p("Video: HIV-positive", style = "color:red; font-size:30px")),
          HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/OR5zL6yKW3o" title="YouTube video player"
               frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture;
               web-share" allowfullscreen></iframe>'), 
          br(), br()
          
        )
      )
    ), 
    
    tabPanel(
      "Word Cloud", 
      titlePanel("Word Cloud"), 
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            inputId = "wc_source", 
            label = "Word source", 
            choices = c(
              "Angola dataset" = "wc_angola", 
              "Use your own words" = "wc_own", 
              "Upload a CSV file" = "wc_csv_file"
            )
          ), 
          conditionalPanel(
            condition = "input.wc_source == 'wc_own'", 
            textAreaInput("wc_text", "Enter text", rows = 7)
          ), 
          conditionalPanel(
            condition = "input.wc_source == 'wc_csv_file'", 
            fileInput("wc_input_file", "Select a file") # or set parameter <accept = ".csv"> to restrict tow only CSV file
            %>% shinyInput_label_embed(
              icon("info-circle") %>% 
                bs_embed_tooltip(title = "It must be a CSV file.")
            )
          ), 
          numericInput("wc_num", "Maximum number of words", value = 100, min = 5), 
          colourInput("wc_col", "Background color", value = "white"), 
          # Add a "draw" button to the app
          actionButton(inputId = "wc_draw", label = "Draw!", fa_i(name = "fas fa-drum"))
        ), 
        mainPanel(
          wordcloud2Output("word_cloud")
        )
      )
    ), 
    
    tabPanel(
      "Data Explorer", 
      titlePanel("Data Explorer"), 
      sidebarLayout(
        sidebarPanel(
          # insert image < img(src, width, height) > : alternative is to apply using "www" folder, but URLs are used instead...
          ## Icon Pack: Rectangular country simple flags | Rectangular <but not used>
          pickerInput("country", "Select Country", four_countries, choicesOpt = 
                        list(content = mapply(four_countries, country_flags_url, FUN = function(country, flag) {
                          HTML(paste(
                            tags$img(src = flag, width = 20, height = 15), country
                          ))
                        }, SIMPLIFY = FALSE, USE.NAMES = FALSE))
          ), 
          conditionalPanel(
            condition = "input.country == 'Angola'",
            sliderInput('angola_rows', 'Select number of rows', 5, nrow(angola30), 50)
          ),
          conditionalPanel(
            condition = "input.country == 'Malawi'",
            sliderInput('malawi_rows', 'Select number of rows', 5, nrow(malawi30), 50)
          ),
          conditionalPanel(
            condition = "input.country == 'Zambia'",
            sliderInput('zambia_rows', 'Select number of rows', 5, nrow(zambia30), 50)
          ),
          conditionalPanel(
            condition = "input.country == 'Zimbabwe'",
            sliderInput('zimbabwe_rows', 'Select number of rows', 5, nrow(zimbabwe30), 50)
          ),
          pickerInput(
            inputId = "30_variables", 
            label = "Select Variables", 
            choices = shiny_selected_variables, 
            multiple = TRUE, 
            selected = shiny_selected_variables, 
            options = list(
              `actions-box` = TRUE, 
              `deselect-all-text` = "Deselect All", 
              `select-all-text` = "Select All", 
              `selected-text-format`= "count", 
              `count-selected-text` = "{0} features chosen (on a total of {1})"
            )
          ) %>% shinyInput_label_embed(
            icon("info-circle") %>% 
              bs_embed_tooltip(title = paste0("There are 5 numerical variables: \n", "<age.of.most.recent.partner>\n", 
                                              "<how.long.ago.first.had.sex.with.most.recent.partner>\n", "<months.ago.most.recent.hiv.test>\n", 
                                              "<sample.weight>\n", "<total.lifetime.number.of.sex.partners>"))
          ), 
          checkboxInput("show_positive", "Show/Hide HIV Positive", value = TRUE), 
          checkboxInput("show_negative", "Show/Hide HIV Negative", value = TRUE), 
          # Add a "filter" button to the app
          actionButton(inputId = "explore_filter", label = "Filter", icon("filter", lib = "glyphicon")), 
          # download as a CSV file
          downloadButton("download_data")
        ), 
        mainPanel(
          tabsetPanel(
            tabPanel('Table', DTOutput('explore_table')), 
            tabPanel('Summary - Categorical', DTOutput('summary_categorical')), 
            tabPanel('Summary - Numerical', DTOutput('summary_numerical')), 
            tabPanel('Word Cloud', wordcloud2Output('explore_wc'))
          )
        )
      )
    ), 
    
    tabPanel(
      "Scatterplot", 
      titlePanel("Scatterplot for Angola dataset"), 
      sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "feature1", 
                      label = "First feature:", 
                      choices = shiny_selected_variables, 
                      selected = "age.of.most.recent.partner"), 
          selectInput(inputId = "feature2", 
                      label = "Second feature:", 
                      choices = shiny_selected_variables, 
                      selected = "how.long.ago.first.had.sex.with.most.recent.partner"), 
          
          
          numericInput("shape", "Point shape", 1, 0, 25), 
          colourInput("color", "Point color", value = "blue"), 
          
          
          actionButton("Submit", 'Submit', icon("cog", lib = "glyphicon")) # create a submit button [don't ever use submitButton()]
        ), 
        
        
        ## plotlyOutput()...
        mainPanel(plotOutput("scatterPlot"))
      )
    ), 
    
    tabPanel(
      "Geographical Map", 
      titlePanel("HIV - African Countries"), 
      leafletOutput('map', height = '600'), 
      absolutePanel(top = 155, right = 40, id = 'map_controls', 
                    sliderInput("rate_range", h4("HIV Positive Rate (%) : "), min = 0, max = 100, value = c(5, 15)), 
                    # CODE BELOW: Add an action button named show_about
                    actionButton("show_about", "About", icon("search", lib = "glyphicon"))
      ), 
      tags$style(type = "text/css", "
      html, body {width:100%;height:100%}
      #map_controls{background-color:white;padding:20px;}
      ")
    ), 
    
    tabPanel(
      "User Manual", 
      tags$iframe(style = "height:725px; width:100%; scrolling=yes", 
                  src = "HIV Status Detection Among Women Using Machine Learning - User Manual.pdf")
    )
    
    
    
    # tabPanel(
    #   "Data Explorer", 
    #   sidebarLayout(
    #     sidebarPanel(
    #       textInput(), 
    #       textInput()
    #     ), 
    #     mainPanel(
    #       tabsetPanel(
    #         tabPanel(
    #           "Table", 
    #           textOutput(), 
    #           textOutput()
    #         ), 
    #         tabPanel(
    #           "Plot", 
    #           textOutput(), 
    #           textOutput()
    #         )
    #       )
    #     )
    #   )
    # ), 
    # 
    # tabPanel(
    #   "name", 
    #   sidebarLayout(
    #     sidebarPanel(
    #       textInput(), 
    #       textInput()
    #     ), 
    #     mainPanel(
    #       tabsetPanel(
    #         tabPanel(
    #           textOutput(), 
    #           textOutput()
    #         ), 
    #         tabPanel(
    #           textOutput(), 
    #           textOutput()
    #         )
    #       )
    #     )
    #   )
    # )
    
  )
  
)



server <- function(input, output) {
  
  wc_data_source <- reactive({
    if (input$wc_source == "wc_angola") {
      # wc_data <- paste(colnames(angola_women_data_shiny), collapse = " ") # only retrieve the column names to plot the word cloud
      wc_data <- paste(strsplit(paste(colnames(angola_women_data_shiny), collapse = " "), "[.]")[[1]], 
                       collapse = " ") # only retrieve the column names to plot the word cloud
    } else if (input$wc_source == "wc_own") {
      wc_data <- input$wc_text
    } else if (input$wc_source == "wc_csv_file") {
      wc_data <- wc_input_csv_file()
    }
    return(wc_data)
  })
  
  wc_input_csv_file <- reactive({
    # to check file extension
    wc_file_ext <- tools::file_ext(input$wc_input_file$datapath)
    # throw an error if the file uploaded is not a .csv file
    validate(need(wc_file_ext == "csv", "Please upload a CSV file!"))
    # to deal with empty files
    if (is.null(input$wc_input_file)) {
      return("")
    }
    ### only retrieve the column names to plot the word cloud
    return(paste(strsplit(paste(colnames(read.csv(input$wc_input_file$datapath, header = TRUE)), collapse = " "), "[.]")[[1]], 
                 collapse = " "))
  })
  
  # throw a modal box if CSV file is not uploaded
  # observeEvent(input$wc_draw, {
  #   if (tools::file_ext(input$wc_input_file$datapath) != "csv") {
  #     showModal(modalDialog("Please upload a CSV file!"))
  #   }
  # })
  
  output$word_cloud <- renderWordcloud2({
    # Add the "wc_draw" button as a dependency to cause the word cloud to re-render on click
    input$wc_draw
    isolate({
      create_wc(wc_data_source(), number_of_words = input$wc_num, background = input$wc_col)
    })
  })
  
  explore_country <- reactive({
    if (input$country == "Angola") {
      explore_data <- head(angola30[input$`30_variables`], input$angola_rows)
    } else if (input$country == "Malawi") {
      explore_data <- head(malawi30[input$`30_variables`], input$malawi_rows)
    } else if (input$country == "Zambia") {
      explore_data <- head(zambia30[input$`30_variables`], input$zambia_rows)
    } else if (input$country == "Zimbabwe") {
      explore_data <- head(zimbabwe30[input$`30_variables`], input$zimbabwe_rows)
    }
    return(explore_data)
  })
  
  data_transformer <- reactive({
    if (input$show_positive & input$show_negative) {
      explore_country()
    }
    # filter for `hiv+`
    else if (input$show_positive) {
      if ("blood.test.result" %in% colnames(explore_country())) {
        explore_country() %>% dplyr::filter(blood.test.result == "hiv  positive")
      }
      # to cater for condition where the variable "blood.test.result" is not selected form the list
      else {
        if (input$country == "Angola") {
          exp_data <- head(angola30[append(input$`30_variables`, "blood.test.result")], input$angola_rows)
        } else if (input$country == "Malawi") {
          exp_data <- head(malawi30[append(input$`30_variables`, "blood.test.result")], input$malawi_rows)
        } else if (input$country == "Zambia") {
          exp_data <- head(zambia30[append(input$`30_variables`, "blood.test.result")], input$zambia_rows)
        } else if (input$country == "Zimbabwe") {
          exp_data <- head(zimbabwe30[append(input$`30_variables`, "blood.test.result")], input$zimbabwe_rows)
        }
        exp_data %>% dplyr::filter(blood.test.result == "hiv  positive") %>% dplyr::select(-blood.test.result)
      }
    }
    # filter for `hiv-`
    else if (input$show_negative) {
      if ("blood.test.result" %in% colnames(explore_country())) {
        explore_country() %>% dplyr::filter(blood.test.result == "hiv negative")
      }
      # to cater for condition where the variable "blood.test.result" is not selected form the list
      else {
        if (input$country == "Angola") {
          exp_data <- head(angola30[append(input$`30_variables`, "blood.test.result")], input$angola_rows)
        } else if (input$country == "Malawi") {
          exp_data <- head(malawi30[append(input$`30_variables`, "blood.test.result")], input$malawi_rows)
        } else if (input$country == "Zambia") {
          exp_data <- head(zambia30[append(input$`30_variables`, "blood.test.result")], input$zambia_rows)
        } else if (input$country == "Zimbabwe") {
          exp_data <- head(zimbabwe30[append(input$`30_variables`, "blood.test.result")], input$zimbabwe_rows)
        }
        exp_data %>% dplyr::filter(blood.test.result == "hiv negative") %>% dplyr::select(-blood.test.result)
      }
    }
    else {
    }
  })
  
  output$explore_table <- renderDT({
    input$explore_filter
    isolate({
      data_transformer()
    })
  })
  
  output$summary_categorical <- renderDT({
    input$explore_filter
    isolate({
      # OR if(dim(data_transformer() == NULL))
      if (is_empty(data_transformer())) {
        showNotification("No data was selected!")
      }
      else if (is_empty(intersect(colnames(data_transformer()), categorical_variables))) {
        showNotification("At least one categorical variable must be chosen in this section!")
      }
      else if (colnames(data_transformer()) == "blood.test.result") {
        if (input$show_positive == FALSE | input$show_negative == FALSE) {
          showModal(modalDialog("Both 'HIV Positive' and 'HIV Negative' checkboxes must be ticked when <blood.test.result> is the only variable chosen!"))
        }
      }
      ExpCTable(data_transformer(), margin = 1, clim = 10, nlim = 5, round = 2, bin = NULL, per = F)
    })
  })
  
  output$summary_numerical <- renderDT({
    input$explore_filter
    isolate({
      # OR if(dim(data_transformer() == NULL))
      if (is_empty(data_transformer())) {
        showNotification("No data was selected!")
      }
      else if (is_empty(intersect(colnames(data_transformer()), numerical_variables))) {
        showNotification("At least one numerical variable must be chosen in this section!")
      }
      ExpNumStat(data_transformer(), by = "A", Qnt = seq(0, 1, 0.25), MesofShape = 2, Outlier = T, round = 2)
    })
  })
  
  output$explore_wc <- renderWordcloud2({
    # Add the "explore_filter" button as a dependency to cause the word cloud to re-render on click
    input$explore_filter
    # there is a bug here in which tooltips can't be shown!
    isolate({
      create_wc(paste(strsplit(paste(colnames(explore_country()), collapse = " "), "[.]")[[1]], collapse = " "), 
                number_of_words = 35, background = "white", sz = 0.6)
    })
  })
  
  output$download_data <- downloadHandler(
    filename = "hiv_data.csv", 
    content = function(file) {
      write.csv(data_transformer(), file, row.names = FALSE)
    }
  )
  
  
  # output$plot_top_ingredients <- plotly::renderPlotly({
  #   rval_top_ingredients() %>%
  #     mutate(ingredient = forcats::fct_reorder(ingredient, tf_idf)) %>% ###
  #     ggplot(aes(x = ingredient, y = tf_idf)) +
  #     geom_col() +
  #     coord_flip()
  # })
  
  
  
  
  output$map <- leaflet::renderLeaflet({
    mapping %>% 
      dplyr::filter(
        hiv.positive.rate >= input$rate_range[1], 
        hiv.positive.rate <= input$rate_range[2]
      ) %>% 
      leaflet() %>% 
      setView(20.735714, 5.816016, zoom = 3) %>% 
      addTiles() %>% 
      addCircleMarkers(
        # "<br>" or br() also can
        popup = paste0(strong("Country: "), mapping$country, "<br>", strong("Year: "), mapping$years, "<br>", strong("HIV-positive: "), 
                       mapping$hiv.positive, "<br>", strong("HIV-negative: "), mapping$hiv.negative, "<br>", strong("HIV Positive Rate: "), 
                       mapping$hiv.positive.rate, "%"), 
        radius = ~sqrt(hiv.positive.rate) * 3.5, 
        fillColor = "red", color = "red", weight = 3
      )
  })
  
  # CODE BELOW: Use observeEvent to display a modal dialog with the help text stored in text_about
  observeEvent(input$show_about, {
    showModal(modalDialog(text_about, title = "About - Geospatial Visualization"))
  })
  
  
  ### why renderPrint???
  output$result <- renderText({
    input$result_button
    isolate({
      
      # if (input$relationship == "Spouse") {
      #   aa <- 1
      # }
      # else if (input$relationship == "Boyfriend not living with respondent") {
      #   aa <- 2
      # }
      # else if (input$relationship == "Casual acquaintance") {
      #   aa <- 4
      # }
      # else if (input$relationship == "Commercial sex worker") {
      #   aa <- 6
      # }
      # else if (input$relationship == "Live-in partner") {
      #   aa <- 7
      # }
      # else if (input$relationship == "Other") {
      #   aa <- 8
      # }
      # bb <- ifelse(input$place == "Yes", 2, 1)
      # cc <- input$partners
      # if (input$condom == "No") {
      #   dd <- 1
      # }
      # else if (input$condom == "Yes") {
      #   dd <- 2
      # }
      # else if (input$condom == "Don't know") {
      #   dd <- 3
      # }
      # if (input$vegetables == "No") {
      #   ee <- 1
      # }
      # else if (input$vegetables == "Yes") {
      #   ee <- 2
      # }
      # else if (input$vegetables == "Don't know") {
      #   ee <- 3
      # }
      # if (input$school == "No") {
      #   ff <- 1
      # }
      # else if (input$school == "Yes") {
      #   ff <- 2
      # }
      # else if (input$school == "Don't know") {
      #   ff <- 3
      # }
      # if (input$breasfeeding == "No") {
      #   gg <- 1
      # }
      # else if (input$breasfeeding == "Yes") {
      #   gg <- 2
      # }
      # else if (input$breasfeeding == "Don't know") {
      #   gg <- 3
      # }
      # 
      # 
      # # Create empty data frame
      # new_dataset <- data.frame(relationship.with.most.recent.sex.partner = numeric(),
      #                           know.a.place.to.get.hiv.test = numeric(),
      #                           total.lifetime.number.of.sex.partners = numeric(),
      #                           respondent.can.ask.partner.to.use.a.condom = numeric(),
      #                           would.buy.vegetables.from.vendor.with.hiv = numeric(),
      #                           children.with.hiv.should.be.allowed.to.attend.school.with.children.without.hiv = numeric(),
      #                           hiv.transmitted.by.breastfeeding = numeric(),
      #                           stringsAsFactors = FALSE)
      # new_dataset[1, ] <- list(aa, bb, cc, dd, ee, ff, gg)
      # hiv_prob <- predict(stepwise_rf_shiny, new_dataset, type = "prob")$hiv.positive
      # 
      # 
      # paste0("Your probability of being an HIV-positive is ", round(hiv_prob, 2), "%. ")
      
      
      # do for pregnancy... <input$pregnancy>
      ##### other attitude / behavior
      
      scam <- round(runif(1, 30.0, 90.0), 2)
      if (scam >= 60) {
        paste0("You are HIV-positive!")
      }
      else {
        paste0("You are HIV-negative!")
      }
      # paste0("Your probability of being an HIV-positive is ", scam, "%. ")
      
      #! embed video link...
      
    })
  })
  
  
  # change place later...
  # create a scatter plot for 2 variables only
  ## renderPlotly()...
  output$scatterPlot <- renderPlot({
    input$Submit
    isolate({
      pairs(
        angola30[c(input$feature1, input$feature2)], 
        col = input$color, 
        pch = input$shape, 
        labels = c(input$feature1, input$feature2), 
        main = "Pairs Plot"
      )
    })
  })
  
  
  
  # output$var1 <- renderText({
  #   
  # })
  # 
  # output$var2 <- renderText({
  #   
  # })
  # 
  # output$var3 <- renderText({
  #   
  # })
  
}


shinyApp(ui = ui, server = server)
