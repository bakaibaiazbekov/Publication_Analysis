library(dplyr)
library(shinydashboard)
library(DT)
library(plotly)
library(dashboardthemes)
library(shiny)

#library(tabulizerjars)
library(textreadr)
library(pdftools)
library(tm)
library(memoise)
library(stringr)

library(tidyverse)
library(tidytext)
library(widyr)
library(igraph)
library(ggraph)
library(lexRankr)
library(pdftools)
library(tools)
library(wordcloud)
library(scholar)
#library(d3wordcloud)


########################################## For Two column Papers ############################################################
# src <- "paper1.pdf"
# trim <- function (x) gsub("^\\s+|\\s+$", "", x)
# 
# QTD_COLUMNS <- 2
# read_text <- function(text) {
#   result <- ''
#   #Get all index of " " from page.
#   lstops <- gregexpr(pattern =" ",text)
#   #Puts the index of the most frequents ' ' in a vector.
#   stops <- as.integer(names(sort(table(unlist(lstops)),decreasing=TRUE)[1:2]))
#   #Slice based in the specified number of colums (this can be improved)
#   for(i in seq(1, QTD_COLUMNS, by=1))
#   {
#     temp_result <- sapply(text, function(x){
#       start <- 1
#       stop <-stops[i] 
#       if(i > 1)            
#         start <- stops[i-1] + 1
#       if(i == QTD_COLUMNS)#last column, read until end.
#         stop <- nchar(x)+1
#       substr(x, start=start, stop=stop)
#     }, USE.NAMES=FALSE)
#     temp_result <- trim(temp_result)
#     result <- append(result, temp_result)
#   }
#   result
# }
# 
# txt <- pdf_text(src)
# 
# result <- ''
# for (i in 1:length(txt)) { 
#   page <- txt[i]
#   t1 <- unlist(strsplit(page, "\n"))      
#   maxSize <- max(nchar(t1))
#   t1 <- paste0(t1,strrep(" ", maxSize-nchar(t1)))
#   result = append(result,read_text(t1))
# }
# 
# #result
# 
# word <- data.frame(text = result)
# word$chapters <- ''
# # removes blank rows
# word2 <- word[!(is.na(word$text) | word$text==""), ]
# #subset until refernces
# word2 <- word2[17:1703,]
# row.names(word2) <- NULL # reset index
# 
# 
# word2[1:112, 'chapters'] = "Intro"
# word2[112:512, 'chapters'] = "Hypothesis"
# word2[189:284, 'chapters'] = "Expertise Diversity, Learning, and Performance in Groups"
# word2[284:386, 'chapters'] = "Collective Team Identification as a Moderator"
# word2[386:412, 'chapters'] = "The Mediating Role of Team Learning Behavior"
# word2[412:505, 'chapters'] = "Nonlinear Effects of Expertise Diversity"
# 
# word2[505:586, 'chapters'] = "Sample and Data Collection"
# word2[586:800, 'chapters'] = "Measures"
# word2[800:840, 'chapters'] = "Analysis"
# 
# word2[840:1032, 'chapters'] = "Results"
# word2[1025:1155, 'chapters'] = "Toward a More Nuanced Theory of Expertise"
# word2[1155:1193, 'chapters'] = "Social Category Diversity and Collective Team Identification"
# word2[1193:1250, 'chapters'] = "Practical Implications"
# word2[1250:nrow(word2), 'chapters'] = "Practical Implications"
# 
# review_words <- word2 %>%
#   unnest_tokens(output = word, input = text) %>%
#   anti_join(stop_words, by = "word") %>%
#   filter(str_detect(word, "[:alpha:]")) %>%
#   distinct()
########################################## For One column Papers ############################################################
# pdf <- read_pdf('1_paper_K_L_clustering.pdf')
# 
# pdf$total <- sapply(pdf$text, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))
# 
# pdf$page<-as.factor(pdf$page_id)
# 

page_number_df <- data.frame(page_id = seq(1, 20, by=1))
# 
# review_words <- pdf %>%
#   unnest_tokens(output = word, input = text) %>%
#   anti_join(stop_words, by = "word") %>%
#   filter(str_detect(word, "[:alpha:]")) %>%
#   distinct()


# chapters_where_mention_word <- review_words %>%
#   count(word, name = "chapters_n") %>%
#   filter(chapters_n >= 9)

# word_correlations <- review_words %>%
#   semi_join(chapters_where_mention_word, by = "word") %>%
#   pairwise_cor(item = word, feature = chapters) %>%
#   filter(correlation >= 0.2)

# graph_from_data_frame(d = word_correlations,
#                       vertices = chapters_where_mention_word %>%
#                         semi_join(word_correlations, by = c("word" = "item1"))) %>%
#   ggraph(layout = "fr") +
#   geom_edge_link(aes(alpha = correlation)) +
#   geom_node_point() +
#   geom_node_text(aes(color = chapters_n, label = name), repel = TRUE)





papers <<- list.files(pattern = "\\.pdf$")

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

read_text <- function(text) {
  result <- ''
  #Get all index of " " from page.
  lstops <- gregexpr(pattern =" ",text)
  #Puts the index of the most frequents ' ' in a vector.
  stops <- as.integer(names(sort(table(unlist(lstops)),decreasing=TRUE)[1:2]))
  #Slice based in the specified number of colums (this can be improved)
  for(i in seq(1, QTD_COLUMNS, by=1))
  {
    temp_result <- sapply(text, function(x){
      start <- 1
      stop <-stops[i] 
      if(i > 1)            
        start <- stops[i-1] + 1
      if(i == QTD_COLUMNS)#last column, read until end.
        stop <- nchar(x)+1
      substr(x, start=start, stop=stop)
    }, USE.NAMES=FALSE)
    temp_result <- trim(temp_result)
    result <- append(result, temp_result)
  }
  result
}


# The list of valid books
# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(paper) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  # if (!(paper %in% papers))
  #   stop("Unknown paper")

  #readLines(sprintf("./%s.txt.gz", book),encoding="UTF-8")
  pdf <- read_pdf(paper)
  text <- pdf$text
  
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but", "-")) 

  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})


getdata <- memoise(function(paper) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  # if (!(paper %in% papers))
  #   stop("Unknown paper")
  
  #readLines(sprintf("./%s.txt.gz", book),encoding="UTF-8")
  pdf <- read_pdf(paper)
  
  pdf$total <- sapply(pdf$text, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))
  
  pdf$page<-as.factor(pdf$page_id)
  pdf
})

getpageid <- memoise(function(paper) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  # if (!(paper %in% papers))
  #   stop("Unknown paper")
  
  #readLines(sprintf("./%s.txt.gz", book),encoding="UTF-8")
  pdf <- read_pdf(paper)
  
  pdf$total <- sapply(pdf$text, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))
  
  pdf$page<-as.factor(pdf$page_id)
  pdf$page_id
})
#sales_data <- read.csv(file = 'MonthlySales.csv')
#data <- ts(sales_data[,2],start = c(2013,1),frequency = 12)

######################################################## 1. HEADER #################################################### 
header <- dashboardHeader(
  #title = 'Text Analysis' , tags$li(class = "dropdown", style = "padding: 10px 12px 0px 0px;", tags$p("Developed by Bakai Baiazbekov"))
  title = shinyDashboardLogo(
    theme = "blue_gradient",
    boldText = 'Publication Analysis',
    mainText = "App",
    badgeText = "v1.1"
  ),titleWidth = 300, tags$li(class = "dropdown", style = "padding: 10px 12px 0px 0px;", tags$p("Developed by Bakai Baiazbekov"))
  )

######################################################## 2. SIDEBAR #################################################### 
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard",tabName = "table",icon = icon("table"))
    #menuItem("Graphs",tabName = "graphs",icon = icon("th")),
    #menuItem("Contact",tabName = "contact", icon = icon("phone"))
  ), collapsed = TRUE
  
)

######################################################## 3. BODY #################################################### 
body <- dashboardBody(
  shinyDashboardThemes(theme = "grey_light"),
  
  tabItems(
    tabItem(tabName = "table",
            sidebarPanel(width = 3,
                         # Input: Select a file ----
                         fileInput("file", "Choose PDF File", multiple = FALSE, accept = c(".pdf")),
                         #selectInput("selection", "Choose a paper:", choices = papers),
                         actionButton("update", "Upload", class="btn-lg btn-success", icon = icon("upload")),
                         hr(),
                         
                         radioButtons("radio", "Type of Paper",
                                      choices = list("Theoretical" = 1, "Empirical" = 2), inline = TRUE),
                         numericInput("speed", "How many words per minute do you read?",
                                      min = 1, max = 250,
                                      value = 75),
                         hr(),
                         
                         # Input: Slider for the number of observations to generate ----
                         selectInput("subset", "Choose a page number:",
                                     choices = unique(page_number_df$page_id),selected =unique(page_number_df$page_id),  multiple = TRUE),

                         sliderInput("freq",
                                     "Minimum Frequency:",
                                     min = 1,  max = 50, value = 15),
                         sliderInput("max",
                                     "Maximum Number of Words:",
                                     min = 1,  max = 300,  value = 100),
                         hr(),
                         # radioButtons("radio_column", "Is paper single columned?",
                         #              choices = list("Yes" = 1, "No" = 2), inline = TRUE),
                         
                         actionButton("summarize", "Summarize it!", class="btn-lg btn-success", icon = icon("rocket")),
                         hr(),
                         sliderInput("n_chapters",
                                     "Minimum Number of Words in every page:",
                                     min = 1,  max = 20,  value = 5),
                         sliderInput("corr",
                                     "Minimum Correlation :",
                                     min = 0.1,  max = 1,  value = 0.5, step = 0.1),
                         hr(),
                         numericInput("page_n", "Which page do you wanna see?",
                                      min = 1, max = 100, step = 1,
                                      value = 1),
                         
            
            ),#sidebarPanel
            
            mainPanel(            
              fluidRow(
                column(12, align="center",               
                       valueBoxOutput("pages_n", width = 3),
                       valueBoxOutput("total_words", width = 3),
                       valueBoxOutput("per_page", width = 3),
                       valueBoxOutput("per_min", width = 3))),
              
              fluidRow(
                column(7,box(title = "Correlation Network",status="primary", plotOutput("network_plot"),width = NULL)),
                column(5,box(title = "Word Cloud",status="primary", plotOutput("plot"), width = NULL))
                
              ),
              fluidRow(
                column(7, box(title = "Summarization Table", DT::dataTableOutput("resultsTable"), width = NULL))
              )),
            fluidRow(
              column(12, box(width = NULL, verbatimTextOutput("placeholder", placeholder = FALSE)))
              
            )
              
              

       
    )#tabItem
  )#tabItems
  
)#dashboardBody

######################################################## 4. UI ######################################################## 

# Create the UI using the header, sidebar, and body
ui <- dashboardPage(header, sidebar, body)

######################################################## 5. SERVER #################################################### 

server <- function(input, output, session) {
  
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing your research paper...")
        print(input$file)
        req(input$file)
        getTermMatrix(input$file$datapath)
      })
    })
  })
  
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
  
  getdata_react <- reactive({
    # Change when the "update" button is pressed...
    input$update
    isolate({
      withProgress({
        setProgress(message = "Processing your research paper...")
        getdata(input$file$datapath)
      })
    })
  })
  
  subset_pages <- reactive({
    
    pages <- c(input$subset)
    review_words <- getdata_react() %>% filter(page %in% pages) %>%
      unnest_tokens(output = word, input = text) %>%
      anti_join(stop_words, by = "word") %>%
      filter(str_detect(word, "[:alpha:]")) %>%
      distinct()
    
    review_words
  })
  
  chapter_n_react <- reactive({
    
    chapters_where_mention_word <- subset_pages() %>%
      count(word, name = "page_id") %>%
      filter(page_id >= input$n_chapters)
    chapters_where_mention_word
  })
  
  correlation_react <- reactive({
    word_correlations <- subset_pages() %>%
      semi_join(chapter_n_react(), by = "word") %>%
      pairwise_cor(item = word, feature = page_id) %>%
      filter(correlation >= input$corr)
    word_correlations
  })
  
  output$network_plot <- renderPlot({
    graph_from_data_frame(d = correlation_react(),
                          vertices = chapter_n_react() %>%
                            semi_join(correlation_react(), by = c("word" = "item1"))) %>%
      ggraph(layout = "fr") +
      geom_edge_link(aes(alpha = correlation)) +
      geom_node_point() +
      geom_node_text(aes(color = page_id, label = name), repel = TRUE)
  })
  
  text_data <- reactive({
    
    # if (input$selection %in% papers) {
    #   src <- input$selection
    #   print(src)
    # }
    # 
    # if (1 %in% input$radio_column) {
    #   QTD_COLUMNS <- 1
    # }
    # else{
    #   QTD_COLUMNS <- 2
    #   
    # }
    
    src <- input$file$datapath
    
    
    txt <- pdf_text(src)
    result <- ''
    for (i in 1:length(txt)) { 
      page <- txt[i]
      t1 <- unlist(strsplit(page, "\n"))      
      maxSize <- max(nchar(t1))
      t1 <- paste0(t1,strrep(" ", maxSize-nchar(t1)))
      result = append(result,read_text(t1))
    }
  result
    
  })
  
  summarization <- eventReactive(input$summarize, {
    showNotification("Parsing text into sentences and tokens...", type = "message")
    Sys.sleep(1)
    showNotification("Calculating pairwise sentence similarities...", type = "message")
    Sys.sleep(2)
    showNotification("Applying LexRank.", type = "message")
    Sys.sleep(3)
    showNotification("Formatting Output.", type = "message")
    #perform lexrank for top 3 sentences
    top_sentences = lexRankr::lexRank(text_data(),
                              #only 1 article; repeat same docid for all of input vector
                              docId = rep(1, length(text_data())),
                              #return 3 sentences to mimick /u/autotldr's output
                              n = 5,rmStopWords = TRUE,removeNum=TRUE,stemWords=TRUE,
                              continuous = TRUE)
    
    #reorder the top 3 sentences to be in order of appearance in article
    order_of_appearance = order(as.integer(gsub("_","",top_sentences$sentenceId)))
    #extract sentences in order of appearance
    ordered_top_sentences = top_sentences[order_of_appearance, "sentence"]
    tmp <- data.frame(text = c(ordered_top_sentences))
    #view(tmp)
    tmp 
  })
  
  output$resultsTable <- DT::renderDataTable({
    # using styleColorBar
    #datatable(summarization()$text,options = list(dom = 'ft'))
    datatable(summarization())
    
  })
  
  ##  Total Number of Pages Box
  output$pages_n <- renderValueBox(
    valueBox(max(getdata_react()$page_id), subtitle = 'Total Number of Pages', icon = icon("book"), width = 2)
  )
  
  ##  Total Number of Words Box
  output$total_words <- renderValueBox(
    valueBox(sum(getdata_react()$total), subtitle = 'Total Number of Words', icon = icon("file-word"), width = 2)
  )
  
  reading_in_h <- reactive({
    if (1 %in% c(input$radio)) {
      round(((sum(getdata_react()$total)/input$speed *2)/60),1)
    }
    else{
      round((sum(getdata_react()$total)/input$speed)/60,1)
    }
  })
  
  output$per_min <- renderValueBox(
    valueBox(reading_in_h(), subtitle = 'Approx. reading time in hours', icon = icon("clock"), width = 2)
  )
  
  output$per_page <- renderValueBox(
    valueBox(getdata_react()$total[input$page_n], subtitle = paste('Total Number of words in page',input$page_n), icon = icon("file-alt"), width = 2)
  )
 # print_data <- reactive({
 #    pages <- c(input$subset)
 #    data <- getdata_react() %>% filter(page %in% pages)
 #    data$text[1]
 #  })
  
  output$placeholder <- renderPrint({
    pages <- c(input$subset)
    cat(getdata_react()$text[input$page_n])
    })
  
  
}#server

######################################################## 6.ShinyApp ###################################################
#options(shiny.host = '0.0.0.0')
#options(shiny.port = 6622)
shinyApp(ui, server)
