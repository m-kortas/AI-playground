library(data.table)
library(shinydashboard)
library(plyr)
library(scales)
library(wordcloud)
library(syuzhet)
library(tidyverse)
library(XML)
library(RCurl)
library(tidyr)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(rtweet)
library(e1071)
library(httpuv)
library(plotly)
library(dplyr)
library(shinydashboardPlus)
library(shinycssloaders)

library(ggplot2)
library(topicmodels)
library(tidytext)




ui <- dashboardPagePlus(
  skin = "red",
  dashboardHeaderPlus(title = "Positive or negative?"),
  dashboardSidebar(
    box(
    title = "What people think?",width = 12, background = "maroon",
    "Sometimes it is good to have a bigger picture and to not rely on information from only one source. 
    What if we could in a moment get to know what people think about certain topic around the world?"
  ),
  box(
    title = "Anger or Fear?", width = 12, background = "light-blue",
    "If they see some topic as a positive or a negative issue? 
    What is the topic associated with? 
    With which emotions people react?"
  ),
  box(
    title = "About the app", width = 12, background = "green",
    "The app connects to Twitter's API and using text mining, natural language processing (NLP), Machine Learning and visualisation analyses recent Tweets (in English) about the topic you choose.
    The analysis doesn't include retweets. 
    Technologies used: R, HTML, CSS. 
    Latent Dirichlet Allocation (an unsupervised learning algorithm) was used to categorize main topics."
  )),
  dashboardBody(
    tags$head(tags$style(HTML('
                              .content-wrapper, .right-side  {
                              background-color: #ffffff;
                              }
                              .skin-red .main-sidebar {
                              background-color: 	#000000;
                              color: #ffffff;
                              }
                              .skin-red .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: 	#000000;
                              }
                              .skin-red .main-sidebar .sidebar .sidebar-menu a{
                              background-color: 	#000000;
                              color: #000000;
                              }
                              .skin-red .main-header .logo {
                              background-color: #000000;
                              }
                              
                              .skin-red .main-header .logo:hover {
                              background-color: #000000;
                              }
                              
                              .skin-red .main-header .navbar {
                              background-color: #000000;
                              }
                              '))),

    fluidRow(),
    fluidRow( img(src='https://i.ibb.co/6DyL7N4/Emotion-People-Cover-Desperate-Cry-Desperately-314481.jpg', align = "right") , boxPlus(
      title = "What would you like to learn about?", 
      closable = FALSE, 
      width = 6,
      status = "warning", 
      solidHeader = FALSE, 
      collapsible = TRUE,
      enable_dropdown = FALSE,
      dropdown_icon = "wrench",
      dropdown_menu = dropdownItemList(
      ),
      p(textInput(inputId = "twitter_input", label = "I am curious what people think about...", value = "#Warsaw"),
        sliderInput("obs", "Number of Twitts to analyse:",
                    min = 0, max = 1000, value = 200
        ))
    )),
    fluidRow( boxPlus(
      title = "If the app doesn't work properly...", 
      closable = TRUE, 
      width = 12,
      status = "warning", 
      solidHeader = FALSE, 
      collapsible = TRUE,
      enable_dropdown = FALSE,
      dropdown_icon = "wrench",
      dropdown_menu = dropdownItemList(
      ),
      p("If you encoutered error while running the app, it unfortunately means that standard Twitter's API rate limits are exceeded. Please wait a bit (max 15 minutes) and try again. Read more: ", 
        tags$a(href="https://developer.twitter.com/en/docs/basics/rate-limits.html", "developer.twitter.com"))
    )),
    
    fluidRow(box(
      title = "Most Popular Twitts", width = 6, background = "maroon",
      withSpinner(verbatimTextOutput("twitt"),  type = getOption("spinner.type", default = 6),
                                                           color = getOption("spinner.color", default = "#ffffff"))
    ), box(
      title = "Twitts' Popularity", width = 6, background = "green",
      withSpinner(plotlyOutput("fav"),  type = getOption("spinner.type", default = 6),
                  color = getOption("spinner.color", default = "#ffffff"))
    )),
    fluidRow(box(
      title = "Languages Of User Accounts", width = 6, background = "blue",
      withSpinner(plotlyOutput("language"),  type = getOption("spinner.type", default = 6),
                  color = getOption("spinner.color", default = "#ffffff"))), 
             box(
      title = "Wordcloud - Most Common Words", width = 6, background = "blue",
      withSpinner(plotOutput("wordcloud"),  type = getOption("spinner.type", default = 6),
                  color = getOption("spinner.color", default = "#ffffff"))
    )),
    fluidRow(),
    fluidRow(box(
      title = "Top 20 Most Common Words", width = 6, background = "yellow",
      withSpinner(plotlyOutput("words"),  type = getOption("spinner.type", default = 6),
                  color = getOption("spinner.color", default = "#ffffff"))
    ), box(
      title = "The Most Common Word's Associations", width = 6, background = "red",
      withSpinner(plotOutput("assocs"),  type = getOption("spinner.type", default = 6),
                  color = getOption("spinner.color", default = "#ffffff"))
    )),
    fluidRow(box(
      title = "What are the topics?", width = 12, background = "blue",
      withSpinner(plotlyOutput("topics"),  type = getOption("spinner.type", default = 6),
                  color = getOption("spinner.color", default = "#ffffff"))
    )),
    fluidRow(box(
      title = "What is the sentiment?", width = 6, background = "green",
      withSpinner(plotlyOutput("sentiment"),  type = getOption("spinner.type", default = 6),
                  color = getOption("spinner.color", default = "#ffffff"))
    ), box(
      title = "What are the emotions?", width = 6, background = "blue",
      withSpinner(plotlyOutput("emotion"),  type = getOption("spinner.type", default = 6),
                  color = getOption("spinner.color", default = "#ffffff"))
    )),
  
    fluidRow(widgetUserBox(
      title = "Magdalena Kortas",
      subtitle = "The Author",
      width = 12,
      type = 2,
      src = "https://i.ibb.co/zbqcbkX/0.jpg",
      color = "maroon",
      "Data Science & Data storytelling enthusiast. Women in Machine Learning & Data Science (WiML&DS) team member. 
      Write me on",  tags$a(href="mailto:magdalenekortas@gmail.com", "magdalenekortas@gmail.com"), "or find me on",
      tags$a(href="https://www.linkedin.com/in/mkortas/", "Linkedin."), " The app was created for The 2019 WIA Data Viz competition on Women in Analytics conference. "
    ))
        )
    )

server <- function(input, output, session) {
  
  data_loading <- function() {   
    
    
    token <- create_token(
      app = "xxx",
      consumer_key = "xxx",
      consumer_secret = "xxx",
      access_token = "xxx",
      access_secret = "xxx")
    
    #origop <- options("httr_oauth_cache")
    #options(httr_oauth_cache = TRUE)
    
    found_tweets <<- search_tweets(q = input$twitter_input, n = input$obs,  lang = "en", type = "mixed", include_rts = FALSE)   
    top <- arrange(found_tweets, desc(favorite_count))
    top1 <<- head(top, n = 3)
    account_lang <<- found_tweets %>% group_by(account_lang) %>% count() %>% arrange(desc(n)) %>% head(10)
    dane <- found_tweets$text
    docs <- Corpus(VectorSource(dane))
    docs <- tm_map(docs, tolower) 
    docs <- tm_map(docs, removeNumbers) 
    docs <- tm_map(docs, removeWords, stopwords("english")) 
    docs <- tm_map(docs, removePunctuation) 
    docs <<- tm_map(docs, stripWhitespace) 
    dtm <<- TermDocumentMatrix(docs)     
    m   <- as.matrix(dtm)                   
    v   <- sort(rowSums(m), decreasing=TRUE)   
    d   <<- data.frame(word=names(v), freq=v) 
    d <<- d[-1,]
    
  }
  

  sentiment_analysis <- function() {
    df_sentiment<-get_nrc_sentiment(as.String(d$word)) 
    df_sentiment_transposed <- t(df_sentiment)
    df_sentiment_final <- data.frame(sentiment=row.names(df_sentiment_transposed),sent_value=df_sentiment_transposed, row.names=NULL) 
    df_emotions <<- df_sentiment_final[1:8,]    
    df_sentiments <<- df_sentiment_final[9:10,] 
    df_sentiments %>% mutate(percent = df_sentiments$sent_value/sum(df_sentiments$sent_value)) ->> df_sentiments
  }
  
  topics_data <- function() {
    text_dtm <<- DocumentTermMatrix(docs)
    text_lda1 <<- LDA(text_dtm, k = 2, method = "VEM", control = NULL)
    text_topics <<- tidy(text_lda1, matrix = "beta")
    
    text_top_terms1 <<- text_topics %>%
      group_by(topic) %>%
      top_n(10, beta) %>%
      ungroup() %>%
      arrange(topic, -beta)
  }
  
  output$twitt <- renderPrint({    
    data_loading()
    top1$text 
  })
  
  output$wordcloud <- renderPlot({
    data_loading()
    wordcloud(words = d$word, freq = d$freq, min.freq = 1, 
              max.words = 30, random.order = TRUE, rot.per = 0.1, colors = brewer.pal(8,"Dark2")) 
  })
  
  output$fav <- renderPlotly({ 
    data_loading()  
    ggplotly(ggplot(found_tweets, aes(x=favorite_count, y=retweet_count)) +
               xlab("Favourite Count") +
               ylab("Retweet Count") +
               geom_point() +
               geom_smooth(method = "lm")) 
  })
  
  output$assocs <- renderPlot({ 
    data_loading()
    term1 <- as.character(d[1,1])
    assocs <- findAssocs(dtm, terms = term1, corlimit = 0.2)  
    df <- ldply(assocs[[1]], data.frame)
    df$.id -> df$word
    df$X..i..*100 -> df$perc
    if (is.null(df$word)) "No correlation!" else wordcloud(words = df$word, freq = df$perc, min.freq = 1, 
                                                           max.words = 30, random.order = TRUE, rot.per = 0.1, colors = brewer.pal(8,"Dark2"))
    
  })
  
  output$words <- renderPlotly({ 
    data_loading()
    ggplotly(ggplot(data = head(d,20), mapping = aes(x = reorder(word, freq), y = freq, fill = freq)) +
               geom_bar(stat = "identity") +
               xlab("Word") +
               ylab("Word frequency") +
               coord_flip()) 
  })
  
  
  output$language <- renderPlotly({ 
    data_loading()
    ggplotly(ggplot(data = account_lang, mapping = aes(x = reorder(account_lang, n), y = n, fill = n)) +
               geom_bar(stat = "identity") +
               xlab("User Account Language") +
               ylab("Number of Twitts") +
               coord_flip())  
  })
  
  output$sentiment <- renderPlotly({ 
    data_loading() 
    sentiment_analysis()
    ggplotly(ggplot(data= df_emotions, mapping= aes(x=sentiment, y = sent_value, color=sentiment, fill = sentiment))+
               geom_bar(stat="identity") +
               xlab("sentiment")+
               ylab("words count") +
               theme(axis.text.x=element_text(angle=90, hjust=1))) 
  })
  
  output$emotion <- renderPlotly({ 
    data_loading()
    sentiment_analysis()
    ggplotly(ggplot(data= df_sentiments, mapping= aes(x=sentiment, y = percent, color=sentiment, fill = sentiment))+
               geom_bar(stat="identity") +
               xlab("emotion")+
               ylab("words count") +
               theme(axis.text.x=element_text(angle=90, hjust=1))) 
  })
  
  output$topics <- renderPlotly({
    data_loading()
    topics_data()
    ggplotly(text_top_terms1 %>%
               mutate(term = reorder(term, beta)) %>%
               ggplot(aes(term, beta, fill = factor(topic))) +
               geom_col(show.legend = FALSE) +
               facet_wrap(~ topic, scales = "free", shrink = TRUE) +
               coord_flip())
  })
}





shinyApp(ui = ui , server = server)

