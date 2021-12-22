library(data.table)
library(shinydashboard)
library(plyr)
library(scales)
library(wordcloud)
library(tidyverse)
library(RCurl)
library(tidyr)
library(maps)
library(RColorBrewer)
library(plotly)
library(dplyr)
library(shinydashboardPlus)
library(ggplot2)
library(tidytext)
library(shinyjs)
library(topicmodels)
library(twitteR)
library(rtweet)
library(SnowballC)
library(tm)
library(e1071)
library(syuzhet)
library(XML)
library(httpuv)
library(memisc)


ui <- dashboardPagePlus(
  skin = "red",
  dashboardHeaderPlus(disable = TRUE),
  dashboardSidebar(disable = TRUE, collapsed = TRUE, sidebarMenu()),
  dashboardBody(
    useShinyjs(),
    tags$head(tags$style(
      HTML(
        '
                              .content-wrapper, .right-side  {
                              background-color:  #ffffff;
                              }
                              .skin-red .main-sidebar {
                              background-color:  #42a75a;
                              color:  #ffffff;
                              }
                              .skin-red .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: 	 #42a75a;
                              }
                              .skin-red .main-sidebar .sidebar .sidebar-menu a{
                              background-color: 	 #42a75a;
                              color:  #000000;
                              }
                              .skin-red .main-header .logo {
                              background-color:  #ffffff;
                              }
                              .skin-red .main-header .logo:hover {
                              background-color:  #ffffff;
                              }
                              .skin-red .main-header .navbar {
                              background-color:  #ffffff;
                              }
                              '
      )
    )),
    
    fluidRow(
      img(
        src = '
                  https://i.ibb.co/r58xQRm/48763178032-e0abe4903f-o.jpg',
        width = "100%",
        height = "100%"
      )
    ),
    fluidRow(
      boxPlus(
        title = "The history",
        width = 12,
        status = "warning",
        collapsible = TRUE,
        dropdown_icon = "wrench",
        dropdown_menu = dropdownItemList(),
        p(
          "In August 2018, a Swedish schoolgirl, Greta Thunberg decided to not attend school after heat waves and wildfires in Sweden.",
          tags$br(),
          "Thunberg protested
by sitting outside the Swedish parliament every day during school hours with a sign",
          tags$b(" school strike for the climate"),
          ".",
          tags$br(),
          tags$br(),
          "She demanded that the Swedish government reduce carbon emissions per the Paris Agreement. She coined the slogan",
          tags$b("#FridaysForFuture and #ClimateStrike"),
          "which gained worldwide attention.",
          tags$br(),
          tags$br(),
          "In September 2019 in New York, the UNICEF hosted a press conference where Thunberg joined 15 other children and together the group announced they had made an official complaint against five nations that are not on track to meet the emission reduction targets they committed to in their Paris Agreement pledges."
        )
      )
    ),
    
    #   Google analytics and Video
    fluidRow(
      box(
        title = "The story",
        width = 6,
        background = "orange",
        plotlyOutput("google")
      )
      ,
      box(
        title = "Video",
        width = 6,
        background = "green",
        uiOutput("video")
      )
    ),
    
    #     Tweets: what and when
    fluidRow(
      widgetUserBox(
        title = "'The Greta effect'",
        subtitle = "",
        width = 12,
        type = 2,
        src = "https://i.ibb.co/gSSv4wk/Zrzut-ekranu-2019-11-16-o-00-21-36.png",
        color = "green",
        "Some critics say that Greta’s activism actually does not change anything, does not have any positive influence on the problem rather than blaming governments and short term popularity after United Nations meeting in September.",
        tags$br(),
        tags$br(),
        "However, Greta has inspired a number of her school-aged peers to take part in student strikes in what has been called",
        tags$b("'The Greta effect'"),
        "In response to her outspoken stance, various politicians have also acknowledged the need to focus on climate change.
In 2019, there were at least 2 coordinated multi-city protests involving",
        tags$b("over 1 million students each.")
      )
    ),
    
    fluidRow(
      widgetUserBox(
        title = "Activism all around the world",
        subtitle = "Tracking 'climate strike' activism for four days",
        width = 12,
        type = 2,
        src = "https://i.ibb.co/0KJtjvr/Zrzut-ekranu-2019-11-16-o-00-23-43.png",
        color = "green",
        "Greta still stays active, travelling across the United States and Europe and taking part in strikes and meetings.
  So there are people around the world that actively take part in protests.",
        tags$br(),
        tags$br(),
        "I have been tracking",
        tags$b(" #climatestrike tweets for four days"),
        "in October to visualize the impact and changes
that are taking place currently all around the world."
      )
    ),
    fluidRow(
      box(
        title = "Four days of #climatestrike tweets",
        width = 12,
        background = "green",
        plotlyOutput("time")
      )
    ),
    fluidRow(
      box(
        title = "Most popular tweets",
        width = 12,
        background = "orange",
        verbatimTextOutput("twitt")
      )
    )
    ,
    fluidRow(
      box(
        title = "#climatestrike tweets all over the world",
        width = 12,
        background = "orange",
        plotOutput("map2")
      )
    ),
    fluidRow(
      box(
        title = "#climatestrike tweets in the United States",
        width = 12,
        background = "green",
        plotOutput("map")
      )
    ),
    
    #  What
    fluidRow(
      widgetUserBox(
        title = "What people say about climate strikes?",
        subtitle = "",
        width = 12,
        type = 2,
        src = "https://i.ibb.co/gJrBSJP/Zrzut-ekranu-2019-11-16-o-00-24-47.png",
        color = "green",
        "What people think? What do they discuss? How they are associated to the problem of climate change?"
      )
    ),
    
    fluidRow(
      box(
        title = "Tweets' most common words",
        width = 12,
        background = "orange",
        plotlyOutput("words")
      )
    ),
    fluidRow(
      box(
        title = "Tweets' most common words",
        width = 6,
        background = "green",
        plotOutput("wordcloud")
      ),
      box(
        title = "Climate change associations",
        width = 6,
        background = "orange",
        plotOutput("assocs")
      )
    ),
    
    #  Approach
    fluidRow(
      widgetUserBox(
        title = "How people feel about climate strikes?",
        subtitle = "",
        width = 12,
        type = 2,
        src = "https://i.ibb.co/XLCWYWM/Zrzut-ekranu-2019-11-16-o-00-25-07.png",
        color = "green",
        "It is often difficult to measure the temperature of a strike or a protest.",
        tags$br(),
        "
   People tweeting about climate strikes can be either climate change activists or - climate change deniers.
   Also tweets itself can be focused both on the problem (and be negative) or - on activism and hope - (and be positive)."
      )
    ),
    
    fluidRow(
      box(
        title = "#climatestrike tweets sentiment",
        width = 6,
        background = "green",
        plotlyOutput("sentiment")
      ),
      box(
        title = "#climatestrike tweets emotions",
        width = 6,
        background = "orange",
        plotlyOutput("emotion")
      )
    ),
    #about author
    
    fluidRow(
      widgetUserBox(
        title = "Magdalena Kortas",
        subtitle = "The Author",
        width = 12,
        type = 2,
        src = "https://media.licdn.com/dms/image/C5603AQFcqLIANkKOTw/profile-displayphoto-shrink_200_200/0?e=1579132800&v=beta&t=vDripK7fkpnneFq1IrzR1juZNi1-Na-9vv4cA-k8v7E",
        color = "green",
        "Data Science & Data storytelling enthusiast.Write me on",
        tags$a(href = "mailto:magdalenekortas@gmail.com", "magdalenekortas@gmail.com"),
        "or find me on",
        tags$a(href = "https://www.linkedin.com/in/mkortas/", "Linkedin.")
      )
    )
  )
)

server <- function(input, output, session) {
  addClass(selector = "body", class = "sidebar-collapse")
  
  data_loading <- function() {
    found_tweets <<- readRDS("tweets.Rds")
    df_sentiments <<- readRDS("df_sentiment.Rds")
    df_emotions <<- readRDS("df_emotions.Rds")
    d <<- readRDS("d.Rds")
    top1 <<- readRDS("top1.Rds")
    climate <<- read.csv(file = "climate.csv",
                         header = TRUE,
                         sep = ",")
    greta <- read.csv(file = "greta.csv",
                      header = TRUE,
                      sep = ",")
    strike <- read.csv(file = "strike.csv",
                       header = TRUE,
                       sep = ",")
    climate$GretaThunberg <<- greta$Greta
    climate$ClimateChange <<- climate$Climate
    climate$ClimateStrike <<- strike$ClimateStrike
    climate$Week <<- as.Date(climate$Week)
    
    
  }
  
  data_loading()
  
  output$twitt <- renderPrint({
    top1$text
  })
  
  output$wordcloud <- renderPlot({
    wordcloud(
      words = d$word,
      freq = d$freq,
      min.freq = 1,
      max.words = 20,
      random.order = TRUE,
      rot.per = 0.1,
      colors = brewer.pal(9, "Greens")
    )
  })
  
  output$assocs <- renderPlot({
    df <- readRDS("df.Rds")
    wordcloud(
      words = df$word,
      freq = df$per,
      min.freq = 1,
      max.words = 7,
      random.order = TRUE,
      rot.per = 0.1,
      colors = brewer.pal(9, "Greens")
    )
  })
  
  output$words <- renderPlotly({
    ggplotly(
      ggplot(
        data = head(d, 18),
        mapping = aes(
          x = reorder(word, freq),
          y = freq,
          fill = freq
        )
      ) +
        geom_bar(
          stat = "identity",
          fill = c(
            "#F7FCF5",
            "#F7FCF5",
            "#E5F5E0",
            "#E5F5E0",
            "#C7E9C0",
            "#C7E9C0",
            "#A1D99B",
            "#A1D99B",
            "#74C476",
            "#74C476",
            "#41AB5D",
            "#41AB5D",
            "#238B45" ,
            "#238B45" ,
            "#006D2C" ,
            "#006D2C" ,
            "#00441B",
            "#00441B"
          )
        ) +
        xlab("Word") +
        ylab("Word frequency") +
        coord_flip()
    )
  })
  
  output$sentiment <- renderPlotly({
    ggplotly(
      ggplot(
        data = df_emotions,
        mapping = aes(
          x = sentiment,
          y = sent_value,
          color = sentiment,
          fill = sentiment
        )
      ) +
        geom_bar(stat = "identity", fill = brewer.pal(8, "Greens")) +
        xlab("sentiment") +
        ylab("words count") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    )
  })
  
  output$emotion <- renderPlotly({
    ggplotly(
      ggplot(
        data = df_sentiments,
        mapping = aes(
          x = sentiment,
          y = percent,
          color = sentiment,
          fill = sentiment
        )
      ) +
        geom_bar(stat = "identity", fill = c("#A1D99B", "#006D2C")) +
        xlab("emotion") +
        ylab("words count") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    )
  })
  
  output$map <- renderPlot({
    rt <- lat_lng(found_tweets)
    par(mar = c(0, 0, 0, 0))
    map("world", lwd = .25)
    with(rt, points(
      lng,
      lat,
      pch = 20,
      cex = .75,
      col = brewer.pal(9, "Greens")
    ))
  })
  
  output$map2 <- renderPlot({
    rt <- lat_lng(found_tweets)
    par(mar = c(0, 0, 0, 0))
    map("state", lwd = .25)
    with(rt, points(
      lng,
      lat,
      pch = 20,
      cex = .75,
      col = brewer.pal(9, "Greens")
    ))
  })
  
  output$time <- renderPlotly({
    ggplotly(found_tweets %>%
               ts_plot("1 hour", col = c("#00441B")) +
               labs(x = 'Date', y = 'Count'))
  })
  
  output$google <- renderPlotly({
    ggplotly(
      ggplot(climate, aes(x = Week)) +
        geom_line(aes(y = ClimateChange), color = c("#41AB5D")) +
        geom_line(aes(y = GretaThunberg), color = c("#006D2C")) +
        geom_line(aes(y = ClimateStrike), color = c("#A1D99B")) +
        scale_x_date() + xlab("Interest over time - Google Trends")  + ylab('')
    )
  })
  
  output$video <- renderUI({
    HTML(
      paste0(
        '<iframe width="560" height="315" src="https://www.youtube.com/embed/-Q0xUXo2zEY" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'
      )
    )
    
  })
  
  
  
}
shinyApp(ui = ui , server = server)