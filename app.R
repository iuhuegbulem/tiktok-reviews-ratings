library(dplyr)
library(plotly)
library(sentimentr)
library(tidyverse)
library(tidytext)
library(lubridate)
library(DataExplorer)
library(stringr)
library(ggplot2)
library(tidyr)
library(scales)
library(DataExplorer)
library(ggthemes)
library(extrafont)
library(remotes)
library(reshape2)
library(wordcloud)
library(shiny)
library(RColorBrewer)
library(fmsb)
library(radarchart)
library(thematic)
library(bslib)
library(fontawesome)
library(shinydashboard)
library(shinyWidgets)


load("clean_data.RData")
load("translated_data.RData")
load("nrc.RData")
load("emotions.RData")
load("sentiment.RData")
load("bigram_tf_idf.RData")

app_theme <- bs_theme(fg = "#5c374c",
                      bg = "#ffffff",
                      primary = "#5f506b", 
                      secondary = "#86bbbd",
                      base_font = font_google("Raleway"),
                      heading_font = font_google("Raleway"))

ui <- fluidPage(
  titlePanel("App reviews and ratings"),
  
  theme = app_theme,
  
  tags$style(".small-box.bg-yellow { background-color: #ccebc5 !important; color: #000000 !important; }"),
  
  useShinydashboard(),
  
  fluidRow(
    column(2,
           div(HTML("<em>The plots and table all react to the inputs below. Select one or more inputs.</em>")),
           checkboxGroupInput("country", h5("Country"),
                              choices = list('Australia' = 'Australia',
                                             'Canada' = 'Canada',
                                             'France' = 'France',
                                             'Mexico' = 'Mexico',
                                             'New Zealand' = 'New Zealand',
                                             'Nigeria' = 'Nigeria',
                                             'Norway' = 'Norway',
                                             'South Africa' = 'South Africa',
                                             'UK' = 'UK',
                                             'USA' = 'USA'), selected = c("Australia", "Canada", "France", "Mexico", "New Zealand")),
           
           sliderTextInput(inputId = "month",
                           label = "Month range slider:",
                           choices = list('January' = 'January',
                                          'February' = 'February',
                                          'March' = 'March',
                                          'April' = 'April',
                                          'May' = 'May',
                                          'June' = 'June'),
                           selected = month.name[c(1, 6)]),
           
           class = "p-3 border rounded"),
    column(10,
           tabsetPanel(
             type = "pills",
             
             tabPanel("Introduction", icon = icon("person-chalkboard"), 
                      p("This Shiny application has been developed to analyze TikTok reviews and ratings on the iOS app store between January and June 2021. 
                        Data from 10 countries (USA, UK, France, Canada, France, Australia, Nigeria, Norway, New Zealand and South Africa) was gathered to understand the following:"),
                      tags$li("App ratings"),
                      tags$li("User sentiment (positive or negative)"),
                      tags$li("Average rating per version"),
                      tags$li("Average review character length"),
                      tags$li("App review sentiment score"),
                      tags$li("Text sentiment analysis"),
                      tags$li("Correlation between rating and sentiment score"),
                      p(""),
                      p("I developed this application for my Masters in Digital Marketing and Data Science thesis as a tool to show the intersection between Marketing and Artificial Intelligence and how AI drives innovation in Digital Marketing.
                        A total of 33,425 reviews were analyzed in this application."),
                      p("Feel free to connect with me via any of the channels below and I hope you enjoy using this aplication. :)"),
                      tags$li(class="dropdown",tags$a(href="https://www.linkedin.com/in/ijeoma-uhuegbulem/" ,icon("linkedin"), "LinkedIn", target="_blank")),
                      tags$li(class="dropdown",tags$a(href="https://github.com/iuhuegbulem" ,icon("github"), "Github", target="_blank")),
                      class = "p-3 border rounded"),
             tabPanel("Rating Analysis", icon = icon("star"),
                      fluidRow(
                        box(width = 12, title = tags$b("Summary Statistics"), solidHeader = TRUE,
                             fluidRow(
                               column(width = 3, valueBox("24,928","Reviews analyzed", color = "yellow",
                                                          width = NULL, icon = icon("chart-line"))),
                               column(width = 3, valueBox(50,"Unique languages translated", color = "yellow",
                                                          width = NULL, icon = icon("globe"))),
                               column(width = 3, valueBox(30,"App versions analyzed", color = "yellow",
                                                          width = NULL, icon = icon("code-fork"))),
                               column(width = 3, valueBox("92,435", "Characters translated", color = "yellow",
                                                          width = NULL, icon = icon("language"))))
                        )  
                      ),
                      tags$b("App rating distribution per market"),
                      plotlyOutput('pt'),
                      tags$b("Average rating per app version"),
                      plotlyOutput('pt2'),
                      class = "p-3 border rounded"
             ),
             tabPanel("Review Analysis", icon = icon("quote-right"),
                      tags$b("Review character length per market"),
                      plotlyOutput('pt3'),
                      tags$br(),
                      tags$b("Top bigram count"),
                      plotlyOutput('pt4'),
                      tags$br(),
                      tags$b("Top 30 occurring words"),
                      plotlyOutput('pt5'),
                      tags$br(),
                      tags$b("Wordcloud showing most frequent words"),
                      plotOutput('wc', height = "500px"),
                      class = "p-3 border rounded"
             ),
             tabPanel("Sentiment Analysis", icon = icon("masks-theater"),
                      tags$b("App reviews sentiment score per market"),
                      plotOutput('pt6'),
                      tags$b("Wordcloud showing positive and negative words using bing lexicon"),
                      plotOutput('wc2'),
                      tags$br(),
                      tags$b("Sentiment mapping using NRC lexicon"),
                      chartJSRadarOutput('pt7'),
                      tags$br(),
                      tags$b("Correlation between rating and sentiment score"),
                      plotOutput('pt8'),
                      class = "p-3 border rounded"
             ),
             tabPanel("Data", icon = icon("table"),
                      h2("The data"),
                      p("This table contains the reviews extracted in their original language. You can search for keywords, filter and sort through the tweets. Have fun!"),
                      dataTableOutput("table"),
                      class = "p-3 border rounded"),
             selected = "Introduction"
           ))
    ))

server <- function(input, output) {
  thematic::thematic_shiny()
  
  # App rating distribution per market
  output$pt <- renderPlotly({
    df %>%
      filter(country %in% input$country) %>%
      filter(month.name[month(date)] %in% (input$month)) %>%
      ggplot(aes(x=rating, fill=country)) + 
      geom_bar(position = "dodge", color = "gray") +
      theme(text = element_text(size = 10), 
            axis.line = element_line(colour = "black"),
            panel.background = element_rect(fill = "white")) + 
      scale_fill_brewer(palette = "Set3") +
      labs(x="Ratings 1-5", y="No of Ratings")
  })
 
  
  # App rating per app version
  output$pt2 <- renderPlotly({
    df %>%
      filter(country %in% input$country) %>%
      filter(month.name[month(date)] %in% (input$month)) %>%
      group_by(app_version, country) %>%
      summarise(avg_rating = mean(rating)) %>%
      ggplot(aes(x = reorder(app_version, -avg_rating), y = avg_rating, fill = country)) +
      geom_col(color = "gray") +
      scale_fill_brewer(palette = "Set3") + 
      theme(axis.text.x=element_text(angle=45, hjust=1), 
            text = element_text(size = 10), 
            axis.line = element_line(colour = "black"),
            panel.background = element_rect(fill = "white")) +
      labs(x = "Version", y = "Average rating")
  })

  
  # Review character length per market
  output$pt3 <- renderPlotly({
    df2 %>%
      filter(country %in% input$country) %>%
      filter(month.name[month(date)] %in% (input$month)) %>%
      ggplot(aes(x=ReviewLength)) + 
      geom_density(aes(y = ..count..), color="#1F3161", fill = "#fccde5", alpha=0.6) +
      geom_vline(aes(xintercept = AvgReviewLength), linetype = "dashed", size = 0.5) +
      geom_text(aes(x=AvgReviewLength, y=2, label=AvgReviewLength), check_overlap = TRUE, size=5, angle=0, vjust=1, hjust=-0.5)+
      ylim(0,5) +
      xlim(5,600) +
      facet_wrap(~country, scales = 'free') +
      scale_fill_brewer(palette = "Set3") +
      theme(text = element_text(size = 8), 
            axis.line = element_line(colour = "black"),
            panel.background = element_rect(fill = "white"))+
      labs(subtitle = "The average length per review for each market", x="Review Length", y="Rating")
  })
  
  # Bigram count
  output$pt4 <- renderPlotly({
    bigram_tf_idf %>%
      filter(country %in% input$country) %>%
      head(30) %>%
      ggplot(aes(x=bigram_count, y=bigram, size = bigram_count, fill = country)) +
      geom_point(alpha=0.7, show.legend = FALSE) + 
      scale_fill_brewer(name="Country", palette="Set3") +
      theme(text = element_text(size=10), axis.line = element_line(colour = "gray"),
            panel.background = element_rect(fill = "white")) + 
      labs(x = "Bigram count",
           y = "Bigram", size = "bigram count")
  })
  
  # Top 30 occurring words
  output$pt5 <- renderPlotly({
    emotions %>%
      filter(country %in% input$country) %>%
      filter(month.name[month(date)] %in% (input$month)) %>%
      count(word, sort = TRUE) %>%
      mutate(word = reorder(word, n)) %>%
      slice(1:30) %>%
      ggplot(aes(word, n)) +
      theme(text = element_text(size = 10), 
            axis.line = element_line(colour = "black"),
            panel.background = element_rect(fill = "white")) +
      labs(x="", y="Count") +
      geom_col(stat="identity", colour="#635e5d", fill = "#8dd3c7") +
      xlab(NULL) +
      coord_flip()
  })
  
  # App reviews sentiment score per market
  output$pt6 <- renderPlot({
    sent %>%
      filter(country %in% input$country) %>%
      filter(month.name[month(date)] %in% (input$month)) %>%
      ggplot(aes(x = date, y = sentiment, fill=country)) + 
      geom_smooth(colour="black", size=0.4) +
      scale_fill_brewer(palette = "Set3") +
      theme(text = element_text(size = 9), 
            axis.line = element_line(colour = "black"),
            panel.background = element_rect(fill = "white")) +
      labs(subtitle = "Date period differs in some countries due to data availability", 
           x="Date", 
           y="Reviews Sentiment Scores")
  })
  
  # Wordcloud showing most frequent words
  wordcloud_rep <- repeatable(wordcloud)
  output$wc <- renderPlot({
    wc_data <- emotions %>%
      filter(country %in% input$country) %>%
      filter(month.name[month(date)] %in% (input$month)) %>%
      count(word)
    wordcloud(words = wc_data$word, 
              freq = wc_data$n, min.freq = 20,
              max.words = 300, random.order = FALSE,
              rot.per=0.35, scale=c(4.0,0.8),
              colors=brewer.pal(8, "Dark2"))
  }) 
  
  # Wordcloud showing positive and negative words 
  output$wc2 <- renderPlot({
    emotions %>%
      inner_join(get_sentiments("bing")) %>%
      filter(month.name[month(date)] %in% (input$month)) %>%
      filter(country %in% input$country) %>%
      count(word, sentiment, sort = TRUE) %>%
      acast(word ~ sentiment, value.var = "n", fill = 0) %>%
      comparison.cloud(colors = c("#D9383A", "#68E193"), 
                       scale=c(3.1,0.6),
                       height = "500px",
                       use.r.layout=FALSE,
                       max.words = 200)
  })
  
  # Sentiment mapping
  output$pt7 <- renderChartJSRadar({
    RadarData <- emotions %>%
      filter(month.name[month(date)] %in% (input$month)) %>%
      group_by(country, sentiment) %>%
      count(country, sentiment) %>%
      select(country, sentiment, sentiment_country_count = n)%>%
      spread(country, sentiment_country_count) 
    
    chartJSRadar(RadarData, 
                 main = "(check boxes below to view specific countries)",
                 scaleLineWidth=5,
                 height = "600px",
                 showToolTipLabel = TRUE, responsive = logical)
  })
  
  # Correlation between sentiment score and rating
  output$pt8 <- renderPlot({
    sent %>%
      filter(country %in% input$country) %>%
      filter(month.name[month(date)] %in% (input$month)) %>%
      ggplot(aes(x = sentiment, y = rating, fill=country)) + 
      geom_smooth(colour="black", size=0.4) +
      theme(text = element_text(size = 10), 
            axis.line = element_line(colour = "black"),
            panel.background = element_rect(fill = "white")) +
      labs(x="Sentiment", 
           y="Rating")
  })
  
  table <- reactive({
    df %>%
      filter(country %in% input$country) %>%
      filter(month.name[month(date)] %in% (input$month)) %>%
      subset(select = -author )
  })
  output$table <- renderDataTable(table(), options = list(pageLength = 10))
  
}

shinyApp(ui = ui, server = server)
