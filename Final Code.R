setwd("C:/Users/prach/OneDrive/Desktop/Aegis/r project")
netflix<- read.csv("ds.csv")
netflix$user.rating.score [is.na(netflix$user.rating.score)] <- 88
rating = unique(netflix$rating)
class(netflix$rating)
summary(netflix)
str(netflix)
r=arrange(netflix,desc(netflix$user.rating.score))
r2 =arrange(netflix,netflix$user.rating.score)
# tail(r)
# tail(filter(r,year == as.numeric(2006)),5)
# head(filter(r,year == as.numeric(2006)),5)
c = sort(unique(netflix$year))
c
# library(plotrix)
library(ggplot2)
# colnames(netflix)
library(shiny)
runApp(list(
  ui = basicPage(
    headerPanel("Netflix 101"),
    sidebarPanel(
      # div(style = "display:inline-block" , 
      #     selectInput("Rate","Please select the year",choices =c,selected="PG-13")
      div(style = "display:inline-block" , 
          selectInput("Year","Please select the year",choices =c,selected="2006") 
          
      ),
      
      div(style = "display:inline-block" , 
          selectInput("Keywords","Please select the keywords",choices =c("sexual"="sexual","violence"="violence","Parental guidance"="Parental guidance","mature"="mature","action"="action","adult content"="adult content","animated"="animated","fantasy"="fantasy","General Audiences"="General Audiences","Suitable for children"="Suitable for children","scary"="scary"),selected="violence") 
          
      ),
      
      div(style = "display:inline-block" , 
          selectInput("Rate","Please select Rating Type",choices =rating,selected="PG-13")
      )
      
      
    ),
    mainPanel(
      tabsetPanel(
        
        # fluidRow(column(2),
        #          column(10, plotOutput("distPlot2")))
        
        tabPanel("Dataset",HTML("<br><br>"),textOutput("text3"),textOutput("text4"),textOutput("text5"),textOutput("text6"),HTML("<br><br>"),dataTableOutput('dataset')),
        tabPanel("Distribution",plotOutput('overall'), plotOutput('pietest')),
        tabPanel("Overall Rating Plot",plotOutput('rating')),
        tabPanel(" Top 5(Year)", plotOutput('bar_rate_year_top'),HTML("<br><br><br>"),textOutput("text1")),
        tabPanel("Bottom 5(Year)", plotOutput('bar_rate_year_bottom'),HTML("<br><br><br>"),textOutput("text2")),
        tabPanel("Top 5(Rating)",  plotOutput('bar_rate_top'),HTML("<br><br><br>"),textOutput("text7")),
        tabPanel("Bottom 5(Rating)",plotOutput('bar_rate'),HTML("<br><br><br>"),textOutput("text8")),
        tabPanel("Keyword Search",dataTableOutput('mytable'))
      )
    )
    
    # mainPanel(   plotOutput('bar_rate_year_top'),plotOutput('bar_rate_year_bottom'),plotOutput('pietest')),
    # textOutput("text1")
  )
  
  ,
  server=function(input,output){
    #output$text1 <- reactive(paste("You are seeing the values for the year",input$Year))
    df2 <- reactive({head(filter(r,year == as.numeric(input$Year)),5)})
    # df2 <- head(df2(),5)
    output$bar_rate_year_top <- renderPlot({ggplot(df2() , aes(df2()$m,df2()$user.rating.score)) + geom_bar(stat = "identity",fill = 'red',alpha = 0.4)+ theme_classic(base_size = 15)+xlab("Titles") + ylab("User Score") + ggtitle("Top 5 Shows per Year")})
    
    df3 <- reactive({head(filter(r2,year == as.numeric(input$Year)),5)})
    # df2 <- head(df2(),5)
    output$bar_rate_year_bottom <- renderPlot({ggplot(df3() , aes(df3()$m,df3()$user.rating.score)) + geom_bar(stat = "identity",fill = 'red',alpha = 0.4)+ theme_classic(base_size = 15)+xlab("Titles") + ylab("User Score") + ggtitle("Bottom 5 Shows per Year")})
    
    df4 = reactive({filter(netflix,year == as.numeric(input$Year))})
    # output$pietest <- renderPlot({pie(summary(df4()$rating))})
    output$pietest <- renderPlot({pie(table(df4()$rating),main="Distribution of Ratings per Year",radius = 1, cex = 1.3)})
    # pie(table(netflix$rating))
    
    netflix_ratings_per_year <- netflix %>% group_by(rating, year) %>% summarise(count = n())
    netflix_ratings_per_year <- netflix_ratings_per_year %>% group_by(year) %>% filter(count == max(count))
    
    output$overall <- renderPlot({ggplot(netflix_ratings_per_year, aes(x= year, y = count, fill = rating)) + geom_bar(stat = "identity") +
        xlab("Release Year") + ylab("Number of shows in the category") + ggtitle("Most common ratings of shows year on year basis")})
    
    
    netflix_user_rating_score<- netflix %>% group_by(user.rating.score,m) %>% summarise(count = n())
    netflix_user_rating_score
    # ggplot(netflix_user_rating_score, aes(x= user.rating.score, y = count)) + geom_bar(stat = "identity",fill="blue") +
    #   xlab("User Rating Score") + ylab("Title") + ggtitle(" Most frequent range of User Rating Scores")
    
    
    output$rating <- renderPlot({ggplot(netflix_user_rating_score, aes(x= user.rating.score, y = count)) + geom_bar(stat = "identity",fill="blue") +
        xlab("User Rating Score") + ylab("Title") + ggtitle(" Most frequent range of User Rating Scores")})
    
    df5 <- reactive({netflix[grep(as.character(input$Keywords),netflix$ratingLevel),]})
    # df2 <- head(df2(),5)
    #output$bar_rate <- renderPlot({ggplot(df2() , aes(df2()$m,df2()$user.rating.score)) + geom_bar(stat = "identity")})
    output$mytable = renderDataTable({df5()})
    
    
    df6 <- reactive({tail(filter(r,rating == as.character(input$Rate)),5)})
    # df2 <- head(df2(),5)
    output$bar_rate <- renderPlot({ggplot(df6() , aes(df6()$m,df6()$user.rating.score)) + geom_bar(stat = "identity",fill = 'blue',alpha = 0.2)+ theme_classic(base_size = 15)+xlab("Titles") + ylab("User Score") + ggtitle("Bottom 5 Shows per Rating Type")})
    
    df7 <- reactive({head(filter(r,rating == as.character(input$Rate)),5)})
    output$bar_rate_top <- renderPlot({ggplot(df7() , aes(df7()$m,df7()$user.rating.score)) + geom_bar(stat = "identity",fill = 'blue',alpha = 0.2)+ theme_classic(base_size = 15)+xlab("Titles") + ylab("User Score") + ggtitle("Top 5 Shows per Rating Type")})
    output$text1 =renderText(paste( "This graph helps determine the Top Rated shows / movies of the year.Netflix has to take strategic decisions almost every month as to which titles to keep as they want to keep only good and well-liked content in such cases these graphs can be very useful"))
    
    output$text2 =renderText(paste( "This graph helps determine the Lowly Rated shows / movies of the year.Netflix has to take strategic decisions almost every month as to which titles to keep as they want to keep only good and well-liked content in such cases these graphs can be very useful , if a title is consistenly not liked by the consumers the Executives will take a decision to drop these titles and instead add something new which has been demanded by the consumers "))
    
    output$text3 = renderText(paste("Ratings for Little Kids: G, TV-Y, TV-G."))

    output$text4 = renderText(paste("Ratings for Older Kids: PG, TV-Y7, TV-Y7-FV, TV-PG."))

    output$text5 = renderText(paste("Ratings for Teens: PG-13, TV-14."))

    output$text6 = renderText(paste(" Ratings for Adults: R, NC-17, NR, UR, TV-MA."))
    
    output$text7 =renderText(paste( "This graph helps determine the Top Rated shows / movies by the Rating Type.Netflix has to take strategic decisions almost every month as to which titles to keep as they want to keep only good and well-liked content in such cases these graphs can be very useful"))
    
    output$text8 =renderText(paste( "This graph helps determine the Lowly Rated shows / movies by the Rating.Netflix has to take strategic decisions almost every month as to which titles to keep as they want to keep only good and well-liked content in such cases these graphs can be very useful , if a title is consistenly not liked by the consumers the Executives will take a decision to drop these titles and instead add something new which has been demanded by the consumers "))
    
    
    output$dataset = renderDataTable({netflix})
  }
    ))

