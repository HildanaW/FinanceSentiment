library(tidyr)
library(ggplot2)
library(sentimentr)
library(readxl)
library(tidytext)
library(textdata)
library(dplyr)
library(stringr)
library(reshape2)
library(wordcloud)
library(shiny)
library(DT)
library(rsconnect)
library(rsconnect)
rm(list=ls())


#setting working directory

setwd("~/Desktop/DATA-332/Project1")

#converting .csv file to .rds
df0<-read.csv("Consumer_Complaints.csv")
saveRDS(df0,"Consumer_Complaints.rds")
df<-readRDS("Consumer_Complaints.rds")

#selecting the first 500000 rows

variables <- df%>%
  select(Company, Product,Issue,)
variables[1:50000, ]

#storing text as character

variables2 <- as.character((variables$Issue))


#sentiment analysis
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")


df_2<-variables%>%
  group_by(Product)%>%
  mutate(linenumber=row_number())%>%
  ungroup()%>%
  unnest_tokens(word, Issue)

df_2<-df_2%>%
  anti_join(stop_words)

nrc_joy <- get_sentiments("nrc") %>% 
 filter(sentiment == "joy")

df_2 %>%
  filter(Product == "Mortgage")%>% 
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)


#Assigning sentiment values by Financial Product
cleaned <- df_2 %>%
  inner_join(get_sentiments("bing")) %>%
  count(Product, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

df_3 <- cleaned %>% group_by(Product) %>% 
  summarise(Total_sentiment=sum(sentiment),
            .groups = 'drop') %>%
  as.data.frame()
df_4<-df_2%>%
  inner_join(get_sentiments("bing")) %>%
  count(Product, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)


#creating chart

ggplot(data=df_3,aes(Total_sentiment,Product))+
  geom_bar(stat="identity",aes(color=Product))+
  labs( title = "Sentiment Analysis by Finacial Product",x="Sentiment value", y="Finacial Product")+
  scale_color_manual(values=c('Red','Orange','Green','Yellow', 'Violet','Black','Brown','Pink','Purple','Grey'))

#Creating the wordcloud

get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", "negative")) %>% 
  count(sentiment)

get_sentiments("bing") %>% 
  count(sentiment)

bing_word_counts <- df_2 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word~sentiment, value.var="n",fill=0)%>%
  comparison.cloud(colors=c("gray20","gray80"), maxwords = 100)



#Creating interactive chart using shiny app 


column_names<-colnames(cleaned) #for input selections


ui<-fluidPage( 
  
  titlePanel(title = "Consumer Complaints on Financial Products"),
  
  fluidRow(
    column(2,
           selectInput('X', 'Choose X',column_names,column_names[4]),
           selectInput('Y', 'Choose Y',column_names,column_names[2])
           
    ),
    column(4,plotOutput('plot_01')),
    column(6,DT::dataTableOutput("table_01", width = "100%"))
  )
  
  
)

server<-function(input,output){
  
  output$plot_01 <- renderPlot({
    ggplot(cleaned, aes_string(x=input$X, y=input$Y, colour=input$Splitby))+ geom_smooth()
    
    
    
    
    
  })
  
  output$table_01<-DT::renderDataTable(cleaned[,c(input$X,input$Y,input$Splitby)],options = list(pageLength = 4))
}

shinyApp(ui=ui, server=server)

