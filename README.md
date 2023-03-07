# Finance Sentiment Analysis 


# Contributor 👩‍💻
Hildana Workija Teklegiorgis

# Introduction
I will be performing a sentiment analysis on consumer complaints on Financial products.
![wallstreet-be6e21ad26e546dd8b015d7be5d71528](https://user-images.githubusercontent.com/108307724/223010662-1f2920d6-42ce-4ac4-ba11-d8e58efec3b8.jpg)

# Libraries 📚

The libraries used and their purposes in this analysis are as follows:

1.tidyr -  to tidy up the dataset<br/> 
2.ggplot2 - for making plots i.e: barplot<br/>
3.sentimentr - to extract a sentinment value from text data<br/> 
4.readxl - to read excel data<br/> 
5.tidytext - to convert text to and from tidy formats<br/> 
6.textdata - to downoald, parse and store text datasets<br/> 
7. dplyr - to manipulate data <br/> 
8.stringr - to manipulate string datasets<br/> 
9.reshape2 - to transform our data to another structure <br/> 
10.worldcloud - to analyze and visualize keywords as a word cloud<br/> 
11.shiny - to build interactive web applications <br/> 
12.DT - to render data ojects in R as HTML JavaScript library<br/> 


# Data Dictionary 📖
1. Company: The company that the complaint was about.
2: Product: The fiancial product taht was being complained about.
4. Issue: The issue that the complain was on.
5. State: The state the complaint was from.
5. Submitted.via: The medium that the complaint was handled through
6. Company.response.to.consumer: The way in which the issue was handled
7. Timely.response: Whether or not the response was given in time
8. Complaint.ID: The Identification number for the complaint.
9. Total Sentiment: aggregate sentiment value


# Data Format 💾
1. Converting .csv to .rds
 The first step in this anlysis was converting the .csv(comma-separated values) data into .rds(R Data Serialization) for better efficiency.
```
df0<-read.csv("Consumer_Complaints.csv")
saveRDS(df0,"Consumer_Complaints.rds")
df<-readRDS("Consumer_Complaints.rds")
```
# Data Cleaning 🧹

1. Selecting the columns I needed 
The data set had a number of columns that provided information on the date, location and medium the complaint was reeived. My hypothesis was that there would be a trend in complaints  financial product. Therefore, I selected to use columns that had information the compnay being complained about, The issue being complained about and the location.
```
variables <- df%>%
  select(Company, Product,Issue,State)

```


2. Working with 50,000 rows
Since I was working with a large data set, I initially worked with the 50,000 rows for faster analysis and debugging.
```
variables[1:50000, ]
```



4. getting rid of stop words
I used
```
anti_join(stop_words) %>%
```
to get rid of words that don't have sentiment which condensed the data that I had to maniuplate.


# Data Summary 📄

| |Product| Sentiment Value|
|-|----------|-----|
|1|Consumer Loan|-6473|
|2|Credit card|-12238|
|3|Credit reporting|-97556|
|4|Debt collection|-110743|
|5|Money transfers|-4655|
|6|Other financial service|-856|
|7|Payday loan|-244|
|8|Prepaid card|-2200|
|9|Student loan|-1699|
|10|Virtual currency|-17|

# Data Analysis 📊
Before making any charts, I analyzed the data by the issue and groouped it by product. This allowed me get an assigned sentiment value for each financial product using the issue being complained about.
```
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

```
```
df_2<-variables%>%
  group_by(Product)%>%
  mutate(linenumber=row_number())%>%
  ungroup()%>%
  unnest_tokens(word, Issue)
```


* Chart 1: Sentiment Analysis by Product
```
cleaned <- df_2 %>%
  inner_join(get_sentiments("bing")) %>%
  count(Product, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

df_3 <- cleaned %>% group_by(Product) %>% 
  summarise(Total_sentiment=sum(sentiment),
            .groups = 'drop') %>%
  as.data.frame()
```
Then I used ggplot to create the chart.
```
ggplot(data=df_3,aes(Total_sentiment,Product))+
  geom_bar(stat="identity",aes(color=Product))+
  labs( title = "Products by sentiments",x="Sentiment value", y="Product")+
  scale_color_manual(values=c('Red','Orange','Green','Yellow', 'Violet','Black','Brown','Pink','Purple','Grey'))

```
![SA](https://user-images.githubusercontent.com/108307724/223020910-37f9df94-e3d0-4bea-bcf7-5d4e31af26c8.png)</br>
    - Debt collection has the highest volume. This tells shows most of the complaints are about Debt collection.</br>
    - Debt collection had the lowest sentiment Value. This analysis shows that the most negative sentiment was about Debt colection in particular.</br>

* Chart 2: Word Cloud
```
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



```
![newwc](https://user-images.githubusercontent.com/108307724/223025700-e65746f4-4832-465f-80b0-7fc4f66abad6.png)
  - The word cloud analysis shows thatwords associated with stong positive sentiment included words such as protection, promised conveneience and savings.</br>
  - The opposite side of the same chart showed association of words such as debt, incorrect, fraud, unable, scam, and false with negative sentiment.
* Chart 3: Shiny App
```

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


```


# Conclusion 
There is a trend within Finicial products in consumer complaints recieved on Financial products. these trends are associated with positive/negative sentiments based on the issues being complained about as well as the volume of these issues.

