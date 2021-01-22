#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(DT)
library(data.table)
library(shinyalert)
library(shinythemes)
library(readr)
library(lubridate)
library(readxl)
library(ggplot2)
library(gridExtra)
library(tm)
library(wordcloud)
library(memoise)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(memoise)
library(stringr)
library(tm)
library(plotly)

# Data --------------------------------------------------------------------
data = read.csv("All_Categories_Updated.csv")
category = data %>% select(Categories) %>% unique()
category_2 = rbind(category, "All")

# data %>% filter(Shop_Name == "Shopee Mart") %>% select(URL) %>% head(1)

# UI ----------------------------------------------------------------------
side_width = 4
ui <- fluidPage(
    theme= shinytheme("united"),
    navbarPage(
        "Shopee Recommender",
        
        # Tab Page 1 --------------------------------------------------------------
        tabPanel("Overview",
                 h1("Overview"), #Title1
                 br(),
                 h3("This app helps to recommend shops based on category, sub category, price and shipping."),
                 br(),
                 h3("Instruction:"),
                 h3("i) Customize the filter box on the left based your liking."),
                 h3("ii) Top 3 best shop based on most unit sold will be display in the bar chart, please click on the bar chart to promt for more insight on the store."),
                 h3("iii) Shop listing will be show in the table, you may sort based on your liking."),
                 br(),
                 h3("Happy Shopping!")
        ),
        # Tab Page 2 --------------------------------------------------------------
        tabPanel("Recommender",
                 sidebarLayout(
                     sidebarPanel(
                         width = 3,
                         h3("Search Filter"),
                         br(),
                         selectInput(
                             "cat_selected",
                             label = "By Category",
                             choices = category_2,
                             selected = "All"
                         ),
                         uiOutput("cat_sub"),
                         uiOutput("cat_keyword"),
                         uiOutput("cat_price"),
                         uiOutput("cat_from"),
                     ),
                     mainPanel(
                         # width = 12 - side_width,
                         h3("Top 3 Shops by Highest Unit Sold", align="center"),
                         plotlyOutput("myBarPlot"),
                         h3("Shop Listing", align="center"),
                         DT::dataTableOutput("mytable")
                     )
                 )
        )
    )
)

# SERVER ------------------------------------------------------------------
server<-function(input,output,session){
    #helper function
    getshopreview <- function(shop){
        df <- data
        # View(df)
        filteredshop <- filter(df, df$Shop_Name == shop)
        x <- filteredshop %>%
            unnest_tokens(word, 'Buyer_Review') %>%
            # anti_join(stop_words2) %>%
            filter(
                !str_detect(word, pattern = "[[:digit:]]"), # removes any words with numeric digits
                !str_detect(word, pattern = "[[:punct:]]"), # removes any remaining punctuations
                !str_detect(word, pattern = "(.)\\1{2,}"),  # removes any words with 3 or more repeated letters
                !str_detect(word, pattern = "\\b(.)\\b")    # removes any remaining single letter words
            ) %>%
            count(word) %>%
            filter(n >= 0) %>%
            arrange(n)
    }
    getpieplot <- function(shop){
        # data<-read.csv("All_Categories_Updated.csv")
        dfdata = data.frame(data)
        #select keyword
        sel_keyword <- dfdata[dfdata$Shop_Name == shop,]
        ByShops <- sel_keyword %>% summarise(ShopName= unique(Shop_Name),
                                             GPQ = sum(Good.product.quality),GVM= sum(Good.value.for.money),
                                             ESS= sum(Excellent.service.by.seller),FD = sum(Fast.delivery), 
                                             OTH= sum(OtherTags))
    }
    output$myBarPlot = renderPlotly({
        
        if (input$cat_selected == "All")
            data_bar <- data
        else
            data_bar = mytable()
        
        df <- data_bar %>%
            group_by(Shop_Name) %>%
            summarise(sum_unit_sold = sum(Unit_Sold)) %>%
            arrange(desc(sum_unit_sold)) %>%
            top_n(3)
        
        df$Shop_Name <- factor(df$Shop_Name, levels = unique(df$Shop_Name)[order(df$sum_unit_sold, decreasing = FALSE)])
        plot_ly(df, y = ~Shop_Name, x = ~sum_unit_sold, type = 'bar', orientation='h', color = 'tomato')
    })
    
    observeEvent(event_data("plotly_click"), {
        barData = event_data("plotly_click")
        selected_shop = barData$y
        # url = data %>% filter(Shop_Name == selected_shop)
        myurl = mytable() %>% filter(Shop_Name == "Shopee Mart") %>% select(URL) %>% head(1)
        url <- a(myurl, href=myurl)
        
        # Plot wordcloud and pie chart
        showModal(modalDialog(title = "Shop Details",
                              h4("WordCloud of Buyer Review", align="center"),
                              tagList(url),
                              renderPlot({
                                  shop = selected_shop
                                  if (is.null(shop)) {
                                      print("error")
                                  } else {
                                      print(shop)
                                      dftext <- getshopreview(shop)
                                      wordcloud(words = dftext$word, freq = dftext$n, min.freq = 3, random.order=FALSE, rot.per=0.20, colors=brewer.pal(8, "Dark2"))
                                  }
                              }),
                              renderPlotly({
                                  df <- getpieplot(selected_shop)
                                  Shop1<- tibble(tags=c("Good Product Quality" , "Good Value for Money", "Excellent Service by Seller", "Fast Delivery", "Other"),
                                                 count = c(as.numeric(df[c("GPQ","GVM","ESS","FD","OTH")])))
                                  
                                  plot_ly(Shop1, labels = ~tags, values = ~count, textinfo = ~tags) %>%
                                      add_pie(hole = 0.5) %>%
                                      layout(title = "Breakdown of Buyer Tags",  showlegend = T,   
                                             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                                             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE))
                              })
        ))
        
    })
    output$cat_sub <- renderUI({
        if(input$cat_selected == 'All') {
            possible_sub = data %>% select(Sub.Categories) %>% unique()
            possible_sub = rbind(possible_sub, "All")
            selectInput(
                "sub_cat_selected",
                label = "By Sub Category",
                choices = possible_sub,
                selected = "All"
            )
        }
        else {
            possible_sub = data %>% filter(Categories==input$cat_selected) %>% select(Sub.Categories) %>% unique()
            possible_sub = rbind(possible_sub, "All")
            selectInput(
                "sub_cat_selected",
                label = "By Sub Category",
                choices = possible_sub,
                selected = "All"
            )
        }
    })
    output$cat_keyword <- renderUI({
        if(input$cat_selected == 'All' & input$sub_cat_selected == "All") {
            possible_keyword = data %>% select(keyword) %>% unique()
            possible_keyword = rbind(possible_keyword, "All")
            selectInput(
                "keyword_selected",
                label = "Choose Keyword",
                choices = possible_keyword,
                selected = "All"
            )
        }
        else if(input$cat_selected != 'All' & input$sub_cat_selected == "All"){
            possible_keyword = data %>% filter(Categories==input$cat_selected) %>% select(keyword) %>% unique()
            possible_keyword = rbind(possible_keyword, "All")
            selectInput(
                "keyword_selected",
                label = "Choose Keyword",
                choices = possible_keyword,
                selected = "All"
            )
        }
        else if(input$cat_selected == 'All' & input$sub_cat_selected != "All"){
            possible_keyword = data %>% filter(Sub.Categories==input$sub_cat_selected) %>% select(keyword) %>% unique()
            possible_keyword = rbind(possible_keyword, "All")
            selectInput(
                "keyword_selected",
                label = "Choose Keyword",
                choices = possible_keyword,
                selected = "All"
            )
        }
        else {
            possible_keyword = data %>% filter(Categories==input$cat_selected) %>% filter(Sub.Categories==input$sub_cat_selected) %>% select(keyword) %>% unique()
            possible_keyword = rbind(possible_keyword, "All")
            selectInput(
                "keyword_selected",
                label = "Choose Keyword",
                choices = possible_keyword,
                selected = "All"
            )
        }
    })
    output$cat_price <- renderUI({
        
        if(input$cat_selected == "All"){
            min_price = data %>% select("Price") %>% min()
            max_price = data %>% select("Price") %>% max()
        }
        
        else {
            min_price = data %>% filter(Categories==input$cat_selected) %>% select("Price") %>% min()
            max_price = data %>% filter(Categories==input$cat_selected) %>% select("Price") %>% max() 
        }
        
        sliderInput("price Range",
                    "Product Price:",
                    min = min_price,
                    max = max_price,
                    format = "##,##0.##",
                    value = c(min_price, max_price))
    })
    output$cat_from <- renderUI({
        if(input$cat_selected == "All") {
            possible_from = data %>% select("Shipping_From") %>% unique()
            possible_from = rbind(possible_from, "All")
            selectInput(
                "from_selected",
                label = "Shipped From",
                choices = possible_from,
                selected = "All"
            )  
        }
        else {
            possible_from = data %>% filter(Categories==input$cat_selected) %>% select("Shipping_From") %>% unique()
            selectInput(
                "from_selected",
                label = "Shipped From",
                choices = possible_from,
                selected = "Selangor"
            )  
        }
    })
    mytable <- reactive({
        # print(data)
        if (input$cat_selected == 'All' & input$sub_cat_selected == "All" & input$keyword_selected == "All") {
            mytable <- data
        }
        else {
            mytable <- data
            if (input$cat_selected != 'All')
                mytable <- mytable %>% filter(Categories %in% input$cat_selected)
            if (input$sub_cat_selected != 'All')
                mytable <- mytable %>% filter(Sub.Categories %in% input$sub_cat_selected)
            if (input$keyword_selected != 'All')
                mytable <- mytable %>% filter(keyword %in% input$keyword_selected)
            if (!is.null(input$price))
                mytable <- mytable %>% filter(Price >= input$price[1] && Price <= input$price[2])
            if (!is.null(input$from_selected))
                mytable <- mytable %>% filter(Shipping_From %in% input$from_selected)
        }
    })
    output$mytable <- renderDataTable({
        # mytable()
        
        r2 <-mytable() %>% mutate(URL = paste0("<a href='", URL,"' target='_blank'>", URL,"</a>")) %>% select(
            Shop_Name,
            Prod_Name,
            Shipping_From,
            Buyer_Rating,
            Unit_Sold,
            Price,
            URL) %>% distinct()
        datatable(r2, list(mode = "single", target = "cell"), escape = FALSE)
    }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
