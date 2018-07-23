# Load packages
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(DT)
library(shinydashboard)

#setwd('/Users/howardchang/Desktop/NYC Data Science/Shiny/shinyApps/shinyProject')
#getwd()

#AH Data
ah <- readr::read_csv("Master A-H Data.csv")
head(ah,6)

#fx data
fx <- readr::read_csv("FX Table.csv")
fx.df <- data.frame(fx)
fxCNYHKD <- select(fx.df , Date, CNYHKD.Price)
fxCNYHKD <- fxCNYHKD %>% mutate(Date = as.Date(Date, "%m/%d/%y"))
head(fxCNYHKD,6)

#index data
index <- readr::read_csv(file="Indexes.csv")
indexHKCN <- select(index, Date, Close, Ticker, Index)
indexHKCN <- indexHKCN %>% mutate(Date = as.Date(Date, "%m/%d/%y"))
head(indexHKCN,6)


#marketcap data
MarketCap <- readr::read_csv(file="MarketCap.csv")
MarketCap.df <-data.frame(MarketCap)
head(MarketCap.df)
MarketCap.df <- MarketCap.df %>% mutate(MarketCapUSD = round((MarketCapUSD/1000000000),2))
MarketCap.df

#AH data
ah.df <- data.frame(ah)
ah.df <- ah.df %>% mutate(Date = as.Date(Date, "%m/%d/%Y"))
head(ah.df)

#Parse out A share data
rawA <- filter(ah.df, AH.Class == 'A')
Ashare <- select(rawA, Date, Close, Ticker, Company.Name, AH.Class)
head(Ashare)

#Parse out H share data
rawH <- filter(ah.df, AH.Class == 'H')
Hshare <- select(rawH, Date, Close, Ticker, Company.Name, AH.Class)
head(Hshare)

#inner join A Share and H Share data
NewAH <- inner_join(Ashare, Hshare, by = c("Date","Company.Name"))
head(NewAH)

#inner join FX data
AHFX <- inner_join(NewAH, fxCNYHKD, by = "Date") 
head(AHFX)

#column rename
AHFX <- AHFX %>% mutate(., CNHKDPrice = Close.x*CNYHKD.Price)
head(AHFX)
colnames(AHFX)
AHFX <- rename(AHFX, CNY.Price = 'Close.x')
AHFX <- rename(AHFX, HKD.Price = 'Close.y')
AHFX <- rename(AHFX, CN.Ticker = 'Ticker.x')
AHFX <- rename(AHFX, HK.Ticker = 'Ticker.y')
AHFX <- rename(AHFX, FX.Rate = 'CNYHKD.Price')

colnames(AHFX)
head(AHFX)


AHFX <- select(AHFX, Date, Company.Name, HK.Ticker, HKD.Price, AH.Class.y, CN.Ticker, CNY.Price, AH.Class.x, FX.Rate, CNHKDPrice)
head(AHFX)

#Calculate Premium/Discount Ratio
AHFX <- AHFX %>% mutate(., PremDiscount = ifelse(CNHKDPrice>HKD.Price, (((CNHKDPrice-HKD.Price)/(CNHKDPrice))),
                                                 ifelse(CNHKDPrice<HKD.Price, (-1*((HKD.Price-CNHKDPrice)/(HKD.Price))), 0)))


#Entire AHFX historical data set
AHFX <- select(AHFX, Date, Company.Name, HK.Ticker, HKD.Price, AH.Class.y, CN.Ticker, CNY.Price, AH.Class.x, PremDiscount)
AHFX

#test <- filter(AHFX, Company.Name == 'SINOPEC CORP')
#test

# format the premdiscount column
AHFX$PremDiscount = round(AHFX$PremDiscount,3)
AHFX$PremDiscount

AHFX


#test <- filter(AHFX, Date == '2018-07-16')
#test

#latest date data
AHFXdate <- filter(AHFX, Date == max(Date))
AHFXdate                  

AHPie <- AHFXdate %>% mutate(PremDiscCat = ifelse(AHFXdate$PremDiscount > .20, 'A-Share Premium >20%', 
                                                  ifelse(AHFXdate$PremDiscount <=.20 & AHFXdate$PremDiscount >=0, 'A-Share Premium = 0% to 20%',
                                                         ifelse(AHFXdate$PremDiscount <0 & AHFXdate$PremDiscount >=-.10, 'A-Share Discount = 0% to -10%', 'A-Share Discount > -10%'))))
AHPie


AHMarketCap <- inner_join(AHFXdate, MarketCap.df, by = "Company.Name")
AHMarketCap


# Define UI
ui <- 
  navbarPage("A-H Pair",
  tabPanel("AH Graph",
  fluidPage(theme = shinytheme("lumen"),
               # titlePanel("A-H Graph"),
                sidebarLayout(
                  sidebarPanel(
                    
                    # Select type of trend to plot
                    selectInput(inputId = "type", label = strong("Company Name"),
                                choices = unique(AHFX$Company.Name),
                                selected = "SH ELECTRIC"),
                    
                    # Select date range to be plotted
                    dateRangeInput("date", strong("Date range"), start = "2015-01-01", end = "2017-07-16",
                                   min = "2015-01-01", max = "2017-07-16"),
                    
                    # Select whether to overlay smooth trend line
                    checkboxInput(inputId = "smoother", label = strong("Overlay smooth trend line"), value = FALSE),
                    
                    # Display only if the smoother is checked
                    conditionalPanel(condition = "input.smoother == true",
                                     sliderInput(inputId = "f", label = "Smoother span:",
                                                 min = 0.01, max = 1, value = 0.67, step = 0.01,
                                                 animate = animationOptions(interval = 100)),
                                     HTML("Higher values give more smoothness.")
                    )
                  ),
                  
                  # Output: Description, lineplot, and reference
                  mainPanel(
                    plotOutput(outputId = "lineplot", height = "300px"),
                    textOutput(outputId = "desc")
                  )
                )
)),

tabPanel("Data",
         fluidRow(box(DT::dataTableOutput("table"), width = '12')
)),

tabPanel("Pie Chart",
         fluidRow(box(plotlyOutput("pie")
         ))),

tabPanel("Box Plot",
         fluidRow(box(plotlyOutput("box"), width = '10'
         ))),

tabPanel("Chart by MarketCap($bil)",
         fluidRow(box(plotlyOutput("MCap"), width = '80'
         )))

)

# Define server function
server <- function(input, output) {
  
  # Subset data
  selected_trends <- reactive({
    req(input$date)
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
    AHFX %>%
      filter(
        Company.Name == input$type,
        Date > as.Date(input$date[1]) & Date < as.Date(input$date[2]
        ))
  })
  
  output$table <- DT::renderDataTable({
    datatable(AHFXdate, rownames=FALSE) %>% 
      formatStyle(input$selected, background="skyblue", fontWeight='bold')
  })
  
  output$pie <- renderPlotly({
    plot_ly(AHPie, labels = ~PremDiscCat, type = 'pie') %>%
      layout(title = 'AH Premium Distribution Chart',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             legend = list(x=20,y=-0.5))
  })
  
  output$MCap <- renderPlotly({
    b <- ggplot(AHMarketCap, aes(MarketCapUSD, PremDiscount, colour=Company.Name)) + geom_point()
    ggplotly(b)
    
  })
    output$box <- renderPlotly({
      
      c <- plot_ly(x = AHFX$Company.Name, y = AHFX$PremDiscount,type = "box")
      ggplotly(c) %>%
      layout(
        title='AH Premium/Discount Box Chart',     
        xaxis = list(title = "Company Name"),
        yaxis = list(title = "Premium/Discount")
        )
  })
  
  # Create scatterplot object the plotOutput function is expecting
  output$lineplot <- renderPlot({
    color = "#434343"
    par(mar = c(4, 4, 1, 1))
    selected_trends()$Date
    plot(x = selected_trends()$Date, y = as.numeric(selected_trends()$PremDiscount), type = "l",
         xlab = "Date", ylab = "Premium/Discount", col = color, fg = color, col.lab = color, col.axis = color)
    # Display only if smoother is checked
    if(input$smoother){
      smooth_curve <- lowess(x = as.numeric(selected_trends()$Date), y = as.numeric(selected_trends()$PremDiscount), f = input$f)
      lines(smooth_curve, col = "#E6553A", lwd = 3)
    }
  })
  

}

# Create Shiny object
shinyApp(ui = ui, server = server)

