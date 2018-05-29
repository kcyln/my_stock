library(shiny)
library(DT)
library(shinythemes)
library(quantmod)
library(TTR)

         
  options(DT.options = list(
        searchHighlight = TRUE,          
        language = list(
                info = '显示第_START_ 至 _END_ 项结果，共 _TOTAL_ 项',
                search = '搜索:',
                paginate = list(previous = '上页', `next` = '下页'),
                lengthMenu = '显示 _MENU_ 项结果')))    
               

shinyUI(fluidPage(
  
  theme=shinythemes::shinytheme("flatly"),
     titlePanel("股票走势分析"),  
   sidebarLayout(position="left",
      sidebarPanel(
        helpText("Select a stock to examine."),
        a("股票代码查询" ,href="http://finance.yahoo.com/lookup"),
         wellPanel(
             
             textInput(inputId = "stock1", label = "股票1"),
             textInput(inputId = "stock2", label = "股票2"),
             textInput(inputId = "stock3", label = "股票3"),
       
             selectInput(inputId = "stock4",
                     label = "股票可选",
                     choices = c("","GOOG",
                                 "MSFT",
                                 "ORCL",
                                 "DIA")),
            # checkboxInput("log_y","log",value = FALSE),
             actionButton("get","GET"))  , 
           
         
         wellPanel(
            p(strong("日期范围 (从现在起倒推)")),
             sliderInput(inputId = "time_num",
                         label = "时间个数",
                         min = 1, max = 24, step = 1, value = 6),

             selectInput(inputId = "time_unit",
                         label = "时间单位",
                         choices = c("日" = "days",
                                     "周" = "weeks",
                                    "月" = "months",
                                     "年" = "years"),
                                      selected = "Months")  
         # dateRangeInput("dates",label="date range",
         #                start = "2013-01-01",
         #                end = as.character(Sys.Date())
         #                )
         ) ),  
      
    mainPanel(
      
      tabsetPanel(
        
            tabPanel(   title="Plot",
                        
                 selectInput(inputId = "chart_type",
                            label = "",
                             choices = c("蜡烛图" = "candlesticks",
                                         "火柴图" = "matchsticks",
                                         "柱形图" = "bars",
                                         "线型图" = "line")),
        
         conditionalPanel(condition = "input.stock1",
                                        br(),
                                       div(plotOutput(outputId = "plot_1"))),    
        conditionalPanel(condition = "input.stock2",
                                                 br(),
                                                div(plotOutput(outputId = "plot_2"))),    
         conditionalPanel(condition = "input.stock3",
                                                br(),
                                               div(plotOutput(outputId = "plot_3"))),    
         conditionalPanel(condition = "input.stock4",
                                             br(),
                                          plotOutput(outputId = "plot_4")),
        radioButtons('xtype', '图片格式', c('png', 'jpeg', 'bmp'), selected='png', inline=T),
        downloadLink('file', '保存图片')
        ),
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("Table", DT::dataTableOutput('table'),
         fileInput('f', '上传文件', multi=T, accept='text/plain, image/*'),
        textOutput('tx', container=pre),
        downloadButton("downloaddata","download")
                 )
        
        
      )
  )  ) ) )
