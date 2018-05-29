library(shiny)
library(DT)
library(quantmod)
library(TTR)

source("global.R")

 shinyServer(function(input, output) {  
   
   
     # Create an environment for storing data
    symbol_env <- new.env()
     
     # Make a chart for a symbol, with the settings from the inputs
     make_chart <- function(symbol) {
         symbol_data <- require_symbol(symbol, symbol_env)
         #TA_STR <- paste0()
         chartSeries(symbol_data,
                            name   = symbol,
                            type   = input$chart_type,
                            subset = paste("last",input$time_num,input$time_unit),
                            #log.scale = input$log_y,
                           theme      = "white")
         
         avgprice <- function(x)apply(HLC(x), 1,mean)
         addavgprice <- newTA(FUN=avgprice,col=1,legend="Avgprice")
         addT_radio <- newTA(FUN=t_radio,col="red",legend="tgtRat")
         addavgprice(on=1)
         
     }
     
     datasetInput <- reactive({
       input$stock1
       switch(input$stock4,
              "GOOG"=GOOG,"MSFT"=MSFT,"ORCL"=ORCL,"DIA"=DIA   )
   })    
    
      output$tx <- renderPrint({ str(input$f)  })
     output$table <- renderDataTable({head(as.data.frame(datasetInput()), n = input$time_num)})
     output$summary <- renderPrint({  summary(datasetInput())    })
     
     output$plot_1 <- renderPlot({ make_chart(input$stock1) })
     output$plot_2 <- renderPlot({ make_chart(input$stock2) })
     output$plot_3 <- renderPlot({ make_chart(input$stock3) })
     output$plot_4 <- renderPlot({ make_chart(input$stock4) })
     
      observeEvent(
            input$xtype,
            output$file <- downloadHandler(
                filename = paste0('make_chart().', input$xtype),
                content = function(file) {
                    image <- switch(input$xtype,
                                    png=png, jpeg=jpeg, bmp=bmp)
                    image(file, width=fig.w, height=fig.h)
                    print(make_chart())
                    dev.off()
                }
            ))
     
     
   })
