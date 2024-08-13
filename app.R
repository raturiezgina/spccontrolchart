library(datasets)
library(RCurl) 
library(shiny) 
library(plotly)
ui <- fluidPage( 
  fluidRow(
    box(
      title = div(strong("SPC Control Chart")), 
      width = 12,
      plotlyOutput("spc_plot")
    ),
    # box(
    #   title = div(strong('Exploratory Data Analysis in R')), 
    #   width = 8,
    #   plotlyOutput("plot_two_categories")
    # )
  )
)

server <- function(input, output, session) { 
  
  spc_plot <- function(sernum,SourceData,target,usl,lsl){
    
    myData <- data.frame(value = SourceData, id = 1:length(SourceData))
    buffer <- myData$value < (mean(myData$value)-(3*sd(myData$value))) | myData$value > (mean(myData$value)+(3*sd(myData$value)))
    bynd <- data.frame(id = myData$id[buffer],
                       value = myData$value[buffer])
    x <- 0
    y <- 0
    buffer2 <- data.frame(violation = double(),observation = integer(),stringsAsFactors=FALSE)
    
    for (i in 1:length(myData$value)){
      if (myData$value[i] > mean(myData$value)){
        x = x + 1
        y = 0
        if (x > 6){
          buffer2 <- rbind(buffer2,c(myData$value[i],myData$id[i]))
          
        }
      }else if (myData$value[i] < mean(myData$value)){
        x = 0
        y = y + 1
        if (y > 6){
          buffer2 <- rbind(buffer2,c(myData$value[i],myData$id[i]))
        }
      }
    }
    colnames(buffer2) <- c("violation","observation")
    
    LCL <- (mean((SourceData))-(3*sd(SourceData)))
    UCL <- (mean((SourceData))+(3*sd(SourceData)))
    CL <- mean(SourceData)
    
    trace1 <- list(line = list(color = "#1F618D",width = 2),  #34495E
                   mode = "lines+markers", 
                   name = "Data", 
                   type = "scatter",
                   x = c(1:length(SourceData)),
                   y = c(SourceData), 
                   xaxis = "x", 
                   yaxis = "y", 
                   text = c(sernum),
                   hovertemplate = paste('<br><b>Chip ID : </b>%{text}<br>',
                                         '<b>Avg : </b> <b>%{y}</b>')
                   #marker = list(size = 6,color = "#34495E",symbol = "circle")  
    )
    
    trace2 <- list(mode = "markers", 
                   name = "Beyond", 
                   type = "scatter",
                   x = c(bynd$id),
                   y = c(bynd$value), 
                   xaxis = "x", 
                   yaxis = "y", 
                   marker = list(line = list(width = 3),size = 8,color = "#F90303",symbol = "circle-open"))
    
    trace3 <- list(line = list(color = "#C0392B",width = 2),
                   mode = "lines", 
                   name = "Center", 
                   type = "scatter", 
                   x = c(0, length(SourceData)+1),
                   y = c(CL, CL), 
                   xaxis = "x", 
                   yaxis = "y")
    
    trace4 <- list(line = list(color = "#C0392B",width = 2,dash = 'dot'),
                   mode = "lines",
                   name = "LCL/UCL", 
                   type = "scatter", 
                   x = c(0, length(SourceData)+1, NA,0, length(SourceData)+1),
                   y = c(LCL, LCL, NA,UCL, UCL),
                   xaxis = "x", 
                   yaxis = "y")
    
    trace5 <- list(line = list(color = "#1F618D",width = 2,dash = 'longdash'),
                   mode = "lines", 
                   name = "USL/LSL",
                   type = "scatter",
                   x = c(0, length(SourceData)+1, NA,0, length(SourceData)+1),
                   y = c(usl, usl, NA,lsl, lsl),
                   xaxis = "x", 
                   yaxis = "y")
    
    trace6 <- list(line = list(color = "#1F618D",width = 2, dash = 'dashdot'),
                   mode = "lines", 
                   name = "Target", 
                   type = "scatter", 
                   x = c(0, length(SourceData)+1),
                   y = c(target, target), 
                   xaxis = "x", 
                   yaxis = "y")
    
    trace7 <- list(mode = "markers",
                   name = "Violation",
                   type = "scatter",
                   x = c(buffer2$observation),
                   y = c(buffer2$violation),
                   xaxis = "x",
                   yaxis = "y",
                   marker = list(line = list(width = 3),size = 8,color = "#F1C40F",symbol = "circle-open"))
    
    layout <- list(xaxis = list(side = "bottom", 
                                type = "linear", 
                                range = c(0, length(SourceData)+1), 
                                ticks = "inside", 
                                title = list(text = "Observation"), 
                                anchor = "y", 
                                domain = c(0.03, 0.9), 
                                mirror = "allticks", 
                                showgrid = TRUE, 
                                showline = TRUE, 
                                zeroline = TRUE, 
                                autorange = FALSE, 
                                linecolor = "rgb(34,34,34)", 
                                linewidth = 1), 
                   yaxis = list(side = "left", 
                                type = "linear", 
                                ticks = "inside", 
                                title = list(text = "Individual Value"), 
                                anchor = "x", 
                                domain = c(0.11, 0.925), 
                                mirror = "allticks", 
                                showgrid = TRUE, 
                                showline = TRUE, 
                                zeroline = FALSE, 
                                autorange = TRUE, 
                                linecolor = "rgb(34,34,34)", 
                                linewidth = 1), 
                   legend = list(x = 0.92, 
                                 y = 0.5, 
                                 xref = "paper", 
                                 yref = "paper", 
                                 bgcolor = "#F8F9F9", 
                                 xanchor = "left", 
                                 yanchor = "bottom"), 
                   margin = list(b = 0, l = 0, r = 0, t = 0, pad = 0, autoexpand = TRUE)
    )
    
    hist_right <- ggplot(myData,aes(x=value)) +
      geom_histogram(data=myData,colour = "#C0392B", fill = "#E74C3C", alpha = 0.2, bins = 13) +
      geom_vline(aes(xintercept=target),linetype="dashed",size=0.1) +
      geom_vline(aes(xintercept=lsl),linetype="dashed", size=0.1) +
      geom_vline(aes(xintercept=usl),linetype="dashed",size=0.1) +
      geom_vline(aes(xintercept=CL),linetype="dashed",size=0.2) +
      geom_vline(aes(xintercept=LCL),linetype="dashed", size=0.2) +
      geom_vline(aes(xintercept=UCL),linetype="dashed",size=0.2) +
      coord_flip()
    hist_right <- ggplotly(p = hist_right)
    
    SpcCtrlChart <- plot_ly() %>%
      add_trace(line=trace1$line, mode=trace1$mode, name=trace1$name, type=trace1$type, x=trace1$x, y=trace1$y,
                xaxis=trace1$xaxis, yaxis=trace1$yaxis, text = trace1$text, hovertemplate = trace1$hovertemplate # marker=trace1$marker
      ) %>%
      add_trace(mode=trace2$mode, name=trace2$name, type=trace2$type,  x=trace2$x,  y=trace2$y,  marker=trace2$marker) %>%
      add_trace(line=trace3$line, mode=trace3$mode, type=trace3$type, name=trace3$name, x=trace3$x, y=trace3$y, xaxis=trace3$xaxis, yaxis=trace3$yaxis) %>%
      add_trace(line=trace4$line, mode=trace4$mode, type=trace4$type, name=trace4$name, x=trace4$x, y=trace4$y, xaxis=trace4$xaxis, yaxis=trace4$yaxis) %>%
      add_trace(line=trace5$line, mode=trace5$mode, type=trace5$type, name=trace5$name, x=trace5$x, y=trace5$y, xaxis=trace5$xaxis, yaxis=trace5$yaxis) %>%
      add_trace(line=trace6$line, mode=trace6$mode, type=trace6$type, name=trace6$name, x=trace6$x, y=trace6$y, xaxis=trace6$xaxis, yaxis=trace6$yaxis) %>%
      add_trace(mode=trace7$mode, name=trace7$name, type=trace7$type,  x=trace7$x,  y=trace7$y,  marker=trace7$marker)
    SpcCtrlChart <- ggplotly(p = SpcCtrlChart,type = 'scatter')
    
    s <- subplot(SpcCtrlChart, hist_right, nrows = 1, heights = 1, widths = c(0.8, 0.2), margin = 0.01, shareX = TRUE, shareY = TRUE, titleX = TRUE, titleY = TRUE)
    
    layout(s, xaxis= list(range = c(0, length(SourceData)+1), showgrid = TRUE, showline = TRUE,title = list(text = "Observation")),
           yaxis= list(title = list(text = "Individual Value")),showlegend = FALSE, plot_bgcolor = "white", paper_bgcolor = "white")
    
  }
  tryCatch({
    #getValue <- read.csv("C:/Users/FORMULATRIX/Documents/R/mantis csv format/getValue - Low Volume Silicone.csv")
    
    getValue <-  read.csv("https://raw.githubusercontent.com/raturiezgina/spccontrolchart/main/getValue%20-%20LVS.csv")
    ##### Display summary statistics ######
    output$spc_plot <- renderPlotly ({
      spc_plot(getValue$serial_number,as.numeric(getValue$calculation_sd),0.1,0.107,0.093)
    })
     
  },
  error=function(cond) {
    showModal(modalDialog(
      title = "Oops!!",
      paste0(cond),
      easyClose = TRUE,
      footer = NULL
    ))
    return(NA)
  },
  warning=function(cond) {
    showModal(modalDialog(
      title = "Oops!!",
      paste0(cond),
      easyClose = TRUE,
      footer = NULL
    ))
    return(NULL)
  })
  
}

shinyApp(ui, server)
