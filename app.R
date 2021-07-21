library(shiny)
library(openair)
#library(readr,lib.loc="/home/pveronesi/R/x86_64-pc-linux-gnu-library/3.2/")
# Define UI for data download app ----

ui <- fluidPage(
  
  # App title ----
  titlePanel("Rose"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(width=2,
      
      # Input: Choose dataset ----
      fileInput('file1', '',
                accept=('text/csv')),
      sliderInput("gradi","Gradi per settore",0,60,15,step=0.5)
      
      
      
     
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(width=12,
              tags$a(href="dex.csv", "csv demo"),
              tags$body(
                p("Rosa dei venti 24h")),
      plotOutput("gvis"),
      tags$body(
        p("Rosa dei venti diurna")),
      plotOutput("gvis2"),
      tags$body(
        p("Rosa dei venti notturna")),
      plotOutput("gvis3"),
      tags$body(
        p("Rosa dei venti stagionale")),
      plotOutput("gvis4")
      )
      
      
    )
    
  )


# Define server logic to display and download selected file ----


server <- function(input, output,session) {
  output$gvis<-renderPlot({
  validate(
    need(!is.null(input$file1),"Carica prima un file"))
  inFile <- input$file1
  file<-read.csv(inFile$datapath)
  colnames(file)<-c("date","fin","wd","ws")
  file$date<-as.POSIXct(strptime(file$date,format ="%Y-%m-%d %H:%M", tz="GMT"))
  file<-cutData (file,type="daylight")
 plot1<- windRose(file,paddle=FALSE, angle=input$gradi,ws.int=0.5, offset=10, border=TRUE, breaks=c(0,0.3,1.5,3.4,5.4,7.9,35))
 plot(plot1)})
  

output$gvis2<-renderPlot({
  validate(
    need(!is.null(input$file1),"Carica prima un file"))
  inFile <- input$file1
  file<-read.csv(inFile$datapath)
  colnames(file)<-c("date","fin","wd","ws")
  file$date<-as.POSIXct(strptime(file$date,format ="%Y-%m-%d %H:%M", tz="GMT"))
  file<-cutData (file,type="daylight")
  plot1<- windRose(selectByDate(file[file$daylight=="daylight",]),paddle=FALSE, angle=input$gradi,ws.int=0.5, offset=10, border=TRUE, breaks=c(0,0.3,1.5,3.4,5.4,7.9,35),annotate=TRUE)
  plot(plot1)})

output$gvis3<-renderPlot({
  validate(
    need(!is.null(input$file1),"Carica prima un file"))
  inFile <- input$file1
  file<-read.csv(inFile$datapath)
  colnames(file)<-c("date","fin","wd","ws")
  file$date<-as.POSIXct(strptime(file$date,format ="%Y-%m-%d %H:%M", tz="GMT"))
  file<-cutData (file,type="daylight")
  
  plot1<- windRose(selectByDate(file[file$daylight=="nighttime",]),paddle=FALSE, angle=input$gradi,ws.int=0.5, offset=10, border=TRUE, breaks=c(0,0.3,1.5,3.4,5.4,7.9,35))
  plot(plot1)})

output$gvis4<-renderPlot({
  validate(
    need(!is.null(input$file1),"Carica prima un file"))
  inFile <- input$file1
  file<-read.csv(inFile$datapath)
  colnames(file)<-c("date","fin","wd","ws")
  file$date<-as.POSIXct(strptime(file$date,format ="%Y-%m-%d %H:%M", tz="GMT"))
  file<-cutData (file,type="daylight")
  
  plot1<- windRose(selectByDate(file[file$daylight=="nighttime",]),paddle=FALSE, angle=input$gradi,ws.int=0.5, offset=10, border=TRUE, breaks=c(0,0.3,1.5,3.4,5.4,7.9,35),type="season")
  plot(plot1)})

}

# Create Shiny app ----
shinyApp(ui, server)