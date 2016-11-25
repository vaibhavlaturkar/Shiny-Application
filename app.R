library(shiny)
library(ggplot2)


ui <- fluidPage(
  navbarPage("Vaibhav",
             tabPanel("View Data",
                      fileInput("file1","Choose the File"),
                      tableOutput("table")),
             tabPanel("Plot",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("x","X",c("1"=1,"2"=2,"3"=3)),
                          selectInput("y","Y",c("1"=1,"2"=2,"3"=3)),
                          selectInput("color","Color",c("1"=1,"2"=2,"3"=3))
                        ),
                        mainPanel(
                          plotOutput("plot"))
                        
                      ))))


server <- function(input,output,session){
  #assigned the data to data
  data <- reactive({      
    file <- input$file1
    if(is.null(file)){return()}
    data <- read.csv(file$datapath,header = TRUE)
  })
  #view data
  output$table <- renderTable({
    if(is.null(data())){return()}
    data()[1:100,]
  })
  
  #choices assigned
  observe({
    dsnames <- names(data())
    cb_options <- list()
    cb_options[ dsnames] <- dsnames
    updateSelectInput(session, "x",
                      label = "X-Axis",
                      choices = cb_options,
                      selected = "")
    updateSelectInput(session, "y",
                      label = "Y-Axis",
                      choices = cb_options,
                      selected = "")
    updateSelectInput(session, "color",
                      label = "Color",
                      choices = cb_options,
                      selected = "")
    
  })
  #plot
  output$plot <- renderPlot({
    p <- ggplot(data(), aes_string(x=input$x, y=input$y)) + geom_point()
    if (input$color != 'None')
      p <- p + aes_string(color=input$color)
    
    print(p)
    
  }, height=600)
  
}

shinyApp(ui=ui,server = server)