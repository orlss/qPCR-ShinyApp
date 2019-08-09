#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(data.table)
library(dplyr)

plate.row <- 1:12
plate <- plate.row
for (i in 1:7) {
  plate.row <- ((as.integer(i)*12)+1):((as.integer(i)*12)+12)
  plate <- rbind(plate, plate.row)
}
row.names(plate) <- letters[1:8]
plate <- as.data.frame(plate)
names(plate) <- 1:12

df <- data.frame()

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("qPCR Analysis ShinyApp"),
  wellPanel(textInput(inputId = "titleinput",
                         label = "Enter experiment title:")),
  wellPanel(fileInput(inputId = "upload", 
            label = "Attach CSV file below. Make sure only the following values are included: Well, Target, Sample, Cq.",
            multiple = FALSE, 
            accept = ".csv")), 
  DT::dataTableOutput('mytable'),
  textOutput("upload_path")
)

        # Show a plot of the generated distribution
        # mainPanel(
        #    plotOutput("distPlot")
        # )

# Define server logic required to draw a histogram
server <- function(input, output) {
    # x <- read.csv(input$upload,
    #             header = TRUE,
    #             sep = ",",
    #             stringsAsFactors = TRUE,
    #             row.names = NULL)
  observeEvent(input$upload,{
    temp_df <- fread(file = input$upload$datapath)
    wells <- as.character(unique(input$upload$Well))
    Cqs <- as.character(unique(input$upload$Cq))
    samples <- as.character(unique(input$upload$Sample))
    primers <- as.character(unique(input$upload$Target))
    for (i in wells) {
      append(df, input$upload$Cq, after = well==i)
    }
  })
 
  attach(temp_df, df, "datafile")
  output$upload_path = renderText(c("File Path:", as.character(input$upload$datapath), collapse = "\n"))
  
    shinyInput = function(FUN, len, col.num, id,...) {
      inputs = character(len)
      for (i in seq_len(len)) {
      value=((col.num)+((i-1)*12))
      inputs[i]= as.character( FUN(paste0(id, value), label=paste0(id, value), width='10%',...) )
      # inputs[i]= as.character(FUN(paste0(id, (12*((i)-1)+1)), label= paste0(id, i),...))
      }
     inputs
   }
  df <- output$mytable
  # datatable with checkbox
  output$mytable = DT::renderDataTable({
    data.frame(Col1=shinyInput(checkboxInput, 8, 1, "#"),
              Col2=shinyInput(checkboxInput, 8, 2, "#"),
              Col3=shinyInput(checkboxInput, 8, 3, "#"),
              Col4=shinyInput(checkboxInput, 8, 4, "#"),
              Col5=shinyInput(checkboxInput, 8, 5, "#"),
              Col6=shinyInput(checkboxInput, 8, 6, "#"),
              Col7=shinyInput(checkboxInput, 8, 7, "#"),
              Col8=shinyInput(checkboxInput, 8, 8, "#"),
              Col9=shinyInput(checkboxInput, 8, 9, "#"),
              Col10=shinyInput(checkboxInput, 8, 10, "#"),
              Col11=shinyInput(checkboxInput, 8, 11, "#"),
              Col12=shinyInput(checkboxInput, 8, 12, "#")
              )
              # inFile <- input$file1,
              # if (is.null(inFile)) return(NULL),
              # read.csv(inFile$datapath, header = input$header)
  }, server = FALSE, escape = FALSE, options = list(
    paging= FALSE,
    preDrawCallback = JS('function(){
                         Shiny.unbindAll(this.api().table().node()); }'),
    drawCallback = JS('function() {
                      Shiny.bindAll(this.api().table().node()); }')
  ) )
  
  # helper function for reading checkbox
  shinyValue = function(id, len) {
    unlist(lapply(seq_len(len))) = function(i) {
      value = input[[paste0(id, i)]]
      if (is.null(value)) NA else value
    }
      # output read checkboxes
      output$checked <- renderTable({
        data.frame(selected=shinyValue("#",nrow(mtcars)))
      })
}

# run the application
shinyApp(ui = ui, server = server)


