library(shiny)
library(DT)

# Define UI
ui <- fluidPage(
  titlePanel("Table and Image Display"),
  sidebarLayout(
    sidebarPanel(
      fileInput("csv_file", "Choose CSV File"),
      fileInput("image_file", "Choose PNG Image")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Table", DTOutput("table")),
        tabPanel("Image", imageOutput("image"))
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
  # Read the CSV file
  data <- reactive({
    req(input$csv_file)
    read.csv(input$csv_file$datapath, stringsAsFactors = FALSE)
  })
  
  # Display the table
  output$table <- renderDT({
    datatable(data())
  })
  
  # Display the image
  output$image <- renderImage({
    req(input$image_file)
    list(src = input$image_file$datapath,
         contentType = 'image/png',
         alt = "PNG Image")
  }, deleteFile = FALSE)
}

# Run the application
shinyApp(ui = ui, server = server)

