install.packages('shiny'); library(shiny)

# Input controls - text
{ui <- fluidPage(
  textInput("name", "What's your name?"),
  passwordInput("password", "What's your password?"),
  textAreaInput("story", "Tell me about yourself", rows = 3)
)

server <- function(input, output, session) {}

shinyApp(ui, server)}

# Input controls - numeric
{ui <- fluidPage(
  numericInput("num", "Number one", value = 0, min = 0, max = 100),
  sliderInput("num2", "Number two", value = 50, min = 0, max = 100),
  sliderInput("rng", "Range", value = c(10, 20), min = 0, max = 100)
)

server <- function(input, output, session) {}

shinyApp(ui, server)}

# Input controls - dates
{ui <- fluidPage(
  dateInput("dob", "When were you born?"),
  dateRangeInput("range", "What is the date range of your data?")
)
  
  server <- function(input, output, session) {}
  
  shinyApp(ui, server)}

# Input controls - choices (drop downs)
{ui <- fluidPage(
  selectInput("state", "What's your favorite state?", state.name),
  selectInput("state2", "What's your favourite state(s)?", state.name, multiple = TRUE)
)
  
  server <- function(input, output, session) {}
  
  shinyApp(ui, server)}

# Input controls - choices (radio buttons)
{animals <- c("dog", "cat", "mouse", "bird", "other", "I don't like animals")
  
  ui <- fluidPage(
    radioButtons("animal", "What's your favorite animal?", animals)
  )
  
  server <- function(input, output, session) {}
  
  shinyApp(ui, server)}

# Input controls - choices (checkbox)
{ui <- fluidPage(
  radioButtons("animal", "What's your favorite animal?", animals),
  checkboxGroupInput("animal", "What animals do you like?", animals)
)
  
  server <- function(input, output, session) {}
  
  shinyApp(ui, server)}

# Input controls - uploads
{ui <- fluidPage(
  fileInput("upload", NULL)
)
  
  server <- function(input, output, session) {}
  
  shinyApp(ui, server)}

# Input controls - action buttons
{ui <- fluidPage(
  actionButton("click", "Click me!"),
  actionButton("download", "Download")
)
  
  server <- function(input, output, session) {}
  
  shinyApp(ui, server)}

# Output controls - text
{ui <- fluidPage(
  textInput("name", "What's your name?"),
  textOutput("text")
)

server <- function(input, output, session) {
  output$text <- renderText({ 
    input$name
  })
}

shinyApp(ui, server)}

# Output controls - tables
{ui <- fluidPage(
  tableOutput("static"),
  dataTableOutput("dynamic")
)

server <- function(input, output, session) {
  output$static <- renderTable(head(mtcars))
  output$dynamic <- renderDataTable(mtcars, options = list(pageLength = 5))
}

shinyApp(ui, server)}

# Output controls - plots
{library(ggplot2)

ui <- fluidPage(
  plotOutput("plot", width = "800px")
)

server <- function(input, output, session) {
  output$plot <- renderPlot(
    ggplot(mtcars) + geom_point(aes(mpg,hp))
  )
}

shinyApp(ui, server)}
