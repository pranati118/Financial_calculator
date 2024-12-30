library(shiny)
library(quantmod)
library(plotly)

# Define UI
ui <- fluidPage(
  titlePanel("Interactive Financial Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("stock", "Select Stock :", 
                  choices = c("AAPL", "GOOGL", "MSFT"), 
                  selected = "AAPL"),
      dateRangeInput("date_range", "Select Date Range:",
                     start = Sys.Date() - 30, end = Sys.Date()),
      actionButton("update", "Update Data")
    ),
    mainPanel(
      plotlyOutput("stock_chart"),
      tableOutput("stock_data")
    )
  )
)

# Define Server
server <- function(input, output) {
  stock_data <- eventReactive(input$update, {
    req(input$stock, input$date_range)
    getSymbols(input$stock, src = "yahoo", 
               from = input$date_range[1], 
               to = input$date_range[2], 
               auto.assign = FALSE)
  })

  output$stock_chart <- renderPlotly({
    data <- stock_data()
    chart_data <- data.frame(
      Date = index(data),
      Price = as.numeric(data[, 4])  # Adjusted close
    )
    plot_ly(chart_data, x = ~Date, y = ~Price, type = 'scatter', mode = 'lines') %>%
      layout(title = paste("Stock Price for", input$stock),
             xaxis = list(title = "Date"),
             yaxis = list(title = "Price"))
  })

  output$stock_data <- renderTable({
    data <- stock_data()
    data.frame(Date = index(data), coredata(data))
  })
}

# Run App
shinyApp(ui = ui, server = server)

