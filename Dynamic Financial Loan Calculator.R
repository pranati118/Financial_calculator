# Load the necessary libraries
library(shiny)
library(ggplot2)
library(scales)

# Define the UI
ui <- fluidPage(
  
  # Centered title
  tags$head(
    tags$style(HTML("
            h2 {text-align: center;}
        "))
  ),
  
  titlePanel(h2("Dynamic Financial Loan Calculator ")),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("loan", "Loan Amount (₹):", value = 500000, min = 0),
      sliderInput("rate", "Annual Interest Rate (%):", min = 1, max = 15, value = 6),
      sliderInput("term", "Loan Term (Years):", min = 1, max = 30, value = 15),
      actionButton("calculate", "Calculate"),
      br(),
      br(),
      h4("Results:"),
      textOutput("monthly_payment"),
      textOutput("total_interest")
    ),
    
    mainPanel(
      plotOutput("payment_plot"),
      p("This app calculates the monthly loan payment based on the provided loan amount, interest rate, and loan term. The graph shows how the loan balance decreases over time, along with the interest paid.")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Function to calculate loan payments
  calculate_loan <- eventReactive(input$calculate, {
    P <- input$loan  # Loan amount
    r <- input$rate / 100 / 12  # Monthly interest rate
    n <- input$term * 12  # Number of months
    
    # Monthly payment formula
    monthly_payment <- P * r * (1 + r)^n / ((1 + r)^n - 1)
    
    # Calculate total interest paid
    total_paid <- monthly_payment * n
    total_interest <- total_paid - P
    
    # Create a payment schedule data frame
    balance <- P
    schedule <- data.frame(Month = 1:n, Balance = NA, Interest = NA, Principal = NA)
    
    for (i in 1:n) {
      interest_payment <- balance * r
      principal_payment <- monthly_payment - interest_payment
      balance <- balance - principal_payment
      schedule[i, ] <- c(i, balance, interest_payment, principal_payment)
    }
    
    list(monthly_payment = monthly_payment, total_interest = total_interest, schedule = schedule)
  })
  
  # Display the monthly payment
  output$monthly_payment <- renderText({
    req(calculate_loan())
    paste("Monthly Payment: ₹", round(calculate_loan()$monthly_payment, 2))
  })
  
  # Display the total interest paid
  output$total_interest <- renderText({
    req(calculate_loan())
    paste("Total Interest Paid: ₹", round(calculate_loan()$total_interest, 2))
  })
  
  # Plot the loan balance and interest paid over time
  output$payment_plot <- renderPlot({
    req(calculate_loan())
    schedule <- calculate_loan()$schedule
    
    # Create a data frame for plotting
    plot_data <- data.frame(
      Month = schedule$Month,
      Balance = schedule$Balance,
      InterestPaid = schedule$Interest
    )
    
    # Plot the loan balance and interest paid over time
    ggplot(plot_data, aes(x = Month)) +
      geom_line(aes(y = Balance, color = "Loan Balance")) +
      geom_line(aes(y = InterestPaid, color = "Interest Paid")) +
      scale_y_continuous(labels = dollar_format(prefix = "₹")) +
      labs(title = "Loan Balance and Interest Paid Over Time", 
           x = "Month", y = "Amount (₹)") +
      scale_color_manual(name = "Legend", values = c("Loan Balance" = "blue", "Interest Paid" = "red")) +
      theme_minimal()
  })
}

# Run the app
shinyApp(ui, server)
