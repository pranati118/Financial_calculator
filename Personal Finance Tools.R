library(shiny)
library(DT)

ui <- navbarPage(
  "Personal Finance Tools",
  
  tabPanel("Loan & Mortgage",
           sidebarLayout(
             sidebarPanel(
               numericInput("loan_amount", "Loan Amount: ", value = 100000, min = 1),
               numericInput("interest_rate", "Annual Interest Rate (%):", value = 5, min = 0),
               numericInput("loan_term", "Loan Term (years):", value = 30, min = 1),
               numericInput("prepayment", "Monthly Prepayment (optional):", value = 0, min = 0),
               actionButton("calculate_loan", "Calculate")
             ),
             mainPanel(
               h3("Monthly Payment:"),
               textOutput("monthly_payment"),
               h3("Amortization Schedule:"),
               dataTableOutput("amortization_table")
             )
           )),

  tabPanel("Savings",
           sidebarLayout(
             sidebarPanel(
               numericInput("initial_savings", "Initial Savings Amount:", value = 1000, min = 0),
               numericInput("monthly_savings", "Monthly Savings Contribution:", value = 200, min = 0),
               numericInput("savings_rate", "Annual Interest Rate (%):", value = 4, min = 0),
               numericInput("savings_term", "Savings Period (years):", value = 10, min = 1),
               actionButton("calculate_savings", "Calculate")
             ),
             mainPanel(
               h3("Future Value of Savings:"),
               textOutput("future_savings"),
               h3("Savings Growth Over Time:"),
               plotOutput("savings_plot")
             )
           )),

  tabPanel("Budgeting",
           sidebarLayout(
             sidebarPanel(
               numericInput("income", "Monthly Income:", value = 4000, min = 1),
               numericInput("expenses", "Total Monthly Expenses:", value = 2500, min = 0),
               actionButton("calculate_budget", "Calculate")
             ),
             mainPanel(
               h3("Savings Rate:"),
               textOutput("savings_rate_output"),
               h3("Debt-to-Income Ratio:"),
               textOutput("debt_income_ratio")
             )
           ))
)

server <- function(input, output) {

  observeEvent(input$calculate_loan, {
    loan_amount <- input$loan_amount
    interest_rate <- input$interest_rate / 100 / 12
    loan_term <- input$loan_term * 12
    prepayment <- input$prepayment
    
    monthly_payment <- (loan_amount * interest_rate) / 
      (1 - (1 + interest_rate)^-loan_term)
    total_monthly <- monthly_payment + prepayment
    
    output$monthly_payment <- renderText({
      paste("Rs.", round(total_monthly, 2))
    })

    schedule <- data.frame(
      Month = numeric(),
      Principal = numeric(),
      Interest = numeric(),
      Balance = numeric()
    )
    balance <- loan_amount
    for (month in 1:loan_term) {
      interest <- balance * interest_rate
      principal <- total_monthly - interest
      balance <- balance - principal
      schedule <- rbind(schedule, data.frame(
        Month = month,
        Principal = round(principal, 2),
        Interest = round(interest, 2),
        Balance = round(max(balance, 0), 2)
      ))
      if (balance <= 0) break
    }
    
    output$amortization_table <- renderDataTable({
      datatable(schedule)
    })
  })

  observeEvent(input$calculate_savings, {
    initial_savings <- input$initial_savings
    monthly_savings <- input$monthly_savings
    savings_rate <- input$savings_rate / 100 / 12
    savings_term <- input$savings_term * 12
    
    future_value <- initial_savings * (1 + savings_rate)^savings_term +
      monthly_savings * ((1 + savings_rate)^savings_term - 1) / savings_rate
    
    output$future_savings <- renderText({
      paste("Rs.", round(future_value, 2))
    })

    savings_over_time <- numeric(savings_term)
    for (month in 1:savings_term) {
      initial_savings <- initial_savings * (1 + savings_rate)
      initial_savings <- initial_savings + monthly_savings
      savings_over_time[month] <- initial_savings
    }
    
    output$savings_plot <- renderPlot({
      plot(savings_over_time, type = "l", col = "blue",
           xlab = "Months", ylab = "Savings ($)",
           main = "Savings Growth Over Time")
    })
  })

  observeEvent(input$calculate_budget, {
    income <- input$income
    expenses <- input$expenses
    
    savings_rate <- ((income - expenses) / income) * 100
    debt_income_ratio <- (expenses / income) * 100
    
    output$savings_rate_output <- renderText({
      paste(round(savings_rate, 2), "%")
    })
    
    output$debt_income_ratio <- renderText({
      paste(round(debt_income_ratio, 2), "%")
    })
  })
}

shinyApp(ui = ui, server = server)
