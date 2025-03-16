## 2.
library(shiny)

ui <- fluidPage(
  titlePanel("SA1 Number 2"),
  
  tabsetPanel(
    tabPanel("Univariate",
             sidebarLayout(
               sidebarPanel(
                 textInput("uni_val", "Enter values (seperate by comma):", ""),
                 textInput("uni_prob", "Enter probabilities (seperate by comma):", ""),
                 actionButton("uni_calc", "Calculate")
               ),
               mainPanel(
                 h4("Mean:"),
                 textOutput("uni_mean"),
                 h4("Variance:"),
                 textOutput("uni_var"),
                 plotOutput("uni_pdf"),
                 plotOutput("uni_cdf")
               )
             )
    ),
    
    tabPanel("Bivariate",
             sidebarLayout(
               sidebarPanel(
                 textInput("bi_values_x", "Enter X values (seperate by comma):", ""),
                 textInput("bi_values_y", "Enter Y values (seperate by comma):", ""),
                 textInput("bi_prob", "Enter joint probabilities (seperate by comma):", ""),
                 actionButton("bi_calc", "Calculate")
               ),
               mainPanel(
                 h4("Mean and Variance of X:"),
                 textOutput("bi_mean_x"),
                 textOutput("bi_var_x"),
                 h4("Mean and Variance of Y:"),
                 textOutput("bi_mean_y"),
                 textOutput("bi_var_y"),
                 h4("Marginal Distribution of X:"),
                 tableOutput("marginal_x"),
                 h4("Marginal Distribution of Y:"),
                 tableOutput("marginal_y"),
                 h4("Conditional Distribution of X|Y:"),
                 tableOutput("conditional_x"),
                 h4("Conditional Distribution of Y|X:"),
                 tableOutput("conditional_y"),
                 plotOutput("bi_cdf"),
                 plotOutput("bi_pdf")
               )
             )
    )
  )
)

server <- function(input, output) {

  observeEvent(input$uni_calc, {
    values <- as.numeric(unlist(strsplit(input$uni_val, ",")))
    probs <- as.numeric(unlist(strsplit(input$uni_prob, ",")))

    if (any(probs < 0) || any(probs > 1)) {
      showNotification("Probabilities must be between 0 and 1.", type = "error")
      return()
    }
    if (sum(probs) != 1) {
      showNotification("Probabilities must sum to 1.", type = "error")
      return()
    }

    mean <- sum(values * probs)
    variance <- sum((values - mean)^2 * probs)
    
    output$uni_mean <- renderText(mean)
    output$uni_var <- renderText(variance)

    output$uni_pdf <- renderPlot({
      barplot(probs, names.arg = values, main = "PDF", xlab = "Values", ylab = "Probability")
    })
    
    output$uni_cdf <- renderPlot({
      plot(ecdf(rep(values, probs * 100)), main = "CDF", xlab = "Values", ylab = "Cumulative Probability")
    })
  })
  
  observeEvent(input$bi_calc, {
    values_x <- as.numeric(unlist(strsplit(input$bi_values_x, ",")))
    values_y <- as.numeric(unlist(strsplit(input$bi_values_y, ",")))
    prob <- as.numeric(unlist(strsplit(input$bi_prob, ",")))
    
    if (any(prob < 0) || any(prob > 1)) {
      showNotification("Probabilities must be between 0 and 1.", type = "error")
      return()
    }
    if (sum(prob) != 1) {
      showNotification("Probabilities must sum to 1.", type = "error")
      return()
    }
    
    prob_matrix <- matrix(prob, nrow = length(values_x), ncol = length(values_y), byrow = TRUE)
    
    marginal_x <- rowSums(prob_matrix)
    marginal_y <- colSums(prob_matrix)
    
    output$marginal_x <- renderTable(data.frame(Value = values_x, Probability = marginal_x))
    output$marginal_y <- renderTable(data.frame(Value = values_y, Probability = marginal_y))
 
    conditional_x <- prob_matrix / matrix(marginal_y, nrow = length(values_x), ncol = length(values_y), byrow = TRUE)
    conditional_y <- t(t(prob_matrix) / marginal_x)
    
    output$conditional_x <- renderTable(data.frame(Value = values_x, Probability = conditional_x))
    output$conditional_y <- renderTable(data.frame(Value = values_y, Probability = conditional_y))
    
    mean_x <- sum(values_x * marginal_x)
    var_x <- sum((values_x - mean_x)^2 * marginal_x)
    
    output$bi_mean_x <- renderText(paste("Mean of X:", mean_x))
    output$bi_var_x <- renderText(paste("Variance of X:", var_x))
    
    mean_y <- sum(values_y * marginal_y)
    var_y <- sum((values_y - mean_y)^2 * marginal_y)
    
    output$bi_mean_y <- renderText(paste("Mean of Y:", mean_y))
    output$bi_var_y <- renderText(paste("Variance of Y:", var_y))

    output$bi_pdf <- renderPlot({
      persp(values_x, values_y, prob_matrix, theta = 30, phi = 30, expand = 0.5, col = "lightblue", 
            xlab = "X", ylab = "Y", zlab = "Probability", main = "PDF")

    output$bi_cdf <- renderPlot({
      cdf_matrix <- apply(prob_matrix, 2, function(col) cumsum(col))
      cdf_matrix <- apply(cdf_matrix, 1, function(row) cumsum(row))
      persp(values_x, values_y, cdf_matrix, theta = 30, phi = 30, expand = 0.5, col = "lightgreen", 
            xlab = "X", ylab = "Y", zlab = "Cumulative Probability", main = "CDF")
      })
    })
  })
}

shinyApp(ui = ui, server = server)