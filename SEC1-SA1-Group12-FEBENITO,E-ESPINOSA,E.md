SEC1-SA1 Group12-FEBENITO,E;ESPINOSA,E
================
Emmanuel Fe Benito
2025-03-16

## 1.

``` r
calculate_defective_probability <- function() {

  read_numeric_input <- function(prompt) {
    while (TRUE) {
      input <- readline(prompt)
      input <- as.numeric(input)
      if (!is.na(input)) {
        return(input)
      }
      cat("Invalid input. Please enter a numeric value.\n")
    }
  }
  
  cat("Enter the proportion of production (x1, x2, x3):\n")
  x1 <- read_numeric_input("x1: ")
  x2 <- read_numeric_input("x2: ")
  x3 <- read_numeric_input("x3: ")
  
  if (x1 < 0.10 || x1 > 0.40 || x2 < 0.10 || x2 > 0.40 || x3 < 0.10 || x3 > 0.40) {
    stop("Error: x values must be between 0.10 and 0.40.")
  }
  
  if (abs(x1 + x2 + x3 - 1) > 1e-10) {
    stop("Error: x1 + x2 + x3 must sum to 1.")
  }
  
  cat("Enter the defective rates (y1, y2, y3):\n")
  y1 <- read_numeric_input("y1: ")
  y2 <- read_numeric_input("y2: ")
  y3 <- read_numeric_input("y3: ")
  
  if (y1 < 0.01 || y1 > 0.05 || y2 < 0.01 || y2 > 0.05 || y3 < 0.01 || y3 > 0.05) {
    stop("Error: y values must be between 0.01 and 0.05.")
  }
  
  if (abs(y1 + y2 + y3 - 0.12) > 1e-10) {
    stop("Error: y1 + y2 + y3 must sum to 0.12.")
  }
  
  defective_prob <- (x1 * y1) + (x2 * y2) + (x3 * y3)
  
  cat("The probability of selecting a defective product is:", defective_prob, "\n")
}

## remove comment below, knitting stalls if this isn't a comment
## calculate_defective_probability()
```

## 2.

``` r
library(shiny)
```

    ## Warning: package 'shiny' was built under R version 4.4.3

``` r
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
    })

    output$bi_cdf <- renderPlot({
      cdf_matrix <- apply(prob_matrix, 2, function(col) cumsum(col))
      cdf_matrix <- apply(cdf_matrix, 1, function(row) cumsum(row))
      persp(values_x, values_y, cdf_matrix, theta = 30, phi = 30, expand = 0.5, col = "lightgreen", 
            xlab = "X", ylab = "Y", zlab = "Cumulative Probability", main = "CDF")
    })
  })
}

shinyApp(ui = ui, server = server)
```

    ## 
    ## Listening on http://127.0.0.1:6397

![](SEC1-SA1-Group12-FEBENITO,E-ESPINOSA,E_files/figure-gfm/Number%202-1.png)<!-- -->

## 3.

``` r
library(shiny)

ui <- fluidPage(
  titlePanel("SA1 Number 3"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("p", "Probability of success (p):", value = 0, min = 0, max = 1, step = 0.01),
      actionButton("simulate", "Run Simulation")
    ),
    
    mainPanel(
      h4("Simulated PDF of Searches"),
      plotOutput("pdf_plot"),
      h4("Mean of Searches"),
      textOutput("mean_searches"),
      h4("Variance of Searches"),
      textOutput("var_searches"),
      h4("Conditional PDF of Searches (Given X > 3)"),
      plotOutput("conditional_pdf_plot"),
      h4("Mean of Conditional Searches"),
      textOutput("mean_conditional"),
      h4("Variance of Conditional Searches"),
      textOutput("var_conditional"),
      h4("Markov Memoryless Property Verification"),
      uiOutput("markov_property") 
    )
  )
)

server <- function(input, output) {
  
  observeEvent(input$simulate, {
    if (is.na(input$p)) {
      showNotification("Please enter a valid probability.", type = "error")
      return()
    }
    if (input$p < 0 || input$p > 1) {
      showNotification("Probability must be between 0 and 1.", type = "error")
      return()
    }
    
    n_simulations <- 10000
    
    searches <- rgeom(n_simulations, prob = input$p) + 1  
    
    output$pdf_plot <- renderPlot({
      hist(searches, breaks = seq(0.5, max(searches) + 0.5), prob = TRUE, 
           main = "Simulated PDF of Searches", xlab = "Number of Searches", ylab = "Probability")
    })
    
    mean_searches <- mean(searches)
    var_searches <- var(searches)
    output$mean_searches <- renderText({
      paste("Mean of searches:", round(mean_searches, 3))
    })
    output$var_searches <- renderText({
      paste("Variance of searches:", round(var_searches, 3))
    })
    
    conditional_searches <- searches[searches > 3] - 3
    
    output$conditional_pdf_plot <- renderPlot({
      hist(conditional_searches, breaks = seq(0.5, max(conditional_searches) + 0.5), prob = TRUE, 
           main = "Conditional PDF of Searches (Given X > 3)", xlab = "Number of Searches", ylab = "Probability")
    })
    
    mean_conditional <- mean(conditional_searches)
    var_conditional <- var(conditional_searches)
    output$mean_conditional <- renderText({
      paste("Mean of conditional searches:", round(mean_conditional, 4))
    })
    output$var_conditional <- renderText({
      paste("Variance of conditional searches:", round(var_conditional, 4))
    })
    
    p_X4_given_X_gt_3 <- sum(searches == 4) / sum(searches > 3)
    p_X1 <- sum(searches == 1) / n_simulations
    
    p_X5_given_X_gt_3 <- sum(searches == 5) / sum(searches > 3)
    p_X2 <- sum(searches == 2) / n_simulations
    
    output$markov_property <- renderUI({
      HTML(paste(
        "P(X = 4 | X > 3):", round(p_X4_given_X_gt_3, 4), "<br>",
        "P(X = 1):", round(p_X1, 4), "<br>",
        "P(X = 5 | X > 3):", round(p_X5_given_X_gt_3, 4), "<br>",
        "P(X = 2):", round(p_X2, 4)
      ))
    })
  })
}

shinyApp(ui = ui, server = server)
```

    ## 
    ## Listening on http://127.0.0.1:7739

![](SEC1-SA1-Group12-FEBENITO,E-ESPINOSA,E_files/figure-gfm/Number%203-1.png)<!-- -->
