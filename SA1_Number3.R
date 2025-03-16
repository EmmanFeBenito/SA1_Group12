## 3.
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
