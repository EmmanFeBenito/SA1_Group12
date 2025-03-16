calculate_defective_probability <- function() {
  cat("Enter the proportion of production (x1, x2, x3):\n")
  x1 <- as.numeric(readline("x1: "))
  x2 <- as.numeric(readline("x2: "))
  x3 <- as.numeric(readline("x3: "))
  
  if (x1 < 0.10 || x1 > 0.40 || x2 < 0.10 || x2 > 0.40 || x3 < 0.10 || x3 > 0.40) {
    stop("Error: x values must be between 0.10 and 0.40.")
  }
  
  if (round(x1 + x2 + x3, 2) != 1) {
    stop("Error: x1 + x2 + x3 must sum to 1.")
  }
  
  cat("Enter the defective rates (y1, y2, y3):\n")
  y1 <- as.numeric(readline("y1: "))
  y2 <- as.numeric(readline("y2: "))
  y3 <- as.numeric(readline("y3: "))
  
  if (y1 < 0.01 || y1 > 0.05 || y2 < 0.01 || y2 > 0.05 || y3 < 0.01 || y3 > 0.05) {
    stop("Error: y values must be between 0.01 and 0.05.")
  }
  
  if (round(y1 + y2 + y3, 2) != 0.12) {
    stop("Error: y1 + y2 + y3 must sum to 0.12.")
  }
  
  defective_prob <- (x1 * y1) + (x2 * y2) + (x3 * y3)
  
  cat("The probability of selecting a defective product is:", defective_prob, "\n")
}

calculate_defective_probability()
