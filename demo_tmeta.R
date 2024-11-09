# demo_tmeta fits tMeta on the mag dataset. 
# Note: To run the results on another dataset, you can replace the mag dataset.

library(ggplot2)
library(gridExtra)
source("simu_1.R")

run_model <- function(y, es2, initialization_method = "rand") {
  opts <- list(maxit = 100, disp_it = 0, tol = 1e-8)
  result <- simu_1(y, es2, initialization_method)
  tme <- result$tme
  
  cat("Model fitting results for tMeta:\n")
  cat(sprintf("\n%10s%10s%10s%10s%10s%10s\n", "muhat", "sigmahat", "nuhat", "logLik", "AIC", "BIC"))
  cat(sprintf("%10.3f%10.3f%10.3f%10.3f%10.3f%10.3f\n", tme$mu, sqrt(tme$s2), tme$nu, tme$logL, tme$AIC, tme$BIC))
  
  return(tme)
}

plot_loglikelihood <- function(tme) {
  T_numeric <- as.numeric(tme$T)
  p1 <- ggplot(data = data.frame(x = 1:length(tme$ll_t), ll_t = tme$ll_t), aes(x = x, y = ll_t)) +
    geom_line(linetype = "dotted", color = "blue", linewidth = 1.5) +
    labs(x = "Iteration number", y = "Log-likelihood") +
    theme_minimal()
  
  p2 <- ggplot(data = data.frame(T = T_numeric, ll_t = tme$ll_t), aes(x = T, y = ll_t)) +
    geom_line(linetype = "dotted", color = "blue", linewidth = 1.5) +
    labs(x = "CPU time", y = "Log-likelihood") +
    theme_minimal()
  
  # Arrange the two plots side by side
  grid.arrange(p1, p2, ncol = 2)
}

demo_tmeta <- function(y, es2, initialization_method = "rand") {
  tme <- run_model(y, es2, initialization_method)
  plot_loglikelihood(tme)
}

data <- read.table('data/mag.txt');y <- data[, 2];es2 <- data[, 3]^2;
demo_tmeta(y, es2, initialization_method = "rand")