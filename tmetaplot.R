# shows evolement of log-likelihood versus number of iterations and CPU time.

library(ggplot2)
library(gridExtra)

tMeta_method <- function(y, es2, initialization = "rand", maxit = 100, tol = 1e-8, disp_it = 0) {
  opts <- list(maxit = maxit, disp_it = disp_it, tol = tol)
  
  result <- simu_1(y, es2, initialization)
  
  tme <- result$tme
  t_cpu <- result$t_cpu
  opts <- result$opts
  
  cat("Model fitting results for tMeta:\n")
  cat(sprintf("\n%10s%10s%10s%10s%10s%10s\n", "muhat", "sigmahat", "nuhat", "logLik", "AIC", "BIC"))
  cat(sprintf("%10.3f%10.3f%10.3f%10.3f%10.3f%10.3f\n", tme$mu, sqrt(tme$s2), tme$nu, tme$logL, tme$AIC, tme$BIC))
  
  plot_loglikelihood(tme)
  
  return(list(mu = tme$mu, s2 = tme$s2, nu = tme$nu, logL = tme$logL, AIC = tme$AIC, BIC = tme$BIC, t_cpu = t_cpu))
}

plot_loglikelihood <- function(tme) {
  # Log-likelihood vs Iteration number
  p1 <- ggplot(data = data.frame(x = 1:length(tme$ll_t), ll_t = tme$ll_t), aes(x = x, y = ll_t)) +
    geom_line(linetype = "dotted", color = "blue", size = 1.5) +
    labs(x = "Iteration number", y = "Log-likelihood") +
    theme_minimal()
  
  # Log-likelihood vs CPU time
  p2 <- ggplot(data = data.frame(T = tme$T, ll_t = tme$ll_t), aes(x = T, y = ll_t)) +
    geom_line(linetype = "dotted", color = "blue", size = 1.5) +
    labs(x = "CPU time", y = "Log-likelihood") +
    theme_minimal()
  
  # Arrange the two plots side by side
  grid.arrange(p1, p2, ncol = 2)
}

simu_1 <- function(y, es2, initialization) {
  opts <- list(maxit = 100, disp_it = 0, tol = 1e-8)
  time_start <- proc.time()
  
  meini <- Metaini(y, es2, initialization, opts, 21)
  
  mep <- tMeta(y, es2, opts, meini)
  
  t_cpu <- proc.time() - time_start
  
  tme <- list(mu = mep$mu, s2 = mep$s2, nu = ifelse(mep$nu == 1e8, Inf, mep$nu), ll_t = opts$errlog, logL = opts$logL, 
              tau = mep$tau, vtau = sort(mep$tau), IDtau = order(mep$tau), iternum = opts$itnum, T = opts$time.it)
  
  AIC <- 2 * 3 - 2 * tme$logL
  BIC <- log(length(y)) * 3 - 2 * tme$logL
  tme$AIC <- AIC
  tme$BIC <- BIC
  tme$T <- cumsum(opts$time.it)
  
  return(list(tme = tme, t_cpu = t_cpu, opts = opts))
}

tMeta <- function(y, es2, opts, me) { }

Metaini <- function(y, es2, method, opts, sam) {}