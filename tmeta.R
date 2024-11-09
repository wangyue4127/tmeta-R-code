#   tMeta performs maximum likelihood estimation for t distribution
#
#   mep:   Structure of all parameters in t distribution
#             mep$mu: mean;
#             mep$nu: degree of freedom;
#             mep$tau: Weights;
#             mep$s2: between-study variance;
#
#  opts:    Structure of related settings in t distribution
#             opts$errlog:  a vector recording the log likelihood at each iteration.
#	        opts$disp_it = 1 display error values; logs error;
#	        opts$tol: threshold (relative change in log-likelihood).
#	        opts$maxit: the maximum number of iterations; default 100.
#             opts$logL: final log-likelihood.
#             opts$itnum: number of iterations
#             opts$time: CPU time


library(stats)
tmeta <- function(y, es2, opts, me) {
  start_time <- Sys.time()
  N <- length(y)
  niters <- ifelse("maxit" %in% names(opts), opts$maxit, 100)
  disp_it <- ifelse("disp_it" %in% names(opts), opts$disp_it, 0)
  test <- ifelse("tol" %in% names(opts), TRUE, FALSE)
  store <-2

  if (store>1) {
    opts$errlog <- numeric(niters)
    opts$time.it <- numeric(niters)
  }

  nu_L <- 1
  nu_U <- 1000
  limit <- c(nu_L, nu_U)
  mep <- me
  opts$time.preit <- Sys.time() - start_time
  t2 <- 0
  
  for (n in 1:niters) {
    t1_time_start <- Sys.time()
    opts$itnum <- n
    ycen2 <- (y - mep$mu)^2
    ss <- mep$s2 + es2
    tr <- ycen2 / ss
    
    g1 <- g(limit[1], N, tr)
    g2 <- g(limit[2], N, tr)
    if (g1 * g2 < 0) {
      mep$nu <- uniroot(function(x) g(x, N, tr), interval = limit)$root
    } else if (g1 < 0) {
      mep$nu <- nu_L
    } else {
      mep$nu <- nu_U
    }
    mep$nu <- min(max(mep$nu, nu_L), nu_U)
    if (mep$nu == 1000) {
      mep$nu <- 1e8
    }
    
    if (disp_it > 0 || store || test) {
      l1 <- N * lgamma((mep$nu + 1) / 2) - N * lgamma(mep$nu / 2) - sum(log(ss)) / 2
      l2 <- (N * mep$nu / 2) * log(mep$nu) - ((mep$nu + 1) / 2) * sum(log(mep$nu + tr))
      L <- -N * log(pi) / 2 + l1 + l2
      e <- L
      
      t1_time_end <- Sys.time()
      t1 <- as.numeric(difftime(t1_time_end, t1_time_start, units = "secs"))
      opts$time.it[n] <- t1 + t2
      if (store) {
        opts$errlog[n] <- e
      }
      if (disp_it > 0) {
        if (n > 1) {
          cat(sprintf("Cycle %4d  logL %11.6f, relative increment %e\n", n, e, (e - eold) / abs(eold)))
        } else {
          cat(sprintf("Cycle %4d  logL %11.6f\n", n, e))
        }
        
        if (n > 1 && e < eold) {
          cat(sprintf("----> LogL decreased in iteration %4d\n", n))
        }
      }
      if (test) {
        if (n > 1 && abs(e - eold) / abs(e) < opts$tol) {
          break
        } else {
          eold <- e
        }
      }
    }
    
    t2_time_start <- Sys.time()
    mep$tau <- (mep$nu + 1) / (mep$nu + tr)
    
    mep$mu <- sum((mep$tau * y) / ss) / sum(mep$tau / ss)
    
    ycen2 <- (y - mep$mu)^2
    mep$s2 <- sum((mep$tau * ycen2 - es2) / (ss^2)) / sum(1 / (ss^2))
    mep$s2 <- max(0, mep$s2)
    
    t2 <- as.numeric(difftime(Sys.time(), t2_time_start, units = "secs"))
  }
  
  opts$time.it <- opts$time.it[1:opts$itnum]
  opts$errlog <- opts$errlog[1:opts$itnum]
  opts$logL <- e
  
  return(list(mep = mep, opts = opts))
}

g <- function(x, N, tr) {
  1 - digamma(x / 2) + log(x / 2) + digamma((1 + x) / 2) - log((1 + x) / 2) +
    (sum(log((x + 1) / (x + tr))) - sum((x + 1) / (x + tr))) / N
}