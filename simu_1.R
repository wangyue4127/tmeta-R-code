simu_1 <- function(y, es2, ini) {
  disp_it <- 0
  num <- 3
  Ndata <- length(y)
  
  opts <- list(maxit = 100, disp_it = disp_it, tol = 1e-8)
  
  start_time <- Sys.time()
  
  source("metaini.R")
  init_result <- metaini(y, es2, ini, opts, 21)
  meini <- init_result$met
  opts <- init_result$opts
  
  source("tmeta.R")
  meta_result <- tmeta(y, es2, opts, meini)
  mep <- meta_result$mep
  opts <- meta_result$opts
  
  t_cpu <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  
  tme <- list(
    mu = mep$mu,
    s2 = mep$s2,
    nu = ifelse(mep$nu == 1e8, Inf, mep$nu),
    ll_t = opts$errlog,
    logL = opts$logL, tau = mep$tau
  )
  sorted_tau <- sort(tme$tau)
  tme$vtau <- sorted_tau
  tme$IDtau <- order(tme$tau)
  
  tme$iternum <- opts$itnum
  tme$T <- opts$time.ini + opts$time.preit + cumsum(opts$time.it)
  tme$Tend <- tail(tme$T, 1)
  tme$AIC <- 2 * num - 2 * tme$logL
  tme$BIC <- log(Ndata) * num - 2 * tme$logL
  
  return(list(tme = tme, t_cpu = t_cpu, opts = opts))
}