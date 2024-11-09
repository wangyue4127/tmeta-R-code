# initialization of parameters

metaini <- function(y, es2, method, opts, sam) {
  set.seed(sam)
  start_time <- Sys.time()
  
  met <- list()
  
  switch(method,
         'sample' = {
           met$mu <- mean(y)
           met$s2 <- var(y)
           met$nu <- 2.1
           met$a <- 1.1
           met$tau <- (met$nu + 1) / (met$nu + sum((y - met$mu)^2 / (met$s2 + es2)))
         },
         'rand' = {
           met$mu <- runif(1)
           met$s2 <- runif(1)
           met$nu <- 4
         },
         'randn' = {
           met$mu <- rnorm(1)
           met$s2 <- abs(rnorm(1))
           met$nu <- 2.1
         }
  )
  
  end_time <- Sys.time()
  opts$time.ini <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  return(list(met = met, opts = opts))
}