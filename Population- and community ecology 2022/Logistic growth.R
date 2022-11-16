log_growth <- function(t, N, parameters) {
  with(as.list(c(N, parameters)), {
    dN.dt <- r * N * (1 - N /K)
    return(list(dN.dt))  
  })
}

parameters <- c(r = 0.5, K = 500)
N <- 10 # initial population size
t <- seq(1, 15, 0.1)
int_time_series <- ode(y = N, times = t, func = log_growth, parms = parameters)
plot(t, int_time_series[, 2], type = "l", lty = 1:2, col=1, ylab = "population size", xlab="time")