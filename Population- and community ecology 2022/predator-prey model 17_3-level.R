predprey.log.prey <- function(t, y, params) {
  R <- y[1]
  C1 <- y[2]
  C2 <- y[3]
  P <- y[4]
  
  with(as.list(params), {
    dR.dt <- r * R * (1 - R / k) - a1 * R * C1 - a2 * R * C2
    dC1.dt <- e1 * a1 * R * C1 - a3 * C1 * P
    dC2.dt <- e2 * a2 * R * C2 - d1 * C2
    dP.dt <-  e3 * a3 * C1 * P- d2 * P
    return(list(c(dR.dt, dC1.dt, dC2.dt, dP.dt)))
  })
}
r <- 2
a1 <- 0.2
a2 <- 0.2
a3 <- 0.5
e1 <- 0.5
e2 <- 0.5
e3 <- 0.5
d1 <- 0.5
d2 <- 0.5
k <- 50

R0 <- 2
C10 <- 6
C20 <- 5
P0 <- 3
params.log.prey1 <- c(r = r, e1 = e1, e2 = e2, e3 = e3, a1 = a1, a2 = a2, a3 = a3, d1 = d1, d2 = d2, k = k)
MaxTime <- 2000
Time <- seq(0, MaxTime, by = 0.5)
log.prey.out <- ode(c(R0, C10, C20, P0), Time, predprey.log.prey, params.log.prey1)

matplot(Time, (log.prey.out[, 2:5]), type = "l", lty = 2:5, col=1, ylab = "Population Size", xlab="Time", ylim = c(0,10))
legend("top", c(expression("resource"), expression("consumer one"), expression("consumer two"), expression("predator")), lty = 2:5, bty = "n")