library(deSolve)
predprey.log.prey <- function(t, y, params) {
  R <- y[1]
  C <- y[2]
  P <- y[3]
  with(as.list(params), {
    dR.dt <- r * R * (1 - R / k) - a1 * R * C
    dC.dt <- e1 * a1 * R * C - P * C * a2
    dP.dt <- e2 * a2 * C * P - d * P
    return(list(c(dR.dt, dC.dt, dP.dt)))
  })
}
r <- 2
e1 <- 0.3
e2 <- 0.3
a1 <- 0.3
a2 <- 0.3
d <- 0.4
k <- 50

R0 <- 3
C0 <- 2
P0 <- 3
params.log.prey1 <- c(r = r, e1 = e1, e2 = e2, a1 = a1, a2 = a2, d = d, k = k)
MaxTime <- 50
Time <- seq(0, MaxTime, by = 0.5)
log.prey.out <- ode(c(R0, C0, P0), Time, predprey.log.prey, params.log.prey1)

matplot(Time, (log.prey.out[, 2:4]), type = "l", lty = 2:4, col=1, ylab = "Population Size", xlab="Time", ylim = c(0,30))
legend("top", c(expression("resource1"), expression("resource2"), expression("predator")), lty = 2:4, bty = "n")

####PLOT WITH GGOPLOT####
library(ggplot2)
library(tidyr)
#Reshape to long format as it's more tidy
log.prey.out <- as.data.frame(log.prey.out)
log.prey.out$time <- as.numeric(log.prey.out$time)
log.prey.out <- gather(log.prey.out, key = "Trophic level", value = "Population_size", -time)
ggplot(data = log.prey.out, aes(x = time, y = Population_size, linetype = `Trophic level`)) +
  geom_line() +
  theme_bw() + 
  labs(x = "Time", y = "Population") +
  scale_linetype_discrete(labels=c('Prey', 'Consumer', 'Predator'))




