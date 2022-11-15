library(deSolve)
predprey.log.prey <- function(t, y, params) {
  R <- y[1]
  C <- y[2]
  P <- y[3]
  with(as.list(params), {
    dR.dt <- r * R * (1 - R / k) - a * R * C
    dC.dt <- e * a * R * C - b * C * P
    dP.dt <- g * b * C * P - d * P
    return(list(c(dR.dt, dC.dt, dP.dt)))
  })
}
r <- 2
e <- 0.5
g <- 0.5
a <- 0.5
b <- 0.5
d <- 0.5
k <- 10

R0 <- 2
C0 <- 6
P0 <- 3
params.log.prey1 <- c(r = r, e = e, g = g, a = a, b = b, d = d, k = k)
MaxTime <- 20
Time <- seq(0, MaxTime, by = 0.5)
log.prey.out <- ode(c(R0, C0, P0), Time, predprey.log.prey, params.log.prey1)


####PLOT WITH MATPLOT####
#Choose correct columns, column 1 is time and the rest are species
matplot(Time, (log.prey.out[, 2:4]), type = "l", lty = 1:3, col=1, ylab = "Population Size", xlab="Time", ylim = c(0,10))
legend("top", c(expression("resource"), expression("consumer"), expression("predator")), lty = 1:3, bty = "n")


####PLOT WITH GGOPLOT####
library(ggplot2)
library(tidyr)
#Reshape to long format as it's more tidy
log.prey.out <- as.data.frame(log.prey.out)
log.prey.out$time <- as.numeric(log.prey.out$time)
log.prey.out <- gather(log.prey.out, key = "Trophic_level", value = "Population_size", -time)

ggplot(data = log.prey.out, aes(x = time, y = Population_size, linetype = Trophic_level)) +
  geom_line() +
  theme_bw() + 
  labs(x = "Time", y = "Population") +
  scale_linetype_discrete(labels=c('Prey', 'Predator 1', 'Predator 2'))
