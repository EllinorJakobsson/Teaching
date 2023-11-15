library(deSolve)
library(ggplot2)
#From Abrams 1993. Effect of Increased Productivity on the Abundances of Trophic Levels.
#The American Naturalist, Vol. 141, No. 3. (Mar., 1993), pp. 351-371
#URL: http://links.jstor.org/sici?sici=0003-0147%28199303%29141%3A3%3C351%3AEOIPOT%3E2.0.CO%3B2-%23
#Model based on figure 2.3.

####FUNCTION####
predprey.log.prey <- function(t, y, params) {
  R1 <- y[1]
  R2 <- y[2]
  P <- y[3]
  with(as.list(params), {
    dR1.dt <- r * R1 * (1 - R1 / k) - a1 * R1 * P
    dR2.dt <- r * R2 * (1 - R2 / L)  - a2 * R2 * P
    dP.dt <- e1 * a1 * R1 * P + e2 * a2 * R2 * P - d * P
    return(list(c(dR1.dt, dR2.dt, dP.dt)))
  })
}
r <- 2 #Intrinsic growth rate of resource (logistic)
e1 <- 0.2 #Efficacy of take-up from resource 1 to predator
e2 <- 0.2 #Efficacy of take-up from resource 2 to predator
a1 <- 0.2 #Attack rate of predator on resource 1
a2 <- 0.2 #Attack rate of predator on resource 2
d <- 0.4 #Death rate of predator
k <- 50 #Carrying capacity of resource 1
L <- 60 #Carrying capacity of resource 2

R10 <- 3 #Initial resource 1 population
R20 <- 2 #Initial resource 2 population
P0 <- 3 #Initial predator population
params.log.prey1 <- c(r = r, e1 = e1, e2 = e2, a1 = a1, a2 = a2, d = d, k = k, L = L)
MaxTime <- 50
Time <- seq(0, MaxTime, by = 0.5)
log.prey.out <- ode(c(R10, R20, P0), Time, predprey.log.prey, params.log.prey1)

####PLOTTNG####
matplot(Time, (log.prey.out[, 2:4]), type = "l", lty = 2:4, col=1, ylab = "Population Size", xlab="Time", ylim = c(0,30))
legend("top", c(expression("resource1"), expression("resource2"), expression("predator")), lty = 2:4, bty = "n")

####PLOT WITH GGOPLOT####
library(ggplot2)
library(tidyr)
#Reshape to long format
log.prey.out <- as.data.frame(log.prey.out)
log.prey.out$time <- as.numeric(log.prey.out$time)
log.prey.out <- gather(log.prey.out, key = "Trophic level", value = "Population_size", -time)
#Plot
ggplot(data = log.prey.out, aes(x = time, y = Population_size, linetype = `Trophic level`)) +
  geom_line() +
  theme_bw() + 
  labs(x = "Time", y = "Population") +
  scale_linetype_discrete(labels=c('Resource 1', 'Resource 2', 'Predator'))
