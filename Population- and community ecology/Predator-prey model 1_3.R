library(deSolve)
library(ggplot2)
####META INFO####
#From Abrams 1993. Effect of Increased Productivity on the Abundances of Trophic Levels.
#The American Naturalist, Vol. 141, No. 3. (Mar., 1993), pp. 351-371
#URL: http://links.jstor.org/sici?sici=0003-0147%28199303%29141%3A3%3C351%3AEOIPOT%3E2.0.CO%3B2-%23
#Model based on figure 2.1.

####FUNCTION####
predprey.log.prey <- function(t, y, params) {
  R <- y[1]
  C <- y[2]
  P <- y[3]
  with(as.list(params), {
    dR.dt <- r * R * (1 - R / k) - a1 * R * C
    dC.dt <- e1 * a1 * R * C - P * C * a2 - d1*C
    dP.dt <- e2 * a2 * C * P - d * P
    return(list(c(dR.dt, dC.dt, dP.dt)))
  })
}
r <- 2 #Intrinsic growth of resource; the same for both
e1 <- 0.5 #Efficacy of take-up from resource to consumer
e2 <- 0.5 #Efficacy of take-up from consumer to predator
a1 <- 0.5 #Attack rate of consumer on resource
a2 <- 0.5 #Attack rate of predator on consumer
d1 <- 0.005 #Mortality of consumer
d <- 0.5 #Mortality of predator
k <- 10 #Carrying capacity of resource

R0 <- 2 #Initial resource population
C0 <- 6 #Initial consumer population
P0 <- 3 #Initial peradtor population
params.log.prey1 <- c(r = r, e1 = e1, e2 = e2, a1 = a1, a2 = a2, d = d, d1 = d1, k = k) #Specify parameters
MaxTime <- 50 #Set time point to solve until
Time <- seq(0, MaxTime, by = 0.5) #Set time steps
log.prey.out <- ode(c(R0, C0, P0), Time, predprey.log.prey, params.log.prey1) #Solve differential equations using deSolve

####PLOTTING####
matplot(Time, (log.prey.out[, 2:4]), type = "l", lty = 2:4, col=1, ylab = "Population Size", xlab="Time", ylim = c(0,30))
legend("top", c(expression("resource1"), expression("resource2"), expression("predator")), lty = 2:4, bty = "n")

####PLOT WITH GGOPLOT####
library(ggplot2)
library(tidyr)
#Reshape to long format
log.prey.out <- as.data.frame(log.prey.out)
log.prey.out$time <- as.numeric(log.prey.out$time)
log.prey.out <- gather(log.prey.out, key = "Trophic level", value = "Population_size", -time)

ggplot(data = log.prey.out, aes(x = time, y = Population_size, linetype = `Trophic level`)) +
  geom_line() +
  theme_bw() + 
  labs(x = "Time", y = "Population") +
  scale_linetype_discrete(labels=c('Prey', 'Consumer', 'Predator'))




