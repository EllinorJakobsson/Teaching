library(deSolve)
library(ggplot2)
#From Abrams 1993. Effect of Increased Productivity on the Abundances of Trophic Levels.
#The American Naturalist, Vol. 141, No. 3. (Mar., 1993), pp. 351-371
#URL: http://links.jstor.org/sici?sici=0003-0147%28199303%29141%3A3%3C351%3AEOIPOT%3E2.0.CO%3B2-%23
#Model based on figure 2.21.

####FUNCTION####
predprey.log.prey <- function(t, y, params) {
  R <- y[1]
  C1 <- y[2]
  C2 <- y[3]
  P <- y[4]
  
  with(as.list(params), {
    dR.dt <- r * R * (1 - R / k) - a1 * R * C1 - a2 * R * C2
    dC1.dt <- e1 * a1 * R * C1 - a3 * C1 * P
    dC2.dt <- e2 * a2 * R * C2 - a4 * C2 * P
    dP.dt <-  e3 * a3 * C1 * P + e4 * a4 * C2 * P - d * P
    return(list(c(dR.dt, dC1.dt, dC2.dt, dP.dt)))
  })
}

r <- 2 #Intrinsic growth of resource (logistic)
e1 <- 0.5 #Efficacy of take-up from resource to consumer 1
e2 <- 0.5 #Efficacy of take-up from resource to consumer 2
e3 <- 0.2 #Efficacy of take-up from consumer 1 to predator
e4 <- 0.2 #Efficacy of take-up from consumer 2 to predator

a1 <- 0.5 #Attack rate of consumer 1 on resource
a2 <- 0.5 #Attack rate of consumer 2 on resource
a3 <- 0.2 #Attack rate of predator on consumer 1
a4 <- 0.2 #Attack rate of predator on consumer 2

d <- 0.4 #Death rate (of predator)
k <- 20 #Carrying capacity of resource

R0 <- 2 #Initial resource population
C10 <- 6 #Initial consumer 1 population
C20 <- 5 #Initial consumer 2 population
P0 <- 18 #Initial predator population
params.log.prey1 <- c(r = r, e1 = e1, e2 = e2, e3 = e3, e4 = e4, a1 = a1, a2 = a2, a3 = a3, a4 = a4, d = d, k = k)
MaxTime <- 50
Time <- seq(0, MaxTime, by = 0.5)
log.prey.out <- ode(c(R0, C10, C20, P0), Time, predprey.log.prey, params.log.prey1)

####PLOTTING####
matplot(Time, (log.prey.out[, 2:5]), type = "l", lty = 2:5, col = c(1, 2, 3, 4), ylab = "Population Size", xlab="Time", ylim = c(0,10))
legend("top", c(expression("resource"), expression("consumer one"), expression("consumer two"), expression("predator")), lty = 2:5, bty = "n")

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
  scale_linetype_discrete(labels=c('Resource', 'Consumer 1','Consumer 2', 'Predator'))

