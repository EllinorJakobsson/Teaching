library(deSolve)
####Function####
predprey2 <- function(t, y, params) {
  R <- y[1]
  C <- y[2]
  with(as.list(params), {
    dR1.dt <- r * R - a * R * C
    dC.dt <- e * a * R * C - d * C
    return(list(c(dR.dt, dC1.dt, dC2.dt)))
  })
}

#Initial parameters
r <- 2 #
a <- 0.5 #
e <- 0.5 #
d <- 0.35 #

####First run####
#Initial population of resource and consumer
R0 <- 3
C0 <- 6
#Add parameters
params1 <- c(r = r, a = a, e = e, d = d)
MaxTime <- 60 # length of the time series, can be changed at any time
Time <- seq(0, MaxTime, by = 0.1) # here we produce a sequence of time points at which we will determine the population densities
LV.out <- ode(c(R0, C0), Time, predpreyLV, params1) # here we call the function "ode" that does the hard work of numerical integration and which is part of the package "deSolve"
matplot(Time, (LV.out[, 2:3]), type = "l", lty = 1:2, col=1, ylab = "Population Density", xlab="Time", ylim = c(0,15))
legend("top", c(expression("resource"), expression("consumer")), lty = 1:2, bty = "n", horiz = TRUE)


####PLOT WITH GGOPLOT####
library(ggplot2)
library(tidyr)
#Reshape to long format as it's more tidy
LV.out <- as.data.frame(LV.out)
LV.out$time <- as.numeric(LV.out$time)
LV.out <- gather(LV.out, key = "Trophic level", value = "Population_size", -time)
ggplot(data = LV.out, aes(x = time, y = Population_size, linetype = `Trophic level`)) +
  geom_line() +
  theme_bw() + 
  labs(x = "Time", y = "Population") +
  scale_linetype_discrete(labels=c('Prey', 'Predator'))

