library(deSolve)
library(ggplot2)
#From Abrams 1993. Effect of Increased Productivity on the Abundances of Trophic Levels.
#The American Naturalist, Vol. 141, No. 3. (Mar., 1993), pp. 351-371
#URL: http://links.jstor.org/sici?sici=0003-0147%28199303%29141%3A3%3C351%3AEOIPOT%3E2.0.CO%3B2-%23
#Model based on figure 1.1.

####FUNCTION####
predpreyLV <- function(t, y, params) {
  R <- y[1]
  C <- y[2]
  with(as.list(params), {
    dR.dt <- r * R * (1 - R / k) - a * R * C
    dC.dt <- e * a * R * C - d * C
    return(list(c(dR.dt, dC.dt)))
  })
}
r <- 2 #Intrinsic growth rate of resource (logistic)
a <- 0.2 #Attack rate of predator on resource
e <- 0.2 #Efficacy of take-up from resource to predator
d <- 0.4 #Death rate of predator
k <- 50 #Carrying capacity of resource

R0 <- 2 #Initial resource population
C0 <- 6 #Initial predator population
params1 <- c(r = r, a = a, e = e, d = d, k = k) # specify parameters
MaxTime <- 50 # length of the time series, can be changed at any time
Time <- seq(0, MaxTime, by = 0.1) # here we produce a sequence of time points at which we will determine the population densities
LV.out1 <- ode(c(R0, C0), Time, predpreyLV, params1) # here we call the function "ode" that does the hard work of numerical integration and which is part of the package "deSolve"

####PLOTTING#
matplot(Time, (LV.out[, 2:3]), type = "l", lty = 1:2, col=1, ylab = "Population Density", xlab="Time")
legend("top", c(expression("resource"), expression("consumer")), lty = 1:2, bty = "n")

####PLOT WITH GGOPLOT####
library(ggplot2)
library(tidyr)
#Reshape to long format
LV.out <- as.data.frame(LV.out1)
LV.out$time <- as.numeric(LV.out$time)
LV.out <- gather(LV.out, key = "Trophic level", value = "Population_size", -time)
#Plot
ggplot(data = LV.out, aes(x = time, y = Population_size, linetype = `Trophic level`)) +
  geom_line() +
  theme_bw() + 
  labs(x = "Time", y = "Population") +
  scale_linetype_discrete(labels=c('Resource', 'Predator'))

####PHASE PLANE####
#Simulation 2
R02 <- 4
C02 <- 8
params1 <- c(r = r, a = a, e = e, d = d, k = k)
MaxTime <- 50 # length of the time series, can be changed at any time
Time <- seq(0, MaxTime, by = 0.1) # here we produce a sequence of time points at which we will determine the population densities
LV.out2 <- ode(c(R02, C02), Time, predpreyLV, params1) # here we call the function "ode" that does the hard work of numerical integration and which is part of the package "deSolve"

#Simulation 3
R03 <- 1
C03 <- 3
params1 <- c(r = r, a = a, e = e, d = d, k = k)
MaxTime <- 50 # length of the time series, can be changed at any time
Time <- seq(0, MaxTime, by = 0.1) # here we produce a sequence of time points at which we will determine the population densities
LV.out3 <- ode(c(R03, C03), Time, predpreyLV, params1) # here we call the function "ode" that does the hard work of numerical integration and which is part of the package "deSolve"

#Plotting
plot(LV.out3[, 2], LV.out3[, 3], lty = 1, col = "green", type = "l", ylab = "Consumer", xlab = "Resource") # adds the third simulation to the plot
points(R03, C03, cex = 1.5, pch = 19, col = "green") # adds a dot to show the starting density
lines(LV.out1[, 2], LV.out1[, 3], lty = 1, col = "red")
points(R0, C0, cex = 1.5, pch = 19, col = "red") # adds a dot to show the starting density
lines(LV.out2[, 2], LV.out2[, 3], lty = 1, col = "blue") # adds the second simulation to the plot
points(R02, C02, cex = 1.5 ,pch = 19, col = "blue") # adds a dot to show the starting density
legend("topright", c(expression("simulation 1"), expression("simulation 2"), expression("simulation 3")), lty = 1, col = c("red","blue","green"), bty = "n")
abline(h = r/a, lty = 2) # iso-cline for the resource, which is a horizontal line
abline(v = d/(e * a), lty = 2) # iso-cline for the consumer, which is a vertical line


