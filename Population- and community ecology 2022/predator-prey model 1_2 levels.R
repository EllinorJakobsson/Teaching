library(deSolve)
predpreyLV <- function(t, y, params) {
  R <- y[1]
  C <- y[2]
  with(as.list(params), {
    dR.dt <- r * R * (1 - R / k) - a * R * C
    dC.dt <- e * a * R * C - d * C
    return(list(c(dR.dt, dC.dt)))
  })
}
r <- 2
a <- 0.2
e <- 0.2
d <- 0.4
k <- 50

R0 <- 2
C0 <- 6
params1 <- c(r = r, a = a, e = e, d = d, k = k)
MaxTime <- 50 # length of the time series, can be changed at any time
Time <- seq(0, MaxTime, by = 0.1) # here we produce a sequence of time points at which we will determine the population densities
LV.out <- ode(c(R0, C0), Time, predpreyLV, params1) # here we call the function "ode" that does the hard work of numerical integration and which is part of the package "deSolve"

matplot(Time, (LV.out[, 2:3]), type = "l", lty = 1:2, col=1, ylab = "Population Density", xlab="Time")
legend("top", c(expression("resource"), expression("consumer")), lty = 1:2, bty = "n")

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

# ####Phase plane diagram####
# plot(LV.out[, 2], LV.out[, 3], type = "l",  lty = 1, col = "red", ylab = "consumer", xlab = "resource")
# points(R0, C0, cex = 1.5, pch = 19, col = "red") # adds a dot to show the starting density
# lines(LV.out2[, 2], LV.out2[, 3], lty = 1, col = "blue") # adds the second simulation to the plot
# points(R02, C02, cex = 1.5 ,pch = 19, col = "blue") # adds a dot to show the starting density
# lines(LV.out3[, 2], LV.out3[, 3], lty = 1, col = "green") # adds the third simulation to the plot
# points(R03, C03, cex = 1.5, pch = 19, col = "green") # adds a dot to show the starting density
# legend("topright", c(expression("simulation 1"), expression("simulation 2"), expression("simulation 3")), lty = 1, col = c("red","blue","green"), bty = "n")
# abline(h = r/a, lty = 2) # iso-cline for the resource, which is a horizontal line
# abline(v = d/(e * a), lty = 2) # iso-cline for the consumer, which is a vertical line
