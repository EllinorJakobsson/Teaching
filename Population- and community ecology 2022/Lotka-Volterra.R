
library(deSolve)
####Function####
predpreyLV <- function(t, y, params) {
  R <- y[1]
  C <- y[2]
  with(as.list(params), {
    dR.dt <- r * R - a * R * C
    dC.dt <- e * a * R * C - d * C
    return(list(c(dR.dt, dC.dt)))
  })
}

#Initial parameters
r <- 2 #
a <- 0.5 #
e <- 0.5 #
d <- 0.35 #

####First run####
#Initial population of resource and consumer
R0 <- 2
C0 <- 6
#Add parameters
params1 <- c(r = r, a = a, e = e, d = d)
MaxTime <- 60 # length of the time series, can be changed at any time
Time <- seq(0, MaxTime, by = 0.1) # here we produce a sequence of time points at which we will determine the population densities
LV.out <- ode(c(R0, C0), Time, predpreyLV, params1) # here we call the function "ode" that does the hard work of numerical integration and which is part of the package "deSolve"
matplot(Time, (LV.out[, 2:3]), type = "l", lty = 1:2, col=1, ylab = "Population Density", xlab="Time", ylim = c(0,8))
legend("top", c(expression("resource"), expression("consumer")), lty = 1:2, bty = "n", horiz = TRUE)

####Changing starting densities####
#Initial population of resource and consumer
R02 <- 2
C02 <- 5
LV.out2 <- ode(c(R02, C02), Time, predpreyLV, params1)
matplot(Time, (LV.out2[, 2:3]), type = "l", lty = 1:2, col=1, ylab = "Population Density", xlab="Time", ylim = c(0,8))
legend("top", c(expression("resource"), expression("consumer")), lty = 1:2, bty = "n", horiz = TRUE)
R03 <- 2
C03 <- 4
LV.out3 <- ode(c(R03, C03), Time, predpreyLV, params1)
matplot(Time, (LV.out3[, 2:3]), type = "l", lty = 1:2, col=1, ylab = "Population Density", xlab="Time", ylim = c(0,8))
legend("top", c(expression("resource"), expression("consumer")), lty = 1:2, bty = "n", horiz = TRUE)



####Phase plane diagram####
plot(LV.out[, 2], LV.out[, 3], type = "l",  lty = 1, col = "red", ylab = "consumer", xlab = "resource")
points(R0, C0, cex = 1.5, pch = 19, col = "red") # adds a dot to show the starting density
lines(LV.out2[, 2], LV.out2[, 3], lty = 1, col = "blue") # adds the second simulation to the plot
points(R02, C02, cex = 1.5 ,pch = 19, col = "blue") # adds a dot to show the starting density
lines(LV.out3[, 2], LV.out3[, 3], lty = 1, col = "green") # adds the third simulation to the plot
points(R03, C03, cex = 1.5, pch = 19, col = "green") # adds a dot to show the starting density
legend("topright", c(expression("simulation 1"), expression("simulation 2"), expression("simulation 3")), lty = 1, col = c("red","blue","green"), bty = "n")
abline(h = r/a, lty = 2) # iso-cline for the resource, which is a horizontal line
abline(v = d/(e * a), lty = 2) # iso-cline for the consumer, which is a vertical line




