
# SCript for running a simple multiple regression
library(readxl)
wd <- ""
setwd(wd)
data <- read_xlsx("datafile.xlsx")

# Write model
model <- lm(scale(response)~(scale(explanatory)) + (scale(explanatory)), data=data)

# Check assumption of normality
aov_residuals <- residuals(object = model)
shapiro.test(x = aov_residuals)
summary(model)

# Plot the data
ggplot(data, aes(x=explanatory, y=response)) + geom_point() + theme_bw()
ggplot(data, aes(x=explanatory, y=response)) + geom_point() + theme_bw()

