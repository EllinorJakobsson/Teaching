
library(readxl)
wd <- ""
setwd(wd)
data <- read_xlsx("datafile.xlsx")
model <- lm(scale(response)~(scale(explanatory)) + (scale(explanatory)), data=data)

#Normailty
aov_residuals <- residuals(object = model)
shapiro.test(x = aov_residuals)
summary(model)

#plotting
ggplot(data, aes(x=explanatory, y=response)) + geom_point() + theme_bw()
ggplot(data, aes(x=explanatory, y=response)) + geom_point() + theme_bw()

