new <- read.csv("/Users/ctor/Development/upc/PE/proto/100U50000.csv", header=TRUE)
attach(new)

#definir models

diff <-  time_list - time_array
model <- lm(diff ~ length)

model_array <- lm(time_array ~ length)
model_list <- lm(time_list ~ length)

#summary models i intervals de confianca

summary(model_array)
summary(model_list)
summary(model)

confint(model_array)
confint(model_list)
confint(model)

#mitjanes i desviació

mean(time_array)
mean(time_list)
mean(diff)

sd(time_array)
sd(time_list)
sd(diff)

#-------------gràfics------------------

#--linealitat
#diferència

plot(length, diff, main = "Difèrencia de temps", xaxt = "n",
     xlab = "mida", ylab = "temps llista - temps vector" , pch = 19, col = "blue")
abline(model, col = "red", lwd = 2)
abline(h = 0, col = "green", lwd = 2, lty = 2)

x_ticks <- seq(0, max(length), by = 10000)
axis(1, at = x_ticks, labels = x_ticks, las = 0)

#vector
plot(length, time_array, main = "vector",
     xlab = "mida", ylab = "temps vector", pch = 19, col = "blue")
abline(model_array, col = "red", lwd = 2)

#llista
plot(length, time_list, main = "llista",
     xlab = "mida", ylab = "temps llista", pch = 19, col = "blue")
abline(model_list, col = "red", lwd = 2)


#hoscedasticitat
plot(model$fitted.values, model$residuals,
     main = "Residuals vs Fitted",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 20)
abline(h = 0, col = "red")

#normalitat (qqplot)
residuals <- residuals(model)

qqnorm(residuals, ylab="Standardized residual")
qqline(residuals)

#independència
time <- seq_along(residuals(model))

plot(time, residuals(model),
     xlab = "Observation Order",
     ylab = "Residuals",
     main = "Residuals Over Time",
     type = "o", pch = 20, col = "blue")
abline(h = 0, col = "red", lwd = 2) # Add a horizontal reference line at zero

detach(new)


