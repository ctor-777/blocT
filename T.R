test <- read.csv("/Users/ctor/Development/upc/PE/proto/N10000.csv", header=TRUE)
testr <- read.csv("/Users/ctor/Development/upc/PE/proto/random_len.csv", header=TRUE)
new <- read.csv("/Users/ctor/Development/upc/PE/proto/100U50000.csv", header=TRUE)
mean(test$time_array)
sd(test$time_array)
mean(test$time_list)
sd(test$time_list)
t.test(test$time_array, test$time_list)

mean(testr$time_array)
mean(testr$time_list)
t.test(testr$time_array, testr$time_list)

split <- split(test, cut(test$array_first, c(0,0.5,1), include.lowest=TRUE))
t.test(split$`(0.5,1]`$time_array, split$`[0,0.5]`$time_array)
t.test(split$`(0.5,1]`$time_list, split$`[0,0.5]`$time_list)

splitr <- split(testr, cut(testr$array_first, c(0,0.5,1), include.lowest=TRUE))
t.test(splitr$`(0.5,1]`$time_array, splitr$`[0,0.5]`$time_array)
t.test(splitr$`(0.5,1]`$time_list, splitr$`[0,0.5]`$time_list)

splitr <- split(testr, cut(testr$length, c(0,50000,100000), include.lowest=TRUE))
t.test(splitr$`[0,5e+04]`$time_array, splitr$`[0,5e+04]`$time_list)
t.test(split$`(0.5,1]`$time_array, splitr$`(5e+04,1e+05]`$time_list)

better_table <- split(test, cut(test$time_list, 12))
table(better_table)
cut_list <- cut(testr$time_list, 10)
plot(table(cut_list))
cut_array <- cut(testr$time_array, 10)
plot(table(cut_array))
plot(better_table, better_table)

plot(testr$length ,testr$time_list)
table(test$time_list)
barplot(test$time_list)
mean(test$time_list)

qqnorm(testr$time_list/testr$length)
qqline(testr$time_list/testr$length)

qqnorm(testr$time_array/testr$length)
qqline(testr$time_array/testr$length)


max(testr$time_array/testr$length)

testr$time_array/testr$length

ktestr <- testr[-c(69),]

tt <- log(testr$time_list, base = exp(1))-log(testr$time_array, base = exp(1))
log_tt <- log(tt, base = exp(1))
mod <- lm(tt~1)
mod
boxplot(tt)
boxplot(log(testr$time_list, base = exp(1)),log(testr$time_array, base = exp(1)))
boxplot(testr$time_list, testr$time_array)
summary(mod)
barplot(tt)
plot(tt)
mean(tt)
plot(table(cut(tt, 10)))


qqnorm(tt)
qqline(tt)

plot(testr$time_array, testr$time_list)
plot(log(testr$time_array, base = exp(1)), log(testr$time_list, base = exp(1)))

     

plot(testr$time_array/testr$length)
kmeans(testr$time_array/testr$length, 3)

library(ggplot2)
p1 <- ggplot(data = data.frame(testr$time_array, testr$time_list), aes(x = testr$time_array, y = testr$time_list)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(x = "y1", y = "y2")

p1

summary(lm(testr$time_array~testr$length))
summary(lm(testr$length~testr$time_array))
diff <-  testr$time_list - testr$time_array

mean(testr$time_array)
mean(testr$time_list)
mean(testr$time_list - testr$time_array)

summary(lm(testr$length ~ testr$time_array + testr$time_list))

summary(lm(testr$length~diff))
testr$time_array-testr$time_list
testr$length

diff_model <- lm(testr$length~diff)
summary(diff_model)

predict(diff_model)[1]

data_500 <- read.csv("/Users/ctor/Development/upc/PE/proto/data_500.csv", header = TRUE)

attach(data_500)


diff <-  time_list - time_array
modelB <- lm(diff ~ length)

summary(model)

detach(data_500)

u_5 <- subset(testr, length < 50000)


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

#mitjanes i desviació

mean(time_array)
mean(time_list)
mean(diff)

sd(time_array)
sd(time_list)
sd(diff)



model_test <- lm(diff ~ length + array_first)
summary(model_test)

#-------------plots------------------

plot(length, diff, main = "Difèrencia de temps", xaxt = "n",
     xlab = "mida", ylab = "temps llista - temps vector" , pch = 19, col = "blue")
abline(model, col = "red", lwd = 2)
abline(h = 0, col = "green", lwd = 2, lty = 2)

x_ticks <- seq(0, max(length), by = 10000)
axis(1, at = x_ticks, labels = x_ticks, las = 0)

plot(length, time_array, main = "vector",
     xlab = "mida", ylab = "temps vector", pch = 19, col = "blue")
abline(model_array, col = "red", lwd = 2)

plot(length, time_list, main = "llista",
     xlab = "mida", ylab = "temps llista", pch = 19, col = "blue")
abline(model_list, col = "red", lwd = 2)

predict(model)

plot(model$fitted.values, model$residuals,
     main = "Residuals vs Fitted",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 20)
abline(h = 0, col = "red")

residuals <- residuals(model)

qqnorm(residuals, ylab="Standardized residual")
qqline(residuals)

time <- seq_along(residuals(model))

# test de independencia
plot(time, residuals(model),
     xlab = "Observation Order",
     ylab = "Residuals",
     main = "Residuals Over Time",
     type = "o", pch = 20, col = "blue")
abline(h = 0, col = "red", lwd = 2) # Add a horizontal reference line at zero

detach(data_500)


