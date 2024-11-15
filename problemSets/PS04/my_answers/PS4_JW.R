#PS4

setwd("C:/Users/julia/OneDrive/Desktop/R Data")
get()

install.packages(car)
library(car)
data(Prestige)
help(Prestige)

#Exercise 1 a)
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)

Prestige$professional[is.na(Prestige$type)] <- 0

head(Prestige[, c("type", "professional")])

model <- lm(prestige ~ income * professional, data = Prestige)

summary(model)

install.packages("stargazer")