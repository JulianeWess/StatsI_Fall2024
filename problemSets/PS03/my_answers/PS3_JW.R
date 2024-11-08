#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

#PS03-JW

setwd("C:/Users/julia/OneDrive/Desktop/R Data")
getwd()

incumbent_subset <- read.csv("incumbents_subset.csv")

install.packages("stargazer")
library(stargazer)

model <- lm(voteshare ~ difflog, data = incumbent_subset)
summary(model)

stargazer(model, type = "latex")

#show the output correctly within the latex, using verbatim{}

#Question 1.2


library(ggplot2)
ggplot(incumbent_subset, aes(x = difflog, y = voteshare)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE, color = "blue") + 
  labs(title = "Scatterplot of Vote Share vs. Difference in Log",
       x = "Difference in Log (difflog)",
       y = "Vote Share (voteshare)") +
  theme_minimal()

#Question 1.3

residuals_model <- residuals(model)
head(residuals_model)

#Question 1.4

#voteshare=β0+ß1xdifflog

#voteshare=0.579031+0.041666×difflog

#Question 2.1

model_presvote <- lm(presvote ~ difflog, data = incumbent_subset)

summary(model_presvote)

stargazer(model_presvote, type = "latex")

#Question 2.2

library(ggplot2)

ggplot(incumbent_subset, aes(x = difflog, y = presvote)) +
  geom_point() +                              # Scatterplot
  geom_smooth(method = "lm", se = FALSE, col = "blue") + 
  labs(x = "Difference in Campaign Spending (difflog)", 
       y = "Vote Share of Presidential Candidate (presvote)", 
       title = "Scatterplot of presvote vs difflog with Regression Line") +
  theme_minimal() 

#Questions 2.3
residuals_presvote <- residuals(model_presvote)

head(residuals_presvote)


#Question 2.4

#presvote=0.507583+0.023837×difflog

#Question 3.1

model_voteshare <- lm(voteshare ~ presvote, data = incumbent_subset)

summary(model_voteshare)

stargazer(model_voteshare, type = "latex")

#Question 3.2
library(ggplot2)

ggplot(incumbent_subset, aes(x = presvote, y = voteshare)) +
  geom_point() +                              
  geom_smooth(method = "lm", se = FALSE, col = "blue") + 
  labs(x = "Vote Share of Presidential Candidate (presvote)", 
       y = "Incumbent's Vote Share (voteshare)", 
       title = "Scatterplot of voteshare vs presvote with Regression Line") +
  theme_minimal()  

#Question 3.3

#voteshare=0.441330+0.388018×presvote

#Question 4.1

residuals_voteshare <- residuals(model_voteshare)

residuals_presvote <- residuals(model_presvote)

residual_model <- lm(residuals_voteshare ~ residuals_presvote)
summary(residual_model)

stargazer(residual_model, type = "latex")

#Question 4.2

library(ggplot2)

residuals_data <- data.frame(
  residuals_voteshare = residuals_voteshare,
  residuals_presvote = residuals_presvote)

ggplot(residuals_data, aes(x = residuals_presvote, y = residuals_voteshare)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE, col = "blue") + 
  labs(x = "Residuals of presvote ~ difflog", 
       y = "Residuals of voteshare ~ difflog", 
       title = "Scatterplot of Residuals with Regression Line") +
  theme_minimal()  

#Question 4.3

#residuals_voteshare=−5.037×10−18 −0.1311×residuals_presvote

#Question 5.1

model_voteshare_combined <- lm(voteshare ~ difflog + presvote, data = incumbent_subset)

summary(model_voteshare_combined)

stargazer(model_voteshare_combined, type = "latex")



#Question 2.4

#presvote=0.507583+0.023837×difflog

#Question 3.1

model_voteshare <- lm(voteshare ~ presvote, data = incumbent_subset)

summary(model_voteshare)

stargazer(model_voteshare, type = "latex")

#Question 3.2
library(ggplot2)

ggplot(incumbent_subset, aes(x = presvote, y = voteshare)) +
  geom_point() +                              
  geom_smooth(method = "lm", se = FALSE, col = "blue") + 
  labs(x = "Vote Share of Presidential Candidate (presvote)", 
       y = "Incumbent's Vote Share (voteshare)", 
       title = "Scatterplot of voteshare vs presvote with Regression Line") +
  theme_minimal()  

#Question 3.3

#voteshare=0.441330+0.388018×presvote

#Question 4.1

model_voteshare_difflog <- lm(voteshare ~ difflog, data = incumbent_subset)
model_presvote_difflog <- lm(presvote ~ difflog, data = incumbent_subset)
residuals_voteshare <- residuals(model_voteshare_difflog)
residuals_presvote <- residuals(model_presvote_difflog)
residual_model <- lm(residuals_voteshare ~ residuals_presvote)
summary(residual_model)

stargazer(residual_model, type = "latex")

#Question 4.2

library(ggplot2)

residuals_data <- data.frame(
  residuals_voteshare = residuals_voteshare,
  residuals_presvote = residuals_presvote)

ggplot(residuals_data, aes(x = residuals_presvote, y = residuals_voteshare)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE, col = "blue") + 
  labs(x = "Residuals of presvote ~ difflog", 
       y = "Residuals of voteshare ~ difflog", 
       title = "Scatterplot of Residuals with Regression Line") +
  theme_minimal()  

#Question 4.3

#residuals_voteshare=−5.037×10−18 −0.1311×residuals_presvote

#Question 5.1

model_voteshare_combined <- lm(voteshare ~ difflog + presvote, data = incumbent_subset)

summary(model_voteshare_combined)

stargazer(model_voteshare_combined, type = "latex")

#Question 5.2

#voteshare=0.4486442+0.0355431×difflog+0.2568770×presvote

#Question 5.3




