#load the packages 

if(!require(pacman)) install.packages("pacman")
library(pacman)

pacman::p_load(dplyr, car, rstatix, lmtest, ggpubr,
               QuantPsyc, psych, scatterplot3d)

data <- read.csv2("data.csv", stringsAsFactors = T) 
View(data)                                 
glimpse(data)
str(data)

plot(data$Tempo_Rev, data$Notas)
plot(data$Tempo_Sono, data$Notas)

#model construction

model <- lm(Notas ~ Tempo_Rev + Tempo_Sono, data)


par(mfrow=c(2,2))
plot(model)

#Regression assumptions

#1°Normality residual test
#Check the Q-Q plot
shapiro.test(model$residuals)

#Residual outliers
#Check the residual vs leverage plot
summary(rstandard(model))

#2°Homocedasticity
#check the scale-location plot 
bptest(model)

#3°Non multicolinearity

# Residuals independency 
durbinWatsonTest(model)

#Multicolinearity
pairs.panels(data)
vif(model)

summary(model)

# Standardized coefficients
install.packages("lm.beta")
library(lm.beta)

lm.beta(model)

#Confidence interval for the coefficients
confint(model)

#Comparing models
model2 <- lm(Notas ~ Tempo_Rev, data)

AIC(model, model2)
BIC(model, model2)

#Comparing nested models; the lower RSS
anova(model, model2)

# Scatter plot 3D
par(mfrow=c(1,1))
graph <- scatterplot3d(data$Notas ~ data$Tempo_Rev + data$Tempo_Sono,
                       pch = 16, angle = 45, color = "steelblue", box = FALSE,
                       xlab="Revision time", ylab="Sleep time", zlab="Score")

graph$plane3d(model, col="black", draw_polygon = TRUE)


#Selectin model

pacman::p_load(MASS)

initial_model <- lm(Notas ~ Tempo_Rev + Tempo_Sono + Tempo_Rev*Tempo_Sono, data)
simple_model <- lm(Notas ~ 1, data)

stepAIC(mod.inicial, scope = list(upper = initial_model,
                                  lower = simple_model), direction = "backward")




