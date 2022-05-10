## Linear regression ##

# Step 1: load packages

if(!require(pacman)) install.packages("pacman")
library(pacman)

pacman::p_load(dplyr, ggplot2, car, rstatix, lmtest, ggpubr)

# Step 2: Load data bank
# Select (working directory) Session > Set Working Directory > Choose Directory

data <- read.csv2("linear_regression_data.csv", stringsAsFactors = T) 

# Data visualization as separeted window
View(data)                 

# Data visualization as summary

glimpse(data)                             

# Step 3: Checking assumpstions for linear regression

# 1° linear relationship between dependent variable (DV) e independent variable (IV)
# 2° Normality of residues
# 3° Residues independency
# 4° Homocedasticity

plot(data$Publicity, data$Sales)

## Model visualization
model <- lm(Sales ~ Publicity, data)

## Diagnostic plots

# 1° Use to plot the 4 graph at the same time
par(mfrow=c(2,2))

plot(model)

### Interpretação: https://data.library.virginia.edu/diagnostic-plots/

par(mfrow=c(1,1))

## 2° Residues normality:
shapiro.test(model$residuals)

## Residues outliers:
#rstandard(model) display the whole residues.
rstandard(model)
#summary(rstandard(model)) display a summary of residues
summary(rstandard(model))

## 3°  Residues independence (Durbin-Watson):
durbinWatsonTest(model)

## Homocedasticity Brow-Forsythe
#aparentemente n funciona p duas variaveis continuas
install.packages("onewaytests")
library(onewaytests)
bf.test(Publicity ~ Sales, data = data)

## 4° Homocedasticidade (Breusch-Pagan):
bptest(model)

# Step 4: Model analysis
summary(model)

# Step 5° Regression graph and line
# The line equation from ggpubr rounds the values (125,2 to 130)
ggplot(data = data,output.type = "expression", mapping = aes(x = Publicity, y = Sales)) +
  geom_point() +
  geom_smooth(method = "lm", col = "orange") +
  stat_regline_equation(aes(label = paste(..eq.label.., ..adj.rr.label..,
  sep = "*plain(\",\")~~")),label.x = 0, label.y = 350) +
  theme_classic()


# Step 5° Regression graph and line using ggpmisc
  
pacman::p_load(ggpmisc)
ggplot(data = data, mapping = aes(x = Publicity, y = Sales)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  stat_cor(aes(label = paste(..rr.label..,..p.label.., sep = "~`,`~")), label.x = 3, label.y = 400) +
  stat_poly_eq(aes(label = paste(..eq.label.., sep = "~~~")), parse = TRUE, coef.digits = 4,
  f.digits = 4, p.digits = 3, label.y = 0.9, label.x = 0.05) +
  theme_classic() 


