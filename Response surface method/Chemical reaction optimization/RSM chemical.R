#Response surface methodology for 2 variables

CCD1 <- read.csv("1 order data.csv", sep = ";", dec = ",")
CCD1

install.packages("rsm")
library(rsm)

#Relationship between coded and natural variables
CCD1 <- as.coded.data(CCD1,
                      x1 ~ (time-35)/5,
                      x2 ~ (temperature-155)/5)

str(CCD1)

#Regression model or Yeld (y)
model_Y1 <- rsm(response ~ SO(x1, x2), data = CCD1)
model_Y1
summary(model_Y1)

#OR
#FO: First order model; TWI: Two-way interaction; PQ: Pure quadratic 

model_Y2 <- rsm(response ~ FO(x1, x2) + TWI(x1, x2) + PQ(x1, x2), data = CCD1) 
model_Y2
summary(model_Y2)

#The first trial experiment did not show a quadratic relationship, meaning that is far from the optmium point.
#A serie of experiments, based on this first trial will lead to the optimum area, based at the maximum yeld
#varying the x1 unit and the x2 unit based on the slope of the x1,x2 coeficient 0.325/0.775 = 0.42

library(ggplot2)
library(dplyr)

steepascent <- read.csv("steepest ascent experiment.csv", sep = ";", dec = ",")
str(steepascent)

plot(steepascent$stepes, steepascent$response)

ggplot(data = steepascent,output.type = "expression", mapping = aes(x = stepes, y = response)) +
  geom_point() + 
  scale_y_continuous(expand = c(0,0), limits = c(40, 85)) +
  scale_x_continuous(expand = c(0,0), limits = c(1, 13),breaks = seq(1, 12, by = 1)) +
  geom_line() +
  theme_classic()

#Increases in response are observed through the tenth step, however all steps beyond this point 
# result in a decrease in yield. Therefore, another first-order model should be fit in
# the general vicinity of the step 10 (x1= 85, x2= 175).

#The procedure is repeated again, using the step 10 as center point (x1= 85, x2=175)
#The region of exploration is x1=[80, 90], x2=[170, 180]
#This will allow to find the max value using the quadratic therm
#The first-order model fit to the coded variables: y = 79.94 + 1.00x1 + 0.50x2

CCD2 <- read.csv("2 order data.csv", sep = ";", dec = ",")

#Relationship between coded and natural variables
CCD2 <- as.coded.data(CCD2,
                      x1 ~ (time-85)/5,
                      x2 ~ (temperature-175)/5)

str(CCD2)

#Regression model or Yeld (y)
optimum_model <- rsm(response ~ SO(x1, x2), data = CCD2)
optimum_model
summary(optimum_model)

#OR
#FO: First order model; TWI: Two-way interaction; PQ: Pure quadratic 

optimum_model <- rsm(response ~ FO(x1, x2) + TWI(x1, x2) + PQ(x1,x2), data = CCD2) 
optimum_model
summary(optimum_model)

#Countour/perspective plots
par(mfrow = c(1,1))
contour(optimum_model, ~ x1+x2,
        image = TRUE,
        yaxp = c(180,170,2),
        xlabs = c("Time (min)", "Temperature (°C)"))

persp(optimum_model, x1~x2, col = terrain.colors(12), contours = "colors",
      zlab = "Yeld (%)", xlabs = c("Time (min)", "Temperature (°C)"))



