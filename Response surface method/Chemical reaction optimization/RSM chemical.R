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

#### 1° step #####
#checking the plausibility of quadratic fitting for the first trial experiment
#Regression model or Yeld (y)
model_fist_trial <- rsm(response ~ FO(x1, x2) + TWI(x1, x2) + PQ(x1, x2), data = CCD1) 
model_fist_trial
summary(model_fist_trial)

#The results shows that this fitting with quadratic and two-way interaction is not a good fit.
#Only the coeficient for x1 and x2 are significant
#This shows that the initial experiment is far from the optimum since not stationary point (max or min) is found.


par(mfrow=c(1,1))
contour(model_fist_trial, ~ x1+x2, image = TRUE)
persp(model_fist_trial, x2~x1, col = terrain.colors(12), contours = "colors",
      zlab = "Yeld (%)", xlabs = c("Time (min)", "Temperature (°C)"))



#The first trial experiment did not show a quadratic relationship, meaning that is far from the optimum point.
#A series of experiments, based on this first trial will lead to the optimum area, based at the maximum yield
#varying the x1 unit and the x2 unit based on the slope of the x1,x2 coefficient 0.325/0.775 = 0.42
#First-order response surface (linear interaction), has its contours of y, is a series of parallel lines, as in the graph.
#The direction of steepest ascent is the direction in which y increases most rapidly. 
#This direction is normal (perpendicular to the central point to the contour lines) to the fitted response surface
#this values is the tangent of x2 in the y axis and x1 in the x axis: coefficient 0.325/0.775 = 0.42.

library(ggplot2)
library(dplyr)

#### 2° step #####
#steepest ascent method to walk towards the optimum region

steepascent <- read.csv("steepest ascent experiment.csv", sep = ";", dec = ",")
str(steepascent)

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



#### 3° step #####
#find the region were the yield is maximized based on the steepest ascent of the trial experiment
#On this vicinity perform regression using interaction and quadratic terms 


CCD2 <- read.csv("2 order data.csv", sep = ";", dec = ",")

#Relationship between coded and natural variables
CCD2 <- as.coded.data(CCD2,
                      x1 ~ (time-85)/5,
                      x2 ~ (temperature-175)/5)

str(CCD2)

#Regression model or Yeld (y) without quadratic term
optimum_model <- rsm(response ~ FO(x1, x2) + TWI(x1, x2), data = CCD2) 
optimum_model
summary(optimum_model)

#results of the lack of fit shows that this linear model is not a good fit for it.

#Regression model or Yeld (y) with quadratic term
optimum_model <- rsm(response ~ FO(x1, x2) + TWI(x1, x2) + PQ(x1, x2), data = CCD2) 
optimum_model
summary(optimum_model)

#using the pure quadratic term, the model fits better


#Countour/perspective plots
par(mfrow = c(1,1))
contour(optimum_model, ~ x1+x2,
        image = TRUE,
        yaxp = c(180,170,2),
        xlabs = c("Time (min)", "Temperature (°C)"))

persp(optimum_model, x2~x1, col = terrain.colors(12), contours = "colors",
      zlab = "Yeld (%)", xlabs = c("Time (min)", "Temperature (°C)"))



