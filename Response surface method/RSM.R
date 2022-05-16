#Response surface methodology for 2 variables

CCD1 <- read.csv("central_composite_design.CSV", sep = ";", dec = ",")
CCD1
install.packages("rsm")
library(rsm)

#relationship between coded and natural variables
CCD1 <- as.coded.data(CCD1,
                      x1 ~ (time-85)/5,
                      x2 ~ (temp-175)/5)
str(CCD1)

#Regression model or Yeld (y)
model_Y1 <- rsm(y ~ SO(x1, x2), data = CCD1)
model_Y1
summary(model_Y1)

#OR
#FO: First order model; TWI: Two-way interaction; PQ: Pure quadratic 

model_Y2 <- rsm(y ~ FO(x1, x2) + TWI(x1, x2) + PQ(x1, x2), data = CCD1) 
model_Y2
summary(model_Y2)

#Improving model by removing not significant terms 
#Removing TWI not significant term (x1:x2  0.250000   0.133145   1.8777  0.102519)

model_Y2 <- rsm(y ~ FO(x1, x2) + PQ(x1, x2), data = CCD1) 
model_Y2
summary(model_Y2)

#Residual plots
#LOF lack of fit no significant, residues most be randomly distributed

#Creates grid of 2 rows by 2 columns for the plots
par(mfrow = c(2,2)) 
 plot(CCD1$y, model_Y2$residuals) +
   abline(h=0, col = "gray75")
 plot(model_Y2$fitted.values, model_Y2$residuals) +
   abline(h=0, col = "gray75")
 plot(CCD1$x1, model_Y2$residuals) +
   abline(h=0, col = "gray75")
 plot(CCD1$x2, model_Y2$residuals) +
   abline(h=0, col = "gray75")

 #Countour/perspective plots
 par(mfrow = c(1,2))
 contour(model_Y2, ~ x1+x2,
         image = TRUE,
         yaxp = c(168,182,2),
         xlabs = c("Time (min)", "Temperature (°C)"))
 mtext("Predicted max value 
80.18606", side=3)
points(CCD1$time, CCD1$temp) 

persp(model_Y2, x1~x2, col = terrain.colors(12), contours = "colors",
      zlab = "Yeld (%)", xlabs = c("Time (min)", "Temperature (°C)"))

#Predicting the yeld at the stationary point
max <- data.frame(x1 = 0.3614555, x2 = 0.2572577)
predict(model_Y2, max)



