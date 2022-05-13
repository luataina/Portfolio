### Design of factorial experiment

# 2 Factors: k; 2 Levels: L. 2^2 

#Level
levels <- c(-1,1)

#Planing 
plan <- expand.grid(levels, levels)

#Experimental replication

plan <- rbind(plan, plan)

#Name columns 
colnames(plan) <- c("x1", "x2")

# Dependent variable (DV)
y <- c(5.1,8.6,7.7,13.5,5.0,8.5,8.2,13.9)

#Adding DV variable to the planing 
plan$DV <- y
 
#Planing matrix
x <- model.matrix(~x1*x2, data = plan[,-3])

#Effects
effect <- crossprod(x,y)/(2*2^2/2)
effect  

#coeficient
coef <- effect/2
coef  

#adjusted values
fitted <- x%*%coef
fitted  

#Residues
residues <- y - fitted
residues  

#Number of assays (N) and terms of the model (r)
N <- dim(x) [1]
r <- dim(x) [2]

#Sum of squared erros
SSE <- sum(residues^2)
  
#Sum of squared total
SST <- sum(y^2)-sum(y)^2/N

#Degrees of freedom of error
DF <- N-r
  
#Degree of freedom total
DFt <- N-1

#Mean square errors (MSE)
MSE <- SSE/DF

#Mean square total (MST)
MST <- SST/DFt

#Calculated T value
tvalue <- coef/sqrt(MSE/N)

#Critic t value
tcritic <- qt(0.025, df= DF, lower.tail = F)
tcritic

#Calculated p value
pvalue <- 2*pt(abs(tvalue), df = DF, lower.tail = F)
pvalue

#Summary data frame T-test
ttestframe <- data.frame(coef, rep(sqrt(MSE/N), 4), tvalue, pvalue)
ttestframe
colnames(ttestframe) <- c("coef", "SE_coef", "T value", "p-value")
ttestframe

#Determination coeficient
r2 <- 1-SSE/SST
r2

r2adj <- 1-MSE/MST
r2adj


### AMOVA ####

#Sum of square effects
SS_x <- crossprod(x[,-1],y)^2/N

#Mean of square effects
MS_x <- SS_x/1


#F value calculated
Fvalue <- MS_x/MSE
Fvalue

#p value associated with ANOVA
pval <- pf(Fvalue, 1, DF, lower.tail = F)
pval


#ANOVA table
source <- c("x1", "x2", "x1x2", "Error", "Total")
SS <- c(SS_x,SSE,SST)
DF <- c(rep(1,3), DF, DFt)
MS <- c(MS_x,MSE,MST)
Fval <- c(Fvalue, NA,NA)

anovatable <- data.frame(SS,DF,MS,Fval)
rownames(anovatable) <- source
anovatable
