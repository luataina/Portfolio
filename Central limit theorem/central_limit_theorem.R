#### Central limit theorem ###

# Randomization
set.seed(9)

# Sample size
n <- 40

# Lambda rate exponential dist.
lambda <- 0.2

# Pre alocation - zero matrix
sample <- matrix(0, nrow = 40, ncol = 1000)

# Sample's random draw from lambda exponential dist.
for (i in 1:1000) {
  sample[,i] <- rexp(n, rate = lambda)
}

# Ploting graph
sample1 <- sample[,1]
hist(sample1, col = "red")

# Mean and standard deviation from exponential population distribution
mean_exp <- 1/lambda
sd_exp <- (1/lambda)/sqrt(n)

# Mean of exponential samples
mean_sample <- colMeans(sample)

# histogram of the exponential sample's mean
hist(mean_sample, col = "lightsteelblue1", freq = F,xlim=c(2,9))

#Teoric mean of exponential distribution
abline(v = mean_exp, col = "red")

#Teoric mean of exponential sample distribution
abline(v = mean(mean_sample), col = "blue")

#Teoric density distribution
x <- seq(2, 9, length = 500)
y <- dnorm(x, mean_exp, sd_exp)
lines(x, y)






