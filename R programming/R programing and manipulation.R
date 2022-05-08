##################### objects and functions ##################### 

5+6
a <- 5
b <- 6
a+b
sum(a,b)

ages <- c(10,12)
ages
sum(ages)
names <- c("Jhon", "Marie")
friends <- data.frame(ages, names)
View(friends)
str(friends)
friends$ages
sum(friends$ages)
friends$names
friends[1,1]
friends[,1]
friends[1,]


##################### installing and using packages ##################### 

install.packages("tidyverse")
library(tidyverse)
install.packages("ggpubr")
library(ggpubr)
install.packages("gapminder")
library(gapminder)
View(gapminder)
install.packages("dplyr")
library(dplyr)
install.packages("ggpmisc")
library(ggpmisc)
install.packages("tibble")
library(tibble)
install.packages("quantreg")
library(quantreg)


#Use of built in datasets
data()
View(starwars)

starwars %>% 
  filter(height > 150, mass < 200) %>% 
  mutate(height_in_meters = height/100) %>% 
  select(height_in_meters, mass) %>% 
  arrange(-height_in_meters) %>% 
  View() 

#data structure and manipulation
View(msleep)
glimpse(msleep)
class(msleep)
class(msleep$name)
length(msleep)
unique(msleep$vore)

missing <- !complete.cases(msleep)
msleep[missing,]

#clean data and selecting variables

starwars %>% 
  select(name, height, mass)

starwars %>% 
  select(1:3)

starwars %>% 
  select(ends_with("color"))

#chaging variable order
starwars %>% 
  select( height,name, mass) 

#changing variable name
starwars %>% 
  rename( charac = name) 

#changing variable type
 class(starwars$hair_color) 
 starwars$hair_color <- as.factor(starwars$hair_color)
 class(starwars$hair_color) 

#changing variable type using pipe operator
# mutate function: creates a new variable or write over a variable
 starwars %>% 
   mutate(hair_color = as.character(hair_color))
 
 starwars %>% 
   mutate(height_M = height/100) %>% 
   select(name, height, height_M)

 #changing variable types 
#changing factor levels order
#factor's levels are ordered by alphabetic logic
 df <- starwars
df$sex <- as.factor(df$sex)
levels(df$sex)

#changing factor level order
df <- df %>% 
  mutate(sex = factor(sex, levels = c("male", "female", "hermaphroditic", "none")))
levels(df$sex)

# Select and filter
# Select is used for columns and filter to rows
starwars %>% 
  select(mass, sex) %>% 
  filter(mass < 55, sex=="male")

#recode data
starwars %>% 
  select(sex) %>% 
  mutate(recode(sex, "male" = "man", "female" = "woman"))
View(starwars)

#dealing with missing data
mean(starwars$height)
#Function is not able to deal with missing values. By default "na.rm = FALSE".
mean(starwars$height, na.rm = TRUE)

#dealing with duplicates
#creating a data frame with duplicates
Names <- c("Jhon", "Marie", "Robert", "Carlos", "Robert") 
Ages <- c(22, 21, 23, 24, 23)
Friends <- data.frame(Names, Ages)

#checking duplicates at the data frame
Friends

Friends %>% 
  distinct()
distinct(Friends)

#conditional change (if_else)
#question: is height <1? if name it short, else: name it tall

starwars %>% 
  mutate(height_M = height/100) %>% 
  select(name, height, height_M) %>% 
  mutate(tallness = if_else(height_M < 1, "short", "tall" ))

#selecting columns using base R
data2 <- gapminder[c("country", "year", "lifeExp")]
View(data2)

#selecting columns using dplyr
data <- select(gapminder, country, year, lifeExp)
View(data)

#reshape data with pivot_winder

#long data fram to wide data frame
wide_data <- data %>% 
  pivot_wider(names_from = year, values_from = lifeExp)
View(wide_data)

#wide data frame to long data frame
long_data <- wide_data %>% 
  pivot_longer(2:13, names_to = "year", values_to = "lifeExp")
View(long_data)


##################### descriptive statistics ##################### 

summary(msleep)

# descriptive statistics of specific variable
summary(msleep$sleep_total)
summary(msleep$brainwt)

#range
min(msleep$awake)
max(msleep$awake)
range(msleep$awake)
IQR(msleep$awake)

#centrality
mean(msleep$awake)
median(msleep$awake)

#summarizing 2 or more variables

msleep %>% 
  select(awake, sleep_total) %>% 
  summary()

#creating table 
table(msleep$vore)

msleep %>% 
  select(vore, order) %>% 
  filter(order %in% c("Rodentia", "Primates")) %>% 
  table()

##################### visalization ##################### 

plot(pressure)

#grammar of graphics: ggplot
#data (data set used)
#mapping aesthetic (a and y axis used, color, shape and size of variables)
#geometry (the geometric shape of the graph)

#bar plot
ggplot(data = starwars, mapping = aes(x = gender),na.rm = TRUE) +
  geom_bar() 

#removing na columns   
ggplot(data = starwars, mapping = aes(x = gender),na.rm = TRUE) +
  geom_bar(na.rm = TRUE) +
  scale_x_discrete(na.translate = FALSE)

#is.na() operator detects whether a value is missing. ! means not,
# filter on non-missing values
# x is gender
# fill is gender

#bar plot 
#use of pipe operator
starwars %>%
  filter(!is.na(gender)) %>% 
  ggplot(aes(x = gender, fill = gender)) + 
  geom_bar()
  
#histogram
starwars %>% 
  drop_na(height) %>% 
  ggplot(aes(x = height)) +
  geom_histogram() +
  stat_bin(bins = 10)

#box plot 
starwars %>% 
  drop_na(height) %>% 
  ggplot(aes(height)) +
  geom_boxplot(fill = "steelblue") +
  theme_classic() +
  labs(title = "boxplot of height", x = "height of characters")

# density plots
starwars %>% 
  drop_na(height) %>% 
  filter(sex %in% c("male", "female")) %>% 
  ggplot(aes(height, color = sex, fill = sex)) +
  geom_density(alpha = 0.5) +
  stat_central_tendency(type = "mean", linetype = "dashed")

# scatter plots
starwars %>% 
  filter(mass < 200) %>% 
  ggplot(aes(height, mass, color = sex, size = mass)) +
  geom_point(alpha = 0.6) +
  theme_classic() +
  labs(title = "Height and mass by sex")

#smoothed model  
starwars %>% 
  filter(!is.na(gender),mass < 200) %>% 
  ggplot(aes(height, mass, color = sex)) +
  geom_point(alpha = 0.6) +
  geom_smooth() +
  facet_wrap(~sex)+
  theme_bw() +
  labs(title = "Height and mass by sex")


##################### data analysis ##################### 

#### T-TEST ####
library(gapminder)
View(gapminder)


t_test_plot <- gapminder %>% 
  drop_na(lifeExp) %>% 
  filter(continent %in% c("Africa", "Europe")) %>% 
  ggplot(aes(lifeExp, color = continent, fill = continent)) +
  geom_density(alpha = 0.5) +
  stat_central_tendency(aes(color = continent), type = "median", linetype = 4) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.1))

t_test_plot

median_lifeExp <- gapminder %>% 
  group_by(continent) %>% 
  filter(continent %in% c("Africa","Europe")) %>% 
  summarise(median=median(lifeExp))

median_lifeExp

gapminder %>% 
  drop_na(lifeExp) %>% 
  filter(continent %in% c("Africa", "Europe")) %>% 
  ggplot(aes(lifeExp, color = continent, fill = continent)) +
  geom_density(alpha = 0.5) +
  geom_vline(data = median_lifeExp, 
  aes(xintercept = median, color = continent), size=1) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 0.1)) +
  scale_x_continuous(expand = c(0,0), limits = c(20, 90))

#T.test want the data indication in "data = "
# %>% pipe the data at beggining of filter function
# data =. tell R that using the %>% , pipe the gapminder into data =


# one can filter the factors of continent's variables by %in% or ==

#filter using %in% 
gapminder %>% 
  filter(continent %in% c("Africa", "Europe")) %>% 
  t.test(lifeExp ~ continent, data = ., alternative = "two.sided", paired = FALSE)

#filter using ==
gapminder %>% 
  filter(continent  == c("Africa", "Europe")) %>% 
  t.test(lifeExp ~ continent, data = ., alternative = "two.sided", paired = FALSE)


#### ANOVA ####

gapminder %>% 
  filter(year == 2007) %>% 
  filter(continent %in% c( "Americas", "Europe", "Asia")) %>% 
  aov(lifeExp ~ continent, data =.) %>% 
  summary()

gapminder %>% 
  filter(year %in% 2007) %>% 
  filter(continent %in% c("Americas", "Europe", "Asia")) %>% 
  aov(lifeExp ~ continent, data =.) %>% 
  summary()

#### POSTHOC test ####
gapminder %>% 
  filter(year == 2007) %>% 
  filter(continent %in% c("Americas", "Europe", "Asia")) %>% 
  aov(lifeExp ~ continent, data =.) %>% 
  TukeyHSD() %>% 
  plot()

#### CHI SQUARE ####

head(iris)

flower <- iris %>% 
  mutate(size = cut(Sepal.Length, breaks = 3, 
  labels = c("small", "medium", "large"))) %>% 
  select(Species, size)

### Chi square goodness of fit test ###
flower %>% 
  select(size) %>% 
  table() %>% 
  chisq.test()

### chi square test of independence ###
flower %>% 
  table() %>% 
  chisq.test()

### linear model ###
#stat_regline_equation: approximation of y-intercept value
head(cars, 10)
cars %>% 
  lm(dist ~ speed, data =.) %>% 
  ggplot(aes(speed, dist)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  stat_cor(aes(label = paste(..rr.label..,..p.label.., sep = "~`,`~")), label.x = 3, label.y = 100) +
  stat_regline_equation(label.x = 3, label.y = 90) +
  theme_classic() +
  labs(title = "Linear regression speed x distance", x = "Car speed", y = "Distance to stop")

### linear model ###
#stat_poly_eq approximation of p-value
cars %>% 
  lm(dist ~ speed, data =.) %>% 
  ggplot(aes(speed, dist)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  stat_poly_eq(formula = y ~ x,
  aes(label = paste(..eq.label.., ..rr.label..,..p.value.label.., sep = "~~~")),
  parse = TRUE, coef.digits = 4, f.digits = 4, p.digits = 3) +
  theme_classic() +
  labs(title = "Linear regression speed x distance", x = "Car speed", y = "Distance to stop")

### linear model ### 
cars %>% 
  lm(dist ~ speed, data =.) %>% 
  ggplot(aes(speed, dist)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  stat_cor(aes(label = paste(..rr.label..,..p.label.., sep = "~`,`~")), label.x = 3, label.y = 105) +
  stat_poly_eq(aes(label = paste(..eq.label.., sep = "~~~")),
  parse = TRUE, coef.digits = 4, f.digits = 4, p.digits = 3) +
  theme_classic() +
  labs(title = "Linear regression speed x distance", x = "Car speed", y = "Distance to stop")

### linear model ### 
  ggscatter(cars, x = "speed", y = "dist",color = "black", shape = 1, size = 3, add = "reg.line",
  add.params = list(color = "red", fill = "lightgray"),conf.int = TRUE, cor.coef = TRUE) +
  stat_poly_eq(aes(label = paste(..eq.label.., sep = "~~~")), parse = TRUE, coef.digits = 4,
  f.digits = 4, p.digits = 3, label.y = 0.85, label.x = 0.05) +
  labs(title = "Linear regression speed x distance", x = "Car speed", y = "Distance to stop")
  
### linear model ### 
  ggscatter(cars, x = "speed", y = "dist",color = "black", shape = 20, size = 3, add = "reg.line",
    add.params = list(color = "red", fill = "gray"),conf.int = TRUE) +
    stat_cor(aes(label = paste(..rr.label..,..p.label.., sep = "~`,`~")), label.x = 3, label.y = 100) +
    stat_poly_eq(aes(label = paste(..eq.label.., sep = "~~~")), parse = TRUE, coef.digits = 4,
    f.digits = 4, p.digits = 3, label.y = 0.9, label.x = 0.05) +
    labs(title = "Linear regression speed x distance", x = "Car speed", y = "Distance to stop")
  
  
  summary(lm(dist ~ speed, data = cars))
  
  