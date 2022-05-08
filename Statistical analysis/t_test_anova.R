library(car)
library(dplyr)
#n R, a package is a collection of R functions, data and compiled code. 
#The location where the packages are stored is the library. 
# You can download  a particular package from the appropriate site and it will be stored in your library.
#To  use the package, use the command "library(package)" which makes it available to you.


##################### T-TEST #########################

fluo2 <- read.csv("nestinfluo2.csv", sep = ";", dec = ",")
str(fluo2)
View(fluo2)
glimpse(fluo2)
fluo2$fluorescencia <- as.numeric(fluo2$fluorescencia)
fluo2$treatment <- as.factor(fluo2$treatment)
# bugs ate aqui. 1° dec teve que ser usado por "," 2° treatment que anterior era <chr> teve que ser mudado p factor <fct>

byf.shapiro(fluorescencia ~ treatment, fluo2)

#Error in byf.shapiro(fluorescencia ~ treatment, fluo2) : could not find function "byf.shapiro" 
#foi precisa usar 1° o library(RVAideMemoire) p funcionar o byf.shapiro

library(RVAideMemoire)

# teste de homogeneidade de variancia levene test. 
# teste usa mediana como default, mas usando center=mean poode-se usar a média
leveneTest(fluorescencia  ~ treatment, fluo2)
leveneTest(fluorescencia  ~ treatment, fluo2, center=mean)
leveneTest(treatment~fluorescencia, fluo2)
#Error in leveneTest.formula(treatment ~ fluorescencia, fluo2) : 
#Levene's test is not appropriate with quantitative explanatory variables.
leveneTest(fluorescencia~treatment,fluo2)
# pelo notado ate entao, as funcoes sao baseadas em order de/para: 
#variavel dependente eixo y -> variavel independete (grupo/fator/eixo x) -> banco de dados utilizado
t.test(fluorescencia~treatment,fluo2,var.equal=TRUE)
library(ggplot2)


### BOXPLOT ####
ggplot(data=fluo2,aes(treatment,fluorescencia)) +
  geom_boxplot(fill = "white", col = "black") + 
  geom_point(position = position_jitter(0.3), color = "black") +
  stat_summary(fun="mean", colour="red", size=1) +
  scale_y_continuous(expand = c(0,0), limits = c(0,300))+
  theme_classic() +
  labs(title="Flourescence measurement after exposure",
  x ="Treatment", y = "Fluorescence (A.U)")+
  theme(axis.text = element_text(size = 12),
  axis.title = element_text(size=13,face="bold"))
  
  


##### COLUMN BARS + SD #####
df <- fluo2
df$treatment <- as.factor(df$treatment)
head(df, 3)
library(dplyr)
df.summary <- df %>%
  group_by(treatment) %>%
  summarise(sd = sd(fluorescencia, na.rm = TRUE),
    fluorescencia = mean(fluorescencia))

df.summary


ggplot(df, aes(treatment, fluorescencia)) +
  geom_col(data = df.summary, fill = NA, color = "black", width = 0.7) +
  geom_jitter( position = position_jitter(0.2), color = "black") + 
  geom_errorbar( aes(ymin = fluorescencia-sd, ymax = fluorescencia+sd), 
  data = df.summary, width = 0.2) +
  scale_y_continuous(expand = c(0,0), limits = c(0,300)) +
  theme_classic() +
  labs(title="Flourescence measurement after exposure",
  x ="Treatment", y = "Fluorescence (A.U)")+
  theme(axis.text = element_text(size = 12),
  axis.title = element_text(size=13,face="bold"))


################### ONEWAY ANOVA #######################


# Passo 1: Carregar os pacotes que serão usados

if(!require(dplyr)) install.packages("dplyr")
library(dplyr)                                
if(!require(RVAideMemoire)) install.packages("RVAideMemoire") 
library(RVAideMemoire)                                        
if(!require(car)) install.packages("car")   
library(car)                                
if(!require(psych)) install.packages("psych") 
library(psych)                                
if(!require(rstatix)) install.packages("rstatix") 
library(rstatix)                                
if(!require(DescTools)) install.packages("DescTools") 
library(DescTools)

# Passo 2: carregar banco de dados
# Importante: selecionar o diretório de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory

fluo3<-read.csv("nestinfluo3.csv", sep =";", dec=",")
str(fluo3)
View(fluo3)
glimpse(fluo3)
fluo3$treatment <- as.factor(fluo3$treatment)

#Passo 3: verificar normalidade dos dados
# Shapiro por grupo (pacote RVAideMemoire). O Shapiry.test nao faz avaliacao por grupos.
#Por isso usamos a funcaoo byf.shapiro do pacote RVAideMemoire.
#Logica da sintaxe do código byf.shapiro (1°Y,2°X,3°banco de dados)
#  1°-variavel analisada da normalidade: independente ou y. 
##usa o til ~ para indicar a variável de agrupamento: treatment (controlxgbhxgly)
### após a virgula usa o banco de dados onde estao as variáveis. 

byf.shapiro(fluorescencia~treatment,fluo3)

# Passo 4: verificar homogeneidade de variancia
# A mesma logica aplicada para verifica normalidade, se aplica para homogeneidade de variancia.
# Logica da sintaxe do código levene.test() (1°Y,2°X,3°banco de dados)
# Observação:
# Por default, o teste realizado pelo pacote car tem como base a mediana (median)
# O teste baseado na mediana é mais robusto
# Mudamos para ser baseado na média (comparável ao SPSS)

leveneTest(fluorescencia~treatment,fluo3, center=mean)

# Passo 5: Verificação da presença de outliers (por grupo) - Pacotes dplyr e rstatix

# Para fluo3:
fluo3 %>% group_by(treatment) %>% identify_outliers(fluorescencia)

## Pelo boxplot:
geom_boxplot(fluorescencia ~ treatment, data = fluo3, ylab="Fluorescence (A.U)", xlab="Treatment") +
  geom_point()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size=13,face="bold"))


# Passo 6: Realização da ANOVA

## Criação do modelo para fluorescencia
anova_fluo3 <- aov(fluorescencia ~ treatment, fluo3)
summary(anova_fluo3)

# Passo 7: Análise post-hoc - Pacote DescTools
# Post-hocs: Tukey HSD "hsd". confidence interval: conf.level=value

PostHocTest(anova_fluo3,method = "hsd", conf.level = 0.95)
PostHocTest(anova_fluo3,method = "bonf", conf.level = 0.95)

## Passo 8: Resumir em uma tabela mais de um post-hoc
round (cbind(bonf = PostHocTest(anova_fluo3, method="bonf")$treatment[,"pval"],
        hsd = PostHocTest(anova_fluo3, method="hsd")$treatment[,"pval"]),6)

# Passo 8: Análise descritiva dos dados
describeBy(fluo3$fluorescencia, group = fluo3$treatment)

## polting p value on graph
install.packages("ggpubr")
library(ggpubr)

p_fluo2 <- ggboxplot(fluo2, x = "treatment", y = "fluorescencia", palette = "jco", add = "jitter")

#  Add p-value
p_fluo2 + stat_compare_means()

# Change method
p_fluo2 + stat_compare_means(method = "t.test")

p_fluo3 <- ggboxplot(fluo3, x = "treatment", y = "fluorescencia", palette = "jco", add = "jitter")
#  Add p-value
p_fluo3 + stat_compare_means()
# Change method
p_fluo3 + stat_compare_means(method = "aov")

