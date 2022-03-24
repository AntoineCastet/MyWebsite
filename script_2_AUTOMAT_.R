################################################################################
##############                 SEMINAIRE AUTOMAT                  ##############                  
##############              Une initiation à R (II)               ##############                  
##############       Antoine Castet et Jean-Baptiste Guiffard     ##############
################################################################################

# Installation et chargement des packages

install.packages('wooldridge')
install.packages('jtools')
install.packages('fixest')
install.packages('modelsummary')
install.packages('table1')
install.packages('DataExplorer')
install.packages('ggthemes')


library(tidyverse)
library(wooldridge)
library(jtools)
library(fixest)
library(modelsummary)
library(table1)
library(DataExplorer)
library(ggthemes)


#Chargement d'une base de données issues du package Wooldridge (datasets utilisés pour sa méthode d'introduction à l'économétrie)
data('wage1')

nrow(wage1) #nombre d'observations
ncol(wage1) #nombre de variables
dim(wage1) # les deux

# Résumé de nos variables avec la commande descibe issu du package explore
View(wage1 %>% explore::describe())

# Ajouter d'une variable catégorielle "région" avec la fonction mutate du package dplyr : 
wage1 <- wage1 %>%
  mutate(region = case_when(
    northcen == 1 ~ "northcen",
    south == 1 ~ "south",
    west == 1 ~ "west"),
    region = ifelse(is.na(region),'unknown',region))

# tableau de statistiques descriptives (avec le package table1) :
table1(~wage + educ + nonwhite + female |region, data=wage1)

# Summary des données avec DataExplorer (davantage d'informations par variables)
datasummary_skim(wage1)

# Histogramme et comparaison à une loi normale
ggplot(wage1, aes(x = wage)) + 
  geom_histogram(aes(y =..density..),
                 breaks = seq(0, 25, by = 1), 
                 colour = "black", 
                 fill = "white")

p <- ggplot(wage1, aes(x = wage)) + 
  geom_histogram(aes(y =..density..),
                 breaks = seq(0, 25, by = 0.5), 
                 colour = "white", 
                 fill = "blue") +
  stat_function(fun = dnorm, args = list(mean = mean(wage1$wage), sd = sd(wage1$wage)))

p + theme_economist() 

p+theme_stata()

p+theme_classic()


# 1) Pas beau
ggplot(wage1, aes(x=educ, y=wage)) + geom_point()

# 2) Changement de la forme des points
ggplot(wage1, aes(x=educ, y=wage)) +
  geom_point(size=2, shape=17)

# 3) Couleurs
ggplot(wage1, aes(x=educ, y=wage)) +
  geom_point(size=2, shape=17, colour='black', fill="red") 

ggplot(wage1, aes(x=educ, y=wage)) +
  geom_point(size=2, shape=24, colour='black', fill="red")

# 4) fond
ggplot(wage1, aes(x=educ, y=wage)) +
  geom_point(size=2, shape=24, colour='black', fill="red")+ 
  theme_classic()

# 5) légende, titre
ggplot(wage1, aes(x=educ, y=wage)) +
  geom_point(size=2, shape=24, colour='black', fill="red") +
  ggtitle('Scatter plot AUTOMAT')+
  theme_classic()+
  labs(caption ="Source: Wooldridge",
       x='Education (in year)',
       y='Hourly Wage (in $)')
 
# 6) Nuage de points par mariage
ggplot(wage1, aes(x=educ, y=wage, colour = factor(married)))+
  geom_point(size=1, shape=16)+
  theme_economist() + 
  scale_color_economist()+
  ggtitle("Wage1 data set")

ggplot(wage1, aes(x=educ, y=wage, colour = factor(married, labels = c("Non", "Oui"))))+
  geom_point()+
  theme_economist() + 
  scale_color_economist()+
  ggtitle("Wage1 data set")+
  labs(colour = "Marié ?")

# Régressions
reg1 <- lm(lwage ~ educ , wage1)
reg2 <- lm(lwage ~ exper, wage1)
reg3 <- lm(lwage ~ tenure, wage1)
reg4 <- lm(lwage ~ educ + exper , wage1)
summary(reg1)
summary(reg2)
summary(reg3)
summary(reg4)

# ajout de davantage de controls
reg5 <- lm(lwage ~ educ + exper + female + nonwhite + married, wage1)
summary(reg5)

# Avec ajout d'un terme quadratique sur l'expérience
reg6 <- lm(lwage ~ educ + exper + expersq + female + nonwhite + married, wage1)
summary(reg6)

# ajouter droite de régression au ggplot
ggplot(wage1, aes(x=educ, y=wage)) +
  geom_point(size=1) +
  geom_smooth(method=lm, se=FALSE)

ggplot(wage1, aes(x=exper, y=wage)) +
  geom_point(size=1) +
  geom_smooth(method=lm, se=FALSE)

# ajouter intervalle de confiance
ggplot(wage1, aes(x=exper, y=wage)) +
  geom_point(size=1) +
  geom_smooth(method=lm, se=TRUE)




# représentation graphique coefficients (coef plots)

plot_summs(reg1, reg2, reg3) 

plot_summs(reg1, reg2, reg3) + coord_flip()
	
plot_summs(reg1, reg2, reg3, reg4)

plot_summs(reg1, reg4, omit.coefs = c("exper", "tenure", "(Intercept)"))

plot_summs(reg1, reg4, reg5, reg6, coefs = c("educ"))

# ajouter les noms des modèles

plot_summs(reg1, reg4, reg5, reg6, coefs = c("educ"),
           model.names = c('Simple','Experience', 'Controls', 'Quadratic term'))



# Régressions avec fixest (fe et clustering se)
data('trade')
View(trade %>% explore::describe())
View(trade)
trade <- trade %>%
  mutate(lndist_km = log(dist_km))

gravity_pois1 = fepois(Euros ~ lndist_km | Year, trade)
modelsummary(gravity_pois1, stars = c("*"=0.1,"**"=.05,"***"=.01))
gravity_pois1 = fepois(Euros ~ log(dist_km) | Year, trade)
gravity_pois2 = fepois(Euros ~ log(dist_km) | Origin + Destination + Product + Year, trade)
modelsummary(gravity_pois2, stars = c("*"=0.1,"**"=.05,"***"=.01))

gravity_pois3 = fepois(Euros ~ log(dist_km) | Origin + Destination + Product + Year, cluster = "Product", trade)
modelsummary(gravity_pois3, stars = c("*"=0.1,"**"=.05,"***"=.01))


models <- list()
models[['Year fe']] <- gravity_pois1
models[['Several fe']] <- gravity_pois2
models[['Clustered fe']] <- gravity_pois3

modelsummary(models, stars = c("*"=0.1,"**"=.05,"***"=.01))


	
	