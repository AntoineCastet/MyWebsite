################################################################################
##############                 SEMINAIRE AUTOMAT                  ##############                  
##############               Une initiation à R (I)               ##############                  
##############       Antoine Castet et Jean-Baptiste Guiffard     ##############
################################################################################


################
#Première Partie
################

# Un langage orienté objet
# numeric, text, vector, list, data.frame, factor

#Scalar
a <- 1
b <- "Le Séminaire Automat"

#Vecteur
c <- c(3, 4, 5, 6)
d <- c(7, 8, 9, 10)
e <- c("Le", "Séminaire", "Automat")

#Matrice
f <- matrix(1, nrow = 2, ncol=4)
g <- rbind(c,d)

h <- f*(g/2)

#Liste 
i <- list(b, d, h, "h")
j <- list(i, "Poupée russe de liste")

#Data frame
taille <- c(152, 156, 160, 160, 163, 167, 169, 173, 174, 174)
masse <- c(51, 51, 54, 60, 61, 64, 70, 71, 72, 73)
sexe <-c("M","F","F","M", "M","F","F","M", "F", "F")
df <- data.frame(taille,masse,sexe)







################
#Deuxième Partie
################

# Installation et chargement des packages
#install.packages("ggplot2") visualisation
#install.packages("dplyr") manipulation des données
#install.packages("tidyr") remise en forme des données
#install.packages("readr") importation de données
#install.packages("purrr") programmation
#install.packages("tibble") tableaux de données
#install.packages("stringr") chaînes de caractères
#install.packages("forcats") variables qualitatives

install.packages("tidyverse")
install.packages("kableExtra")
install.packages('haven')
install.packages('readxl')

library(tidyverse)
library(kableExtra)
library(haven)
library(readxl)



# créer son chemin d'accès
setwd('C:/Users/acastet/Desktop')

# Chargement des données (création de data.frame)
# CSV
eco_eu_csv <- read.csv2("economistes_eu.csv")

# Excel
eco_eu_xls <- read_excel("economistes_eu.xlsx")

#.dta (STATA)
eco_eu_dta <- read_dta("economistes_eu.dta")

#Connaitre la classe de  l'objet précédemment créé
class(eco_eu_csv)

#Connaitre la classe des variables du data.frame
str(eco_eu_csv) 
#chr = character = texte / int = integer = nombre
# NA = not available = absence de données

#Avoir un aperçu de l'objet
View(eco_eu_csv)









#La variable city est une variable "catégorielle" ou "qualitative" contenant le nom de la ville du chercheur. 
#Il faut informer R que nous souhaitons que cette variable soit considéré comme une variable catégorielle.
#On va donc en profiter pour créer une nouvelle variable, copie de city mais en facteur, city_factor.

eco_eu_csv$city_factor <- as.factor(eco_eu_csv$city)
str(eco_eu_csv) 

#On peut ensuite regarder les différentes modalités.
levels(eco_eu_csv$city_factor)

#On s'aperçoit qu'il y a un problème avec l'écriture de la ville de Paris. On va donc corriger cela.
eco_eu_csv$city_factor <- recode_factor(eco_eu_csv$city_factor, 
                                 "PARIS"  = "Paris", 
                                 "Paris ()" = "Paris",
                                 "Paris-Grignon" = "Paris",
                                 "Paris " = "Paris")

#Désormais on peut regarder le nombre de chercheurs étant référencé à Paris.
table(eco_eu_csv$city_factor) 

#Un aspect intéressant peut être de regarder la concentration des chercheurs dans les capitales des 3 pays.
#On crée donc une variable binaire indiquant si le chercheur est référencé dans une capitale.
eco_eu_csv$capital <- ifelse(eco_eu_csv$city_factor %in% c("Paris", "Madrid", "London"), 1,0)

#On observe les informations principales avec summary.
summary(eco_eu_csv$capital)



#On s'interroge ensuite sur la quantité d'articles publiés par les chercheurs grâce à la variable sum_publi.
summary(eco_eu_csv$sum_publi)

#On crée une variable indiquant au sein de quel quartile de publications les chercheurs sont.
eco_eu_csv <- eco_eu_csv %>% 
  mutate(publi_intervals = case_when(
    sum_publi <= 2 ~ "1st quartile (<=2)",
    sum_publi > 2 & sum_publi <= 10 ~ "2nd quartile (<=10)",
    sum_publi > 0 & sum_publi <= 31 ~ "3rd quartile (<=31)",
    sum_publi > 31 ~ "4th quartile (<=1016)"))

#On souhaite ordonner la base de données en fonction du nombre de publications des individus.
eco_eu_csv <- eco_eu_csv %>%
  arrange(sum_publi)

#Puis on crée une table mettant en regard le quartile de publication avec le fait d'être ou non dans la capitale.
table(eco_eu_csv$capital, eco_eu_csv$publi_intervals) 










#On souhaite ensuite connaitre le genre des auteurs.
#Pour cela on va faire une analyse des prénoms en s'aidant d'une base de données extérieure.
#On charge la base de données des prénoms.
prenoms_csv <- read.csv2("Prenoms.csv")
view(prenoms_csv)

# On constate que les prénoms sont en minuscule.
#Donc on crée la variable prénom dans la base chercheur. 
eco_eu_csv$prenom <- gsub( "\\s.*", "", eco_eu_csv$authors)

#Puis on la transforme en minuscule.
eco_eu_csv$prenom <- tolower(eco_eu_csv$prenom)

#Ces deux lignes auraient pu être écrite en une :
eco_eu_csv$prenom <- tolower(gsub( "\\s.*", "", eco_eu_csv$authors))

#On va ensuite fusionner deux les bases de données, prénom et auteurs, en fonction du prénom.
eco_eu_pre_csv <- left_join(eco_eu_csv, prenoms_csv,  by = c("prenom" = "X01_prenom"))

#On conserve uniquement les auteurs dont les prénoms ont pu être retrouvé dans la base de données prénom.
eco_eu_pre_csv <-eco_eu_pre_csv %>%
  filter(is.na(X02_genre)==FALSE)
         
#On conserve uniquement les auteurs dont les prénoms sont référence strictement comme homme ou femme.
eco_eu_pre_csv <-eco_eu_pre_csv %>%
  filter(X02_genre!="m,f"& X02_genre!="f,m")

#On crée une variable binaire indiquant si le chercheur est un homme ou une femme.
eco_eu_pre_csv$femme <- ifelse(eco_eu_pre_csv$X02_genre=="f",1,0 )

#Puis on crée une table mettant en regard le quartile de publication avec le fait d'être ou non une femme.
table(eco_eu_csv$femme, eco_eu_csv$publi_intervals) 










#Pour finir, on souhaite finalement conservé uniquement les chercheurs faisant du développement.
#On crée une nouvelle base de données avec uniquement les chercheurs "NEP.DEv".
eco_eu_dev <- subset(eco_eu_csv, NEP.DEV>=1)


#Après réflexions, les chercheurs du NEP.AFR devraient être également inclus..
#On crée donc une base de données avec uniquement les chercheurs "NEP.AFR" que l'on va ajouter à la précédente.
eco_eu_afr <- subset(eco_eu_csv, NEP.AFR>=1)

#on réunit les deux bases de données.
eco_eu_devafr <- rbind(eco_eu_dev, eco_eu_afr)


#Le problème c'est que des auteurs peuvent être "NEP.DEv" et "NEP.AFR". Il faut donc repérer les doublons.
eco_eu_devafr <-eco_eu_devafr %>%
  mutate(dup_authors = duplicated(authors)) 

#On observe le nombre de doublons.
table(eco_eu_devafr$dup_authors)

#Et on supprime les doublons de la base de données.
eco_eu_devafr <-eco_eu_devafr %>%
  filter(dup_authors==FALSE)

#Ceci étant fait on peut supprimer la variable permettant de repérer les doublons.
eco_eu_devafr <- eco_eu_devafr %>%
  select(-dup_authors) 

#Et on peut renommer les variables. 
eco_eu_devafr <-eco_eu_devafr %>%
  rename(auteurs="authors")






