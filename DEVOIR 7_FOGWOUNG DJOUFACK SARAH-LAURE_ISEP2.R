
#DEVOIR 7 : Sur la base de donnees cereales 
#Chargement de la base
library(haven)
cereales <- read_dta(paste0("DOC EHCVM/cereales.dta"))

library("tidyverse")
glimpse(cereales)

#--Renommer variables
colnames(cereales)[4:14] <- c("AutresCereales","Qtty_cons",
                              "Unite_cons","Taille_cons",
                              "AutoCons","AutresProv",
                              "DernierAchat","Qtty_achat",
                              "Unite_achat","Taille_achat",
                              "Value_achat")

##-Recodage de la variable cereales__id
cereales$cereales__id_recoded <- factor(cereales$cereales__id,
                                        levels = unname(attr(cereales$cereales__id,
                                                             "labels")),
                                        labels =names(attr(cereales$cereales__id,
                                                           "labels")))
edit(cereales$cereales__id_recoded)
View(cereales)

##-Recodage de la variable Unite_cons pour avoir les unites de mesures correspondantes
cereales$Unite_cons_recoded <- factor(cereales$Unite_cons,
                                      levels = unname(attr(cereales$Unite_cons,
                                                           "labels")),
                                      labels =names(attr(cereales$Unite_cons,
                                                         "labels")))
edit(cereales$Unite_cons_recoded)

##-Recodage de la variable Taille_cons pour avoir les unites de mesures correspondantes
cereales$Taille_cons_recoded <- factor(cereales$Taille_cons,
                                       levels = unname(attr(cereales$Taille_cons,
                                                            "labels")),
                                       labels =names(attr(cereales$Taille_cons,
                                                          "labels")))
edit(cereales$Taille_cons_recoded)

##-Changer le type d'une variable
attach(cereales)
DernierAchat <- as.factor(DernierAchat)


##-Decoupage en classe, identifier une cereale et une unite standard
cereales$classCereal_RizKg <- if_else(cereales$cereales__id==1 & cereales$Unite_cons==100,
                                      cut(cereales$Qtty_cons, labels = c("Tres faible","Faible", "Moyen" ,"Eleve"), 
                                          breaks = c(0,50,70,110,168)), NA)
edit(cereales$classCereal_RizKg)

## Essayons de merger la base cereale avec la table de conversion 
library(haven)
table_de_conversion <- read_csv2(paste0("E:/ISEP 2/MON DOSSIER/SEMESTRE 2/TRAITEMENTS STATISTIQUES AVEC R/DOC EHCVM/Table_de_conversion_phase_2_1.xlsx"))
View(table_de_conversion)

##-Supprimons les variables creees lors de l'importation
table_de_conversion[8:25] <- NULL
View(table_de_conversion)

###Créons dans la base cereales et la base table_de_conversion une variable real_id qui est la concaténation des valeurs de produitID, UniteID et TailleID
View(cereales)
cereales$real_id <- paste(cereales$cereales__id, cereales$Unite_cons, cereales$Taille_cons)
View(cereales)
table_de_conversion$real_id <- paste(table_de_conversion$produitID, table_de_conversion$uniteID, table_de_conversion$tailleID)
View(table_de_conversion)

###Nous pouvons maintenant fusionner la base cereales et la base table_de_conversion par la variable real_id
basefusionne <- inner_join(cereales, table_de_conversion, by= "real_id")
View(basefusionne)
