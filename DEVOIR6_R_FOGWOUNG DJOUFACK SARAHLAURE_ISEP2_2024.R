# DEVOIR SEANCE 6 de R

# RECODONS LA VARAIBLE TYPE DE CEREALES 
    # Importons la base 
library(haven)
cereales <- read_dta("DOC EHCVM/cereales.dta")
View(cereales)
colnames(cereales)[4:14] <- c("AutresCereales","Qtty_cons",
                              "Unite_cons","Taille_cons",
                              "AutoCons","AutresProv",
                              "DernierAchat","Qtty_achat",
                              "Unite_achat","Taille_achat",
                              "Value_achat")
    # Recodage (Je vais regrouper les differents types de cereales representés par la variable cereales__id en 4 differentes categories )
names(cereales)
unique(cereales$cereales__id)

library(dplyr)

vecteur_recodage <- case_when(
  cereales$cereales__id %in% c(1,2,3,4,5,6,7,9,10,11,12) ~ "Céréales de base",
  cereales$cereales__id %in% c(13,14,15,16,17,18,19,20) ~ "Produits dérivés des céréales",
  cereales$cereales__id %in% c(21,22,23,24,25,26) ~ "Produits de boulangerie",
  cereales$cereales__id == 169 ~ "Céréales du petit déjeuner",
  TRUE ~ NA_character_  # Pour les valeurs non prévues, NA
)

cereales <- cereales %>%
  mutate(cereale_recodée = vecteur_recodage)

#DECOUPAGE EN CLASSE D'une cereale et une unité standard qu'on aura identifié  (PETITE CONSOMMATION (inferieur à 10kg), MOYENNE(inf à 20 ) ET GRANDE ((sup à 20)))
# Regroupons en classe les differentes consommations 

library(dplyr)

cereales <- cereales %>%
  mutate (Quantite_achetee_recodée = case_when (
    Qtty_achat < 5 ~ "Faible quantité achetée", 
    Qtty_achat >= 5 & Qtty_achat < 10 ~ "Moyenne quantité achetée",
    Qtty_achat >= 10 ~ "Forte quantité achetée",
    TRUE ~ NA_character_
    ))

cereales <- cereales %>%
  mutate (Quantite_consommée_recodée = case_when (
    Qtty_cons < 5 ~ "Faible quantité consommée", 
    Qtty_cons >= 5 & Qtty_cons < 10 ~ "Moyenne quantité consommée",
    Qtty_cons >= 10 ~ "Forte quantité consommée",
    TRUE ~ NA_character_
  ))

install.packages("readxl")  
library(readxl)  
table_de_conversion <- read_excel("E:/ISEP 2/MON DOSSIER/SEMESTRE 2/TRAITEMENTS STATISTIQUES AVEC R/DOC EHCVM/Table_de_conversion_phase_2_1.xlsx", sheet="nationale")
View(table_de_conversion)

#IMPORTER LA TABLE DES CONVERSIONS , IDENTIFIER LES VARIABLES DE MERGE ET FAIRE DES MANIPULATIONS POUR MERGER CA AVEC LA BASE CEREALES
ensemble <- merge(table_de_conversion, cereales, by = "cereales__id")
View(ensemble)

















