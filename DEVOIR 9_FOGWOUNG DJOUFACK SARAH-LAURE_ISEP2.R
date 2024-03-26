### DEVOIR DE R SEANCE 9 BY SARAH-LAURE FOGWOUNG 

library(readxl)
Table_de_conversion<- read_excel(
  "E:/ISEP 2/MON DOSSIER/SEMESTRE 2/TRAITEMENTS STATISTIQUES AVEC R/ISEPcoursR2024/Table de conversion phase 2.xlsx")

Table_de_conversion$...8 <- NULL
Table_de_conversion$...9 <- NULL

# Fusion des base cerales et table de conversion

library(haven)
cereales <- read_dta(paste0("E:/ISEP 2/MON DOSSIER/SEMESTRE 2/TRAITEMENTS STATISTIQUES AVEC R/ISEPcoursR2024/cereales.dta"))

colnames(cereales)[4:14] <- c("AutresCereales","Qtty_cons",
                              "Unite_cons","Taille_cons",
                              "AutoCons","AutresProv",
                              "DernierAchat","Qtty_achat",
                              "Unite_achat","Taille_achat",
                              "Value_achat")

mergee <- merge(cereales, Table_de_conversion, 
               by.x=c("cereales__id", "Unite_achat", "Taille_achat"),
               by.y=c("produitID", "uniteID", "tailleID"),
               all.x = TRUE)

# Verifions s'il y a les valeurs manquantes 

anyNA(mergee$Qtty_achat)  # Ca renvoie true donc il y en a et on va donc les supprimer 

# Supprimons donc ces valeurs manquantes 
mergee <- mergee[!(is.na(mergee$Qtty_achat)),]

# Calculons la quantité consommée en kg

library(data.table)

cereales4 <- data.table(mergee)
setnames(cereales4,"poids","poids_achat") # Pour renommer la colonne poids en poids acheté

cereales4[,poids_achat:=as.numeric(poids_achat)]  # Pour convertir en numerique 
cereales4[,qtty_achat_kg:= poids_achat*Qtty_achat/1000]  # On divise par 1000 car les quanntités sont en gramme

# Calculons le prix unitaire 

cereales4[,prix_unit:= Value_achat/qtty_achat_kg] 

# Calculons les dépenses de consommation 
     ### RECONDUISONS LA QUANTITE TOTALE CONSOMMEE 
cereales4[,qtty_cons_kg:= poids_achat*Qtty_cons/1000] 
     ### Calcul des depenses de consommation proprement dites 
cereales4[,depen_cons:= prix_unit*qtty_cons_kg]  


#Correction des valeurs aberrantes

# Calcul de l'intervalle interquartile
   # On supprime au préalable les valeurs manquantes 
cereales4 <- cereales4[!(is.na(cereales4$Qtty_achat)),]


Q1 <- quantile(cereales4$qtty_achat_kg, 0.25)
Q3 <- quantile(cereales4$qtty_achat_kg, 0.75)
IQR <- Q3 - Q1

# Définition des limites pour les valeurs aberrantes
upper_bound <- Q3 + 1.5 * IQR
lower_bound <- Q1 - 1.5 * IQR
 
# Suppression des valeurs aberrantes
cereales4 <- cereales4[cereales4$qtty_achat_kg >= lower_bound 
                       & cereales4$qtty_achat_kg <= upper_bound, ]
dim(cereales4) #  Ca quitte de 11114 obs au depart à 9495 quand on enleve les valeurs manquantes puis à 8031 lorsque l'on supprime les valeurs aberrantes