 ################## DEVOIR 8 DE R TRAITEMENTS STATISTIQUES AVEC R 
#' calculer la quantite achete en kg; 
#' calculer le prix unitaire ;
#' Calculer les depenses de consommations ; 
#' Valeurs aberrantes :: corrections 



# Imporation de la base Table de conversion

library(readxl)
Table_de_conversion<- read_excel(
  "E:/ISEP 2/MON DOSSIER/SEMESTRE 2/TRAITEMENTS STATISTIQUES AVEC R/DOC EHCVM/Table_de_conversion_phase_2_1.xlsx")

Table_de_conversion$...8 <- NULL
Table_de_conversion$...9 <- NULL

# Rexonfiguration des variables dans ma table de conversion pour pouvoir fare le merge 
colnames(Table_de_conversion)[1:6] <- c("cereales__id","Nom_Prod",
                                        "Unite_cons","Nom_Unite",
                                        "Taille_cons","Nom_Taille")

# Fusion des base cerales et table de conversion
library(haven)
cereales <- read_dta(paste0("DOC EHCVM/cereales.dta"))

merge <- merge(cereales, Table_de_conversion, 
               by = c("cereales__id", "Unite_cons", "Taille_cons"), all.x = TRUE)

# Calculons la quantité consommée en kg

library(data.table)
cereales3 <- data.table(merge)
setnames(cereales3,"poids","poids_cons")

cereales3[,poids_cons:=as.numeric(poids_cons)] 
cereales3[,qtty_cons_kg:= poids_cons*Qtty_cons/1000] 
cereales3[,summary(qtty_cons_kg)] 

# Calculons la quantité achetée en Kg

cereales3[,qtty_achat_kg:= poids_cons*Qtty_achat/1000] 
cereales3[,summary(qtty_achat_kg)] 

# Calculons le prix unitaire 

cereales3[,prix_unit:= Value_achat/qtty_achat_kg] 
cereales3[,summary(prix_unit)] 

# Calculons les dépenses de consommation 

cereales3[,depen_cons:= prix_unit*qtty_cons_kg]  
cereales3[,summary(depen_cons)] 

#Correction des valeurs aberrantes

# Calcul de l'intervalle interquartile
Q1 <- quantile(cereales3$qtty_cons_kg, 0.25)
Q3 <- quantile(cereales3$qtty_cons_kg, 0.75)
IQR <- Q3 - Q1

# Définition des limites pour les valeurs aberrantes
upper_bound <- Q3 + 1.5 * IQR
lower_bound <- Q1 - 1.5 * IQR

# Suppression des valeurs aberrantes
cereales3 <- cereales3[cereales3$qtty_cons_kg >= lower_bound 
                       & cereales3$qtty_cons_kg <= upper_bound, ]

