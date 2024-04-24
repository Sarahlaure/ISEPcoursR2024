############## DEVOIR POUR LA SEANCE 10

#Charger les packages nécessaires
library(readxl)
library(haven)
library(magrittr)
library(data.table)

#' Decider de prendre la moyenne ou la mediane de pu, pour chaque combinaison (p,u,t). Vous aurez une base prixunitaire dont chque (p,u,t) aura 
#' un seul prix p 

## Comme on a déjà traité les valeurs aberrantes, on peut prendre la moyenne
prixunitaire_mean <- prixunitaire %>%
  group_by(Unite_achat, Taille_achat) %>%
  summarise(pu_mean = mean(pu, na.rm = TRUE))

# Calculer les dépenses de consommations
cereales4$depenses_cons <- with(cereales4, qtty_achat_kg * pu)

# Evaluer le taux de matching
matching_rate <- nrow(prixunitaire) / nrow(cereales4)

# En d'autres termes, le "taux de matching" est une mesure de la proportion des données sur les céréales pour lesquelles on a un prix unitaire.  Il indique à quel point nos données sur les prix correspondent à vos données sur les céréales.  Un taux de matching élevé signifie que vous avez des prix unitaires pour la plupart de vos données sur les céréales,  tandis qu'un taux plus bas indique que vous avez moins de données sur les prix.C'est une mesure importante pour évaluer la qualité de vos données et leur adéquation pour une analyse ultérieure.

# Affichons le résultat
print(paste("Taux de matching:", matching_rate)) # 0.854327874752564
## Interprétation
# Un taux de matching de plus de 0,854 signifie que plus de 85,4 % des combinaisons 
# dans votre ensemble de données cereales4 ont un prix unitaire 
# correspondant dans votre ensemble de données prixunitaire.

### On peut prendre les prix moyens de la localité/ commune où ces cereales sont vendus

# Calcul du prix unitaire (d'un kg)
cereales4[,prix_kg:=Value_achat/Qtty_achat_tt]

# Calcul des depenses de consommation 
# Identifier ceux qui n'ont pas de de depenses de consommation
cereales4[,nayant_un_prix:=ifelse(is.na(depenses_cons),1,0)]
# Pour ceux qui n'ont pas de prix, on fera 
cereales4[,depenses_cons:=prix_kg*Qtty_cons*nayant_un_prix]


# Imputer les valeurs manquantes par la médiane
cereales4$pu[is.na(cereales_fina$pu)] <- median(cereales_fina$pu, na.rm = TRUE)

#' Valeurs aberrantes :: corrections ; 
Q1 <- quantile(cereales4$pu, 0.25, na.rm = TRUE)
Q3 <- quantile(cereales4$pu, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Remplacer les valeurs aberrantes par la médiane
cereales4$pu[cereales4$pu < (Q1 - 1.5 * IQR) | cereales4$pu > (Q3 + 1.5 * IQR)] <- median(cereales4$pu, na.rm = TRUE)
