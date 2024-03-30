
############## DEVOIR POUR LA SEANCE 10

#Charger les packages nécessaires
library(readxl)
library(haven)
library(magrittr)
library(data.table)

# Charger les données
library(haven)
Table_de_conversion_phase_2 <- read_excel("E:/ISEP 2/MON DOSSIER/SEMESTRE 2/TRAITEMENTS STATISTIQUES AVEC R/ISEPcoursR2024/Table de conversion phase 2.xlsx")
cereales <- read_dta("E:/ISEP 2/MON DOSSIER/SEMESTRE 2/TRAITEMENTS STATISTIQUES AVEC R/ISEPcoursR2024/cereales.dta")

# Renommer les colonnes dans cereales
colnames(cereales)[4:14] <- c("AutresCereales", "Qtty_cons", "Unite_cons", "Taille_cons", "AutoCons", "AutresProv", "DernierAchat", "Qtty_achat", "Unite_achat", "Taille_achat", "Value_achat")

# Charger le package magrittr
library(magrittr)

# Modifier et manipuler les données avec dplyr et magrittr
library(dplyr)

tableconves <- Table_de_conversion_phase_2 %>%
  select(-c(8, 9)) %>%
  mutate(
    produit = factor(produitID, 
                     levels = produitID,
                     labels = produitNom
    ), 
    unite_cons = factor(uniteID,
                        levels = uniteID,
                        labels = uniteNom),
    taille_cons = factor(tailleID,
                         levels = tailleID,
                         labels = tailleNom)
  )

# Fusionner les données
cereales3 <- merge(cereales, tableconves, 
                   by.x = c("cereales__id","Unite_cons","Taille_cons"),
                   by.y = c("produitID","uniteID","tailleID"),
                   all.x = T)

# Convertir en data.table
library(data.table)
cereales3 <- data.table(cereales3)
setnames(cereales3, "poids", "poids_cons")

# Vérifier NA dans poids
anyNA(cereales3$poids_cons)
sum(is.na(cereales3$poids_cons))

# Calculer la quantité consommée en kg
cereales3$poids_cons <- as.numeric(cereales3$poids_cons)
cereales3$Qtty_cons <- as.numeric(cereales3$Qtty_cons)


# Vérifier si les colonnes sont numériques
is.numeric(cereales3$Qtty_cons)
is.numeric(cereales3$poids_cons)

# Convertir poids en numeric
cereales3[, poids_cons := as.numeric(poids_cons)]
is.numeric(cereales3$poids_cons)

# Résumé
cereales3[, summary(qtty_cons_kg)]

# Trouver les valeurs anormales
cereales3_anormal <- cereales3[qtty_cons_kg > 1000]
glimpse(cereales3_anormal)

# Boxplot
boxplot(cereales3$qtty_cons_kg)

# Calculer la quantité achetée en kg
cereales3 <- cereales3[, Unite_achat := as.double(Unite_achat)]
cereales3 <- cereales3[, Taille_achat := as.double(Taille_achat)]
cereales3 <- cereales3[, cereales__id := as.double(cereales__id)]

cereales4 <- merge(cereales3, tableconves, 
                   by.x = c("cereales__id","Unite_achat","Taille_achat"),
                   by.y = c("produitID","uniteID","tailleID"),
                   all.x = T)

glimpse(cereales4)
cereales4 <- data.table(cereales4)
setnames(cereales4, "poids", "poids_achat")
cereales4 <- cereales4[, poids_achat := as.numeric(poids_achat)]

summary(cereales4$poids_achat)
summary(cereales4[!is.na(cereales4$poids_achat), "poids_achat"])

table(cereales4$tailleNom.y)
test <- cereales4[!is.na(cereales4$poids), "poids"]

cereales4[, qtty_achat_kg := Qtty_achat * poids_achat / 1000]
boxplot(cereales4$qtty_achat_kg)

# Calculer le prix unitaire
cereales4$pu <- cereales4$Value_achat / cereales4$Qtty_achat
cereales4[Unite_achat == 100, summary(pu)]
cereales4[cereales__id < 5 & Unite_achat == 100, summary(pu)]
cereales4[cereales__id < 5 & Unite_achat == 100 & pu < 2000, summary(pu)]

# Traitement des valeurs aberrantes
#boxplot(cereales4$pu)
library(dplyr)

# Calculer le 75e centile de 'pu' pour chaque 'cereales__id'
centiles <- cereales4 %>%
  group_by(cereales__id) %>%
  summarize(q75_pu = quantile(pu, 0.75, na.rm = TRUE))

# Fusionner les centiles avec le dataframe original
cereales4 <- left_join(cereales4, centiles, by = "cereales__id")

# Remplacer les valeurs aberrantes dans 'pu'
cereales4 <- cereales4 %>%
  mutate(pu = ifelse(!is.na(pu) & pu > q75_pu, q75_pu, pu))

# Supprimer la colonne temporaire 'q75_pu'
cereales4 <- cereales4 %>%
  select(-q75_pu)


### Exos proprement dit

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

# En d'autres termes, le "taux de matching" est une mesure de la proportion de vos données sur les céréales pour lesquelles vous avez un prix unitaire.  Il indique à quel point vos données sur les prix correspondent à vos données sur les céréales.  Un taux de matching élevé signifie que vous avez des prix unitaires pour la plupart de vos données sur les céréales,  tandis qu'un taux plus bas indique que vous avez moins de données sur les prix.C'est une mesure importante pour évaluer la qualité de vos données et leur adéquation pour une analyse ultérieure.
 
# Affichons le résultat
print(paste("Taux de matching:", matching_rate)) # 0.854327874752564
                         ## Interprétation
# Un taux de matching de plus de 0,854 signifie que plus de 85,4 % des combinaisons 
# dans votre ensemble de données cereales4 ont un prix unitaire 
# correspondant dans votre ensemble de données prixunitaire.

#' Reflechir a comment valoriser ces quantites n'ayant de prix  

### On peut prendre les prix moyens de la localité/ commune où ces cereales sont vendus

# Calcul du prix unitaire (d'un kg)
cereales4[,prix_kg:=Value_achat/Qtty_achat_tt]

# Calcul des depenses de consommation 
  # Identifier ceux qui n'ont pas de de depenses de consommation
cereales4[,nayant_un_prix:=ifelse(is.na(depenses_cons),1,0)]
# Pour ceux qui n'ont pas de prix, on fera 
cereales4[,depenses_cons:=prix_kg*Qtty_cons*nayant_un_prix]
