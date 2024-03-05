#Exercice de maison numéro3
#1:completer votre base en ajoutant des varibles(15) et des observations(300)
#2:Ecrire un code import/exprort ; renommer ; faire des stats desc sur 
#les variables
#Exercice2
##Implementer manuellement le test de khi-deux


# Installons les packages nécessaires
install.packages(c("dplyr", "random"))

# Chargeons les packages installés
library(dplyr)
library(random)

# Fixons le nombre d'observations à 500 observations
n <- 500

Mes_Donnees <- data.frame(
  Sexe = sample(c("Homme", "Femme"), n, replace = TRUE),
  Age = sample(18:100, n, replace = TRUE), # Age en années
  Statut_Matrimonial = sample(c("Celib","Marié","Divorcé","Veuf_Veuve"),n, replace = TRUE),
  Taille = rnorm(n, mean=160, sd=10), # Taille en cm
  Poids = rnorm(n, mean=70, sd=15), # Poids en kg
  Niveau_De_Revenu = sample(c("Pauvre", "Moyen", "Elevé","Tres elevé"), n, replace = TRUE), # Revenu
  Profession = sample(c("Etudiant", "Enseignant", "Medecin","Ingenieur","Avocat","Comptable","Vendeur","Statisticien"), n, replace = TRUE),
  Ville = sample(c("Dakar", "Thiès", "Kaolack", "Ziguinchor", "Touba"), n, replace = TRUE),
  Milieu_De_Residence=sample(c("Rural","Urbain"), n, replace = TRUE),
  Education = sample(c("Primaire","Secondaire","Supérieur","Pas fréquenté"), n, replace = TRUE),
  Nombre_Enfants = sample(0:5, n, replace = TRUE), # Nombre d'enfants
  Sportif = sample(c("Oui", "Non"), n, replace = TRUE),
  GroupeSanguin = sample(c("A", "B", "AB", "O"), n, replace = TRUE), # Groupe sanguin
  Etat_De_Santé=sample(c("Bonne santé","Tres bonne santé","Malade","Handicapé"), n, replace = TRUE),
  Langues_parlées=sample(c("Français","Anglais","Wolof","Autres"), n, replace = TRUE),
  TypeLogement = sample(c("Maison", "Appartement", "Studio"), n, replace = TRUE) # Type de logement
)

# Installation d'un package nécessaire
install.packages("writexl")

# Chargeons le package
library(writexl)

# Exportons la base de données en format Excel
write_xlsx(Mes_Donnees, "MaBaseExcel.xlsx")

# Définissons les limites des classes pour l'âge, la taille et le poids
limites_age <- seq(0, 100, by = 10) # Classes d'âge de 10 ans
limites_taille <- seq(130, 200, by = 10) # Classes de taille de 10 cm
limites_poids <- seq(30, 120, by = 10) # Classes de poids de 10 kg

# Définissons les labels pour chaque classe
labels_age <- paste("[", head(limites_age, -1), ";", tail(limites_age, -1), "]", sep = "")
labels_taille <- paste("[", head(limites_taille, -1), ";", tail(limites_taille, -1), "]", sep = "")
labels_poids <- paste("[", head(limites_poids, -1), ";", tail(limites_poids, -1), "]", sep = "")

# Créons des variables de classe pour l'âge, la taille et le poids
Mes_Donnees$ClasseAge <- cut(Mes_Donnees$Age, breaks = limites_age, include.lowest = TRUE, labels = labels_age)
Mes_Donnees$ClasseTaille <- cut(Mes_Donnees$Taille, breaks = limites_taille, include.lowest = TRUE, labels = labels_taille)
Mes_Donnees$ClassePoids <- cut(Mes_Donnees$Poids, breaks = limites_poids, include.lowest = TRUE, labels = labels_poids)


# Installation de packages nécessaires
install.packages(c("dplyr", "ggplot2"))

# Chargeons les packages
library(dplyr)
library(ggplot2)

# Calculons la fréquence de chaque classe d'âge
freq_age <- table(Mes_Donnees$ClasseAge)
print(freq_age)

# Calculons la fréquence de chaque classe de taille
freq_taille <- table(Mes_Donnees$ClasseTaille)
print(freq_taille)

# Calculons la fréquence de chaque classe de poids
freq_poids <- table(Mes_Donnees$ClassePoids)
print(freq_poids)

# Créeons un histogramme des classes d'âge
ggplot(Mes_Donnees, aes(x=ClasseAge)) +
  geom_histogram(stat="count", fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  labs(title="Distribution des classes d'âge", x="Classe d'âge", y="Fréquence")

# Créons un histogramme des classes de taille
ggplot(Mes_Donnees, aes(x=ClasseTaille)) +
  geom_histogram(stat="count", fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  labs(title="Distribution des classes de taille", x="Classe de taille", y="Fréquence")

# Créons un histogramme des classes de poids
ggplot(Mes_Donnees, aes(x=ClassePoids)) +
  geom_histogram(stat="count", fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  labs(title="Distribution des classes de poids", x="Classe de poids", y="Fréquence")

# Calculons le tableau de contingence
tableau <- table(Mes_Donnees$Sexe, Mes_Donnees$Sportif)

# Affichons le tableau de contingence
print(tableau)

# Calculons les totaux marginaux
total_lignes <- rowSums(tableau)
total_colonnes <- colSums(tableau)
total_general <- sum(tableau)

# Calculons le tableau des fréquences attendues
attendu <- outer(total_lignes, total_colonnes) / total_general

# Affichons le tableau des fréquences attendues
print(attendu)

# Calculons la statistique du test du khi-2
khi2 <- sum((tableau - attendu)^2 / attendu)

# Affichons la statistique du test du khi-2
print(khi2)