# EXRCICES DE R 

#Créer une base de données fictive ayant au moins 5 variables avec les types numériques, caractères, facteurs …

Data_frame1 <- data.frame(
  Nom = c("Ange","Aissatou","Awa","Niass","Ameth","Celina","Laurine","Tamsir"),
  Age = c(19,16,20,21,22,25,29,35),
  Sexe = c("M","F","F","M","M","F","F","M"),
  Matim = c("Celib","Marié","Celib","Celib","Marié","Marié","Celib","Marié"),
  Nb_enf = c(0,3,0,0,2,3,0,4)
)
View(Data_frame1)
#créer une matrice à partir de été base de données, renommer les lignes et les colonnes.
Matrice1 <- as.matrix(Data_frame1)
colnames(Matrice1)<- c("Nom","Age","Sexe","Situation_matrimoniale","Nombre_enfants")
rownames(Matrice1)<- c("Individu1","Individu2","Individu3","Individu4","Individu5","Individu6","Individu7","Individu8")

View(Matrice1)

#Faites des statistiques descrptives : moyenne, max, quartiles, ….
summary(Data_frame1)

# Supposons que 'df' est notre DataFrame et 'sexe' est la colonne qui contient le sexe
part <- table(Data_frame1$Sexe)

# Création du diagramme à secteurs
pie(part, labels = names(part), main = "Répartition par sexe")

# Supposons que 'df' est votre DataFrame et 'age' est la colonne qui contient l'âge

breaks <- seq(15, max(Data_frame1$Age), by = 5)
labels <- paste("[", breaks[-length(breaks)], "-", breaks[-1], "]", sep="")
Data_frame1$classe_age <- cut(Data_frame1$Age, breaks = breaks, include.lowest = TRUE, right = FALSE, labels = labels)

# Installation de la bibliothèque ggplot2 si elle n'est pas déjà installée
if (!require(ggplot2)) install.packages('ggplot2')

# Chargement de la bibliothèque ggplot2
library(ggplot2)

# Création du diagramme en barres
ggplot(Data_frame1, aes(x=classe_age)) +
  geom_bar() +
  xlab("Classe d'âge") +
  ylab("Nombre") +
  ggtitle("Diagramme en barres de la variable 'classe_age'")


#Résoudre à la main un problème facile d’optimisation.
# Vecteur de coût
c <- c(-2, -3, -4)

# Matrice de contraintes
A <- matrix(c(3, 2, 1,
              2, 5, 3,
              4, 2, 2), nrow = 3, byrow = TRUE)

# Côtés droits des contraintes
b <- c(10, 15, 18)

# Solution initiale
B <- matrix(c(1, 0, 0,
              0, 1, 0,
              0, 0, 1), nrow = 3, byrow = TRUE)

# Indices des variables de base initiales
solIndexes <- c(1, 2, 3)

# Fonction Simplexe
simplex <- function(c, A, b, B, solIndexes) {
  i = 0
  j = 1
  sum = 0
  max = -1
  min = 1000000
  entryVariable = -1
  exitVariable = -1
  entryVariable.relative = -1
  exitVariable.relative = -1
  cb <- c()
  entryCriterion <- c()
  
  # Etape 1: Initialisation
  invB = solve(B)               # Inversion de la matrice
  xb <- invB %*% b             # Tableau de solution initiale
  
  for(i in solIndexes){        # Tableau des indices de la solution
    cb <- c(cb, c[i])
  }
  cb[is.na(cb)] <- 0
  
  noSolIndexes <- c()          # Indices des variables candidats
  for(i in 1:3){
    if(!i %in% solIndexes){
      noSolIndexes <- c(noSolIndexes, i)
    }
  }
  
  # Itération par l'algorithme
  while(TRUE){
    # Etape 2: Critère d'entrée
    for(i in noSolIndexes){     # On obtient le critère pour décider quelle variable va entrer dans la solution
      ac <- A[, i]
      y <- invB %*% ac
      
      candidateVariableCost = c[i]
      if(is.na(candidateVariableCost))  candidateVariableCost = 0
      entryCriterion <- c(entryCriterion, cb %*% y - candidateVariableCost)
    }
    
    for(i in entryCriterion){   # Maximum (la variable qui va entrer est obtenue)
      if(i <= 0){
        sum = sum + 1
      } else if(i > max){
        max = i
        entryVariable.relative = j
      }
      j = j + 1
    }
    
    if(sum == length(entryCriterion)){ # Une solution optimale a été trouvée
      print("[ Optimal solution ]")
      break
    }
    
    entryVariable = noSolIndexes[entryVariable.relative] # L'index de la variable d'entrée est obtenu
    
    # Etape 3: Critère de sortie
    y <- c()
    sum = 0
    j = 1
    y <- invB %*% A[, entryVariable]
    
    for(i in y){
      if(i <= 0){
        sum = sum + 1
      } else if(xb[j] / i < min){
        min = xb[j] / i
        exitVariable.relative = j
      }
      j = j + 1
    }
    
    exitVariable = solIndexes[exitVariable.relative]
    
    if(sum == length(A[, entryVariable])){
      return("[ Unbounded problem ]")
    }
    
    # Etape 4: La solution est recalculée
    B[, exitVariable.relative] = A[, entryVariable]
    
    invB = solve(B)               # Inverse de la matrice B
    xb <- invB %*% b              # La solution est obtenue
    solIndexes[exitVariable.relative] = entryVariable 
    noSolIndexes[which(noSolIndexes == entryVariable)] = exitVariable
    cb[exitVariable.relative] = c[entryVariable]
    if(is.na(cb[exitVariable.relative]))  cb[exitVariable.relative] = 0
    
    # Les variables temporaires sont nettoyées
    i = 0
    j = 1
    sum = 0
    max = -1
    min = 1000000
    entryVariable = -1
    exitVariable = -1
    entryVariable.relative = -1
    exitVariable.relative = -1
    entryCriterion <- c()
  }
  
  # Retour des valeurs
  z = cb[i] %*% xb[i]
  return(list("Valeur des variables" = xb, "Coût minimal" = z, "Base" = solIndexes))
}

# Exécution de la fonction Simplexe avec les nouvelles données
simplex(c, A, b, B, solIndexes)
