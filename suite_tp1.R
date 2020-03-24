####### Exercice 2-bis ######

# Estimer l'erreur de g??n??ralisation du mod??le de r??gression utilisant les 10 variables ?? notre disposition.
# Ce mod??le semble t'il meilleur que celui avec x2 seulement ?


source("./fonctions_tp_reg.R")

ozone = read.table("./ozone.txt", header = T)
ozone_n = read.table("./ozone_n.txt", header = T)
mod1 = myreg(ozone, c(2:11),1)

eqm1=separation(ozone,c(2:11),1);
eqm1# 270.4734

eqm_stable(ozone,c(2:11),1,100)
Oui c'est meilleur que x2 seul

######## Exercice 3 : Selection ascendante

## Partie 1 : cr??ation de la fonction de s??lection 




# Compl??tez la fonction selec_asc du fichier fonctions_tp_reg.R pour r??aliser une s??lection de variable ascendante
# avec le crit??re de l'erreur de g??n??ralisation et comme crit??re d'arr??t le crit??re classique (arr??t d??s que 
# l'erreur remonte pour la premi??re fois)
# Aide : s'aider du pseudo-algorithme que j'ai donn?? en cours.
# Pour chercher la valeur minimale d'un vecteur v --> min(v), 
# et la position de ce minimum dans v est donn??e par which.min(v)
# Je vous conseille de renvoyer une matrice ?? 2 lignes et autant de colonnes que de variables s??lectionn??es (avant l'arr??t).
# La premi??re ligne contiendra les indices des variables que vous choisissez au fur et ?? mesure
# La dexi??me colonne contiendra les erreurs de g??n??ralisation correspondantes.
# Si vous reprenez l'exemple de l'exercice 5 fait en cours, la matrice renvoy??e par selec_asc sera : 
#   1     4    2     3
# 7.77  5.81  5.80  5.77
# Ici on s'arr??te car il n'y a plus de variables (l'erreur de g??n??ralisation diminue ?? chaque ??tape de l'algorithme), mais
# ce ne sera pas toujours le cas.
# La commande pour cr??er une matrice est  M = matrix(nrow= ??, ncol= ??) o?? nrow repr??sente le nombre de ligne de la matrice
# et ncol le nombre de colonnes




## Partie 2 : mise en pratique de la s??lection ascendante (cf cours)

source("./fonctions_tp_reg.R")

selec_asc (ozone, c(2:11), 1, 100)

# A partir de cet exercice, on va arr??ter de "tricher", on ne va plus consid??rer l'ensemble
# ozone_n comme les individus que l'on devra pr??dire. On va simplement regrouper les 2 jeux de donn??es
# que l'on a : ozone et ozone_n, et tenter de choisir le meilleur mod??le pour pr??dire y ?? l'aide de ce 
# jeu de donn??es.
ozone = rbind(ozone, ozone_n) # on regroupe les 2 jeux
dim(ozone) # il y a maintenant 111 individus dans ozone.

# Pour faire de la s??lection ascendante, il faut (comme pr??cis?? en cours) mettre de cot?? un jeu de test
# On va choisir ici de mettre 17 individus (environ 15% de ozone) de cot?? dans un jeu de test
# A vous de jouer, vous devez cr??er ozone_test (17 individus au hasard) et ozone_app (tous les autres)

ap_index=sample(nrow(ozone),floor(nrow(ozone)*15/100))
	ozone_app=ozone[ap_index,]
	ozone_test=ozone[-ap_index,] 

# ?? compl??ter

# Ensuite, appliquer l'algorithme de s??lection au jeu d'apprentissage uniquement.
# Quelles sont les variables s??lectionn??es ?

selec_asc (ozone_app, c(2:11), 1, 100)

         [,1]     [,2]    [,3]
[1,]   2.0000  11.0000   5.000
[2,] 268.1876 190.2123 141.854


# Cr??er un mod??le avec ces variables ?? partir du jeu d'apprentissage.

mod_3variables= myreg(ozone_app, c(2,11,5),1);

 prediction=predict(mod_3variables, ozone_test)
 eq=prediction-ozone_test$y
 mean(eq^2)

# Calculer l'EQM de ce mod??le sur le jeu de test (pr??dire le y du jeu de test et calculer l'EQM)
# Retenez cette valeur, elle correspond ?? l'estimation de la performance de ce mod??le.

	mod= myreg(ozone_app, c(2,11,5),1);
	prediction=predict( mod,ozone_test);
 	eq=prediction-ozone_test[,1]
	eqm=mean(eq^2);
	eqm

# Essayez une nouvelle s??lection ascendante. Si les variables s??lectionn??es sont diff??rentes, 
# estimer la performance de ce nouveau mod??le sur le jeu de test.

# Faites cette proc??dure plusieurs fois, et d??terminer le meilleur mod??le pour pr??dire y








