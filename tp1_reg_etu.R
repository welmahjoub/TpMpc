source("./fonctions_tp_reg.R")

# Chargement des données : 
ozone = read.table("./ozone.txt", header = T)
# Nous allons travailler durant tout ce TP avec ces données.

# Les individus de ce tableau de données représentent des jours.
# Pour chacun de ces jours, on a relevé plusieurs mesures météorologiques : 
# - y : la valeur maximale de la concentration en ozone (O3) dans la journée
# - x1, x2, et x3 : les températures à 9h, 12h, et 15h
# - x4, x5, et x6 : la nébulosité à 9h, 12h, et 15h
# - x7, x8, et x9 : projection du vent sur l'axe EO à 9h, 12h et 15h
# - x10 : la concentration maximale en ozone de la veille

# L'objectif de ce TP est de mettre en oeuvre les techniques vues
# en cours afin de trouver le meilleur modèle pour prédire y en utilisant x1, x2, ... , x10.

# Je vous donne également 10 individus nouveaux (dans le fichier ozone_n).
# Notre objectif est de faire des prédictions pour ces individus, et les meilleurs possibles !
# Dans ce fichier ozone_n, il y a également la vérité (ce qu'on souhaite prédire). Normalement,
# nous ne sommes pas censés connaître cette vérité. Mais pour illustrer les différentes notions
# de ce TP, on va faire comme si on connaissait la vérité. (ce qui n'arrivera jamais en pratique !)
# Pour charger ces 10 nouveaux individus : 
ozone_n = read.table("./ozone_n.txt", header = T)




# Question : Combien y a t'il de lignes et de colonnes dans le 
dim(ozone)
[1] 101  11
#nbre de ligne 101 et nb colonne 11
# tableau ozone ? Et dans le tableau ozone_n ?
dim(ozone_n)
#nbre de ligne 10 et nbre de colonne 11
# Afficher le début de chacun de ces tableaux (commande head())
 head(ozone)
head(ozone_n)

######## Exercice 1 : Prédiction de y à partir d'une seule variable
# Dans cet exercice, nous supposons que nous n'avons le droit d'utiliser
# qu'une seule variable (parmi les x_i) pour prédire y. L'objectif est de 
# déterminer laquelle semble la plus adaptée.

# On va commencer par créer un modèle pour prédire y en fonction de x1.
# La regression linéaire avec R se fait grâce à la commande lm()
# Elle s'utilise de la façon suivante :
mod = lm(y~x1, data = ozone)
# Cela signifie qu'un modèle de regréssion de y en fonction de x1 
# dans le tableau de données ozone est créé. IL faut évidemment
# qu'existent de telles colonnes dans ozone

# Vous pouvez également utiliser la fonction myreg() du fichier fonctions_tp_reg.R
# Elle sera plus pratique d'utilisation pour les exercices d'un peu plus tard, 
# mais autant l'utiliser tout de suite
# Aller voir dans le fichier pour comprendre sa description et comment l'utiliser.
mod1 = myreg(ozone, 2,1) # cela créé le même modèle que celui avec la commande lm ci dessus

# Affichage des données et de la droite de régression :
plot(ozone$x1, ozone$y)
abline(mod1, col = "red")

# Vous avez accès à différentes informations sur le modèle de regréssion : 
summary(mod1)
(Intercept) -33.0106 
x1            6.7460
 mod$coefficients

# Question : repérez grâce à cette commande les informations suivantes sur le modèle que l'on vient de créer
# - les coefficients du modèle (accès direct via mod$coefficients). Quelle est l'équation du modèle ?

#equation y=-33.0106 +6.7460x1

# - mod$residuals vous donne les valeurs des résidus pour chaque individu qui a servi à créer le modèle. En déduire le SCE_r
mod$residuals

sce_r = sum(mod$residuals^2)
sce_r 
39455.8
# - le coefficient de détermination (summary(mod)$r.squared)
summary(mod)$r.squared
#R^2=0.5159751
# - la valeur test du test de Fisher (summary(mod)$fstatistic). Comparer cette valeur avec le quantile de Fisher qf(...)
# et en déduire que le modèle complet est statistiquement significatif


summary(mod)$fstatistic
  value    numdf    dendf 
105.5349   1.0000  99.0000 

#quantil de fisher avec .95%, nbre de variable p= 1,  et n-p-1=99

qf(.95,1,99)
 3.937117

on a :
105.5349 >>>  3.937117 on rejette H0 =>> x est influe significativement

# Le coeff de détermination et le SCE_r  sont des critères qui permettent de
# comparer la performance de différents modèles de prédiction (avec le même
# nombre de variables uniquement). Gardez en mémoire les valeurs de ces critères pour ce premier modèle.

# Question : 
# Essayer maintenant de créer un modèle (qu'on appellera mod4) pour prédire y en fonction de x4.
# Regardez les valeurs des 2 critères de performance pour ce modèle.
# x4 semble t'elle plus adaptée que x1 pour prédire y ?
res=sapply(c(2:11),function(x){
mod1 = myreg(ozone, x,1);

 summary(mod1)$r.squared;#r2
 
 
})
res
> res des R au carr�
 [1] 0.5159751 0.6128946 0.5973574 0.3781787 0.3969218 0.2256940 0.2600824
 [8] 0.1862145 0.1443667 0.4697289




res=sapply(c(2:11),function(x){
mod1 = myreg(ozone, x,1);

 ssum(mod1$residuals^2);#scer

})

res


#> res des scer
 #[1] 39455.80 31555.31 32821.84 50688.42 49160.56 63118.37 60315.17
 #[8] 66336.59 69747.85 43225.61

R^2X4= 0.3781787 <<< R^2X1

X1 est plus adapt� que x4

res# le plus petit r2  est x9

x2 est plus adapt� car comporte le plus grand R2

# Essayez une par une les autres variables prédictrices (x2, x3, etc...) et conclure
# quant à la variable la plus adaptée pour prédire y.
# Les 2 critères (coefficient de détermination et SCE_R) sont-ils en accord ?
# Aide : commande sapply() permet de faire ça vite.

# Créer un modèle mod_best qui correspond au meilleur modèle que vous venez de trouver.


mod4= myreg(ozone, 5,1);

mod_best= myreg(ozone, 3,1);


# On va maintenant utiliser le jeu de données ozone_n pour vérifier comment se comportent les différents modèles
# que l'on vient de créer pour prédire des nouvelles données.
# La commande

prediction=predict(mod4, ozone_n)

#   1         2         3         4         5 
 90.66269  97.35211  77.28385  77.28385 110.73096 
        6         7         8         9        10 
 77.28385  77.28385 104.04153 124.10980  90.66269 

mod4= myreg(ozone, 5,1);
eq=prediction-ozone_n$y
eqm=mean(eq^2)
eqm# 343.2581

# calculer eqm des 3 modele(1,2et4)

sapply(c(2,3,5),function(x){
 mod = myreg(ozone, x,1);

 prediction=predict(mod, ozone_n)
 eq=prediction-ozone_n$y
 mean(eq^2)

})

# 304.8180 216.8478 343.2581

# c est le model (2) qui le plus petit eqm 
# classement eqm : 2 , 1 puis 4
#classement r2 et scer: 2,1 puis 4

# donc meme classement

# prédit (en utilisant le modèle mod) la variable y (car c'est la variable cible du modèle mod) des individus de ozone_n

# Question : 
# Pour les 3 modèles que l'on vient de créer (mod1, mod4, et mod_best), calculer l'EQM du modèle sur les données de ozone_n
# Autrement dit, utiliser chaque modèle pour prédire, puis comparer les prédictions à la vérité.

# En termes de performance, comment sont classés ces 3 modèles (du meilleur au moins bon) d'après les EQM que vous venez de calculer ?
# Si vous n'avez pas fait d'erreurs, les critères (coefficient de détermination et SCE_R) sont en accord avec ce classement des modèles.
# Tant mieux, cela semble vouloir dire que ces critères peuvent permettre de trouver quel modèle semble être le plus performant (
# normalement on ne peut pas se baser sur l'EQM du nouveau jeu de données ozone_n car on n'est pas censé le connaître)

######### Exercice 1-bis : Comparaison avec un modèle à 2 variables prédictrices

# Créer un modèle qu'on appelle mod_2variables (avec lm ou myreg) pour prédire y en fonction de x2 (la meilleure variable seule) et x1.

 mod_2variables= myreg(ozone, c(2,3),1);
 summary( mod_2variables)$r.squared;#r2 :  0.6154227
 sum( mod_2variables$residuals^2);#scer  : 31349.22

# Calculer les 2 critères de performance (coeff de determination et SCE_r) de ce nouveau modèle.

# Selon ces critères, ce modèle est-il meilleur que les 3 modèles précédants ?

# oui le r2 est augmenter car on a mis plus de variable 
# scer est plus petit que tous les modele 

# Utiliser ce modèle pour prédire y des individus de ozone_n, et calculer l'EQM sur ces données.
# Vous devriez voir que les 2 critères ne sont pas en accord avec cette EQM. 

 prediction=predict( mod_2variables, ozone_n)
 eq=prediction-ozone_n$y
 mean(eq^2)# 225.946

# eqm du modele avec 2 variable et plus grand que le eqm du modele avec la variable x2

# C'est assez embêtant car si l'on suit ces critères on va utilser le modèle à 2 variables, alors qu'a priori il sera 
# moins bon pour prédire des nouvelles données. 

# La raison est (comme je vous l'ai montré en cours) que ces critères favorisent toujours des modèles avec un plus grand
# nombre de variables. Même si on ajoute des variables complétement aléatoires par rapport à y, les critères diront que 
# ces variables permettent d'améliorer le modèle (alors que c'est aberrant).

# Il ne faut donc surtout pas utiliser ces critères pour comparer des modèles qui n'ont pas le même nombre de variables.

# Un critère plus fiable et qui peut être utilisé quelque soit le nombre de variables est l'erreur de généralisation d'un modèle (cf cours)
# Mais il fait être capable de bien l'estimer, c'est l'objet de l'exercice 2.



######## Exercice 2 : Estimation de l'erreur de généralisation d'un modèle par
#### séparation apprentissage / test.

# Nous avons vu en cours que l'erreur de généralisation était un critère plus approprié
# pour comparer les performances de différents modèles de prédiction (notemment avec 
# un nombre de variables différents). Pour estimer l'erreur de généralisation
# d'un modèle, il faut néanmois procéder correctement. La séparation apprentissage/test

# fait partie des techniques qui permettent d'estimer avec fiabilité cette erreur
# de généralisation. Vous allez la mettre en place dans cet exercice afin de sélectionner
# le meilleur modèle à 1 variable suivant le critère de l'erreur de généralisation.
# Rappel de la procédure d'estimation de l'erreur de généralisation par
# séparation apprentissage/test : 
# 1) Choisir aléatoirement 70% (ou 75%) des individus et les mettre dans un ensemble d'apprentissage
# 2) Mettre les individus restants dans un ensemble de test
# 3) Apprendre un modèle de régression (y en fonction de x1 par ex) en utilisant uniquement les individus de l'ensemble 
# d'apprentissage

# 4) Prédire à l'aide de ce modèle la variable y pour tous les individus de l'ensemble de
# test, et calculer l'EQM de ces prédictions. Cette EQM représente l'estimation de
# l'erreur de généralisation du modèle.

# A vous de jouer, étape par étape : 

# Etapes 1 et 2: Créer un ensemble d'apprentissage contenant 70% (aléatoirement) des individus d'ozone
# et un ensemble de test contenant les 30% restants
# Aide : 
# Il y a 101 individus dans ozone, 70% représente donc 70 individus
# La commande sample(101,70) tire au hasard 70 valeurs entre 1 et 101. Ces valeurs
# peuvent vous servir pour représenter les indices des lignes d'ozone que vous
# voulez mettre dans l'ensemble d'apprentissage. 
# Rappel : si vous voulez garder les lignes 4, 12, et 20 d'un tableau tab, vous 
# pouvez procéder comme suit : 

ap_index=sample(101,70)
apprenti=ozone[ap_index,]
test=ozone[-ap_index,]


# idx = c(4,12,20) --> idx est un vecteur contenant les indices des lignes à garder

# tab[idx, ]  récupère les lignes de tab dont les indices sont dans idx.
# et tab[-idx, ] prend toutes les lignes de tab sauf celles dont les indices sont dans idx
# Avec ça, je pense que vous pouvez créer l'ensemble d'apprentissage et de test
# facilement

# Etape 3
# Apprendre un modèle de régression y~x1 avec l'ensemble d'apprentissage.
# Facile

 mod1= myreg(apprenti, 2,1);

# Etape 4 : Utiliser ce modèle pour prédire les valeurs de y des individus de 
# l'ensemble de test. Facile avec la commande predict.

 prediction=predict( mod1,test )

# Calculer l'EQM de ces prédictions.

 eq=prediction-test$y

 mean(eq^2)#  293.2935

# Quelle est l'estimation de l'erreur de généralisation du modèle qui prédit y 
# à partir de x1 ?
 293.2935

# Afin d'automatiser cette procédure, je vous conseille de créer une fonction :
# separation = function(data, idxp, idxc){...}

# qui renvoie une estimation de l'erreur de généralisation d'un modèle de regréssion 
# pour prédire la colonne d'indice idxc de data à partir des colonnes d'indice idxp
# Il vous suffit d'utiliser myreg ainsi que les commandes que vous avez réalisées dans
# les étapes 1 à 4 ci-dessous et de mettre le tout dans le corps de la fonction.
# Attention à bien faire cette fonction générique.
# La signature de la fonction est écrite dans le fichier "fonctions_tp_reg.R"



# Question: Estimer l'erreur de généralisation des 4 modèles que l'on a déjà créés : mod2, mod4, mod_best, mod_2variables
# Quel est le meilleur modèle suivant cette estimation ?

source("./fonctions_tp_reg.R")

eqm1=separation(ozone,2,1);
eqm1#333.6034

sapply(c(3,5,2,c(2,3)),function(x){

  separation(ozone,x,1);
})
# modele 2 , 4 , 1 , modele avec deux variable (1 et 2)
# 306.8703 389.6565 378.6420 586.1540 314.6059

# le plus c est le modele 2 ( best)

# Question : Estimer une deuxième fois ces 4 erreurs de généralisation (procédure
# entière à refaire). Obtient-on les mêmes conclusions ? Est ce normal ?

# non on trouve pas le mme ordre

# Question : 
# Utiliser le critère d'estimation de l'erreur de généralisation par séparation 
# app / test  pour choisir la meilleure variable x1, x2, ..., x10 
# à utiliser pour prédire y.
# Vous pouvez éventuellement créer une fonction qui prend en entrée un 
# tableau de données, l'indice de la variable cible et qui renvoie le 
# meilleur modèle de régression à 1 variable pour prédire cette variable cible
# tout en affichant l'estimation de son erreur de généralisation.
# Aide : la commande sapply (que vous connaissez) peut vous faciliter la tâche

source("./fonctions_tp_reg.R")

meilleurmodele(ozone,1)

meilleurmodele_stable(ozone, c(2:11), 1,100)#2

meilleur_stable(ozone,1)#2

# Refaites une seconde fois toute la procédure qui vous permet
# de choisir la meilleure variable pour prédire y.
# Trouvez vous le même résultat que la première fois ?non


# La séparation aéatoire apprentissage/test n'est pas la façon la plus
# fiable pour estimer l'erreur de généralisation d'un modèle.
# On va maintenant essayer d'améliorer la précision de l'estimation de l'erreur
# de généralisation. 

# Une façon (facile et directe) d'améliorer cette estimation est la suivante :
# On fait plusieurs séparation app/test aléatoires, et on moyenne
# les résultats obtenus pour chaque séparation.

# Question : 
# Modifiez votre fonction separation de façon à améliorier la précision de l'estimation
# Aide : il suffit de faire une boucle for qui fait plusieurs fois ce que vous venez
# de faire, et faire une moyenne à la fin.

# Appliquer maintenant cette méthode d'estimation de l'erreur de généralisation pour choisir
# la meilleure variable (x1 à x10) pour prédire y. 

# Le résultat est-il plus stable ?

# La méthode la plus fiable (et plus jolie) pour estimer l'erreur de généralisation 
# est la validation croisée à K plis présentée en cours. 
# Elle est un peu plus compliquée à mettre en place. 
# Nous y reviendrons un peu plus tard.


pour nous l'eqm qui renvoie 2 est pour la var x2 


