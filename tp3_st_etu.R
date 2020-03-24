# Chargement des librairies nécéssaires

library(stats)
library(expsmooth)
library(forecast)

####### TP3 : prédiction d'une série avec tendance ET saisonnalité ########
# Dans ce TP, vous allez appliquer les deux méthodes vues en cours pour ce type de série :

# 1) décomposition
# 2) lissage exponentiel triple

# Nous allons utiliser la série qui est dans le fichier 
# CO2.txt qui représente le niveau moyen de CO2 mensuel dans 
# une ville des Etats Unis pendant 27 ans
# Notre objectif est de prédire la prochaine année (c'est à dire les 12 prochaines valeurs de cette série)

# Chargement de la série 

s = read.table("./co2.txt", header = T)

# Question : Comment s'appele la colonne qui contient les valeurs de la série ? CO2

head(s)

# Question : Combien de points (ou de valeurs) comporte cette série? 300

dim(s)

# Question : Afficher la série à l'aide de la commande plot. Choisissez le type

# de ligne qui vous semble le plus adapté

plot(s$CO2, typ = "l")

# Question :
# En regardant le graphe de la série, pensez-vous qu'il y ait une tendance
# et/ou une saisonnalité?
oui saison et tendance

# Question : Tracer le corrélogramme de cette série.

acf(s, lag.max = 40)

# Quelle conclusion pouvez-vous tirer ?

oui saison et tendance

########## Première partie ######################
####        Prévision par décomposition      ####
#################################################


# Vous allez appliquer dans cette section la méthode de prévision par décomposition vue en cours.
# Les  étapes de cette méthode sont les suivantes :

# 1. estimation non-paramétrique de la tendance par moyenne mobile
# 2. estimation des coefficients saisonniers sur la série sans tendance
# 3. ré-estimation paramétrique de la tendance par régression linéaire sur la série désaisonalisée 
# 4. prévision en utilisant les coefficients estimés


##### Filtrage par moyenne mobile

# Le filtrage par moyenne mobile se fait avec R par la commande ma, à laquelle on doit indiquer 2 paramètres : 
# une série et un entier qui représente l’ordre de la moyenne mobile. 
# Ici la série est CO2, et on choisit un ordre de 12 car la saisonnalité est 12 :
#odre p =k si la serie est saisonalite

CO2_mm = ma(s$CO2,12, centre = T)# estimation de la tendance ( on supprimer la saisonalite)

CO2_mm

# Question : Regarder les valeurs de CO2_mob. Pourquoi des NA apparaissent au début et à la fin ?
car on prend 6 valeur avant et apres chaque element pour calculer les coefficient

# Affichage sur un même graphique la série originale ainsi que la tendance estimée par moyenne mobile :

plot(s$CO2, typ = "l", xlim = c(1,300), ylim = c(310,345))

lines(CO2_mm, col = "red")# affichage du tendance

##### Estimation des coefficients saisonniers sur la série sans tendance.

# Pour estimer les coefficients saisonniers, il faut travailler sur une série dont on a enlevé la tendance. 
# Il faut donc travailler dans cette section avec la série CO2 à laquelle on soustrait la tendance estimée par moyenne mobile :

CO2_st = s$CO2 - CO2_mm # serie sans tendance
CO2_st

# attention il y a des NA dans CO2_st mais c'est normal
# st veut dire sans tendance

# Question : Faites un graphique pour visualiser CO2_st, et vérifier qu'il n 'y a plus de tendance
# dans cette série.
# Tracer le corrélogramme de CO2_st pour vérifier qu'il n y  plus de tendance. Attention, il faut indiquer
 à la commande
# acf de négliger les NA qui sont présents dans CO2_st. Pour cela, il faut ajouter na.action = na.pass dans 
l'appel de acf

plot(CO2_st, typ = "l")

acf(CO2_st,lag.max =40,na.action = na.pass )

# On va maintenant estimer les coefficients saisonniers sur CO2_st. 
# Il y a 12 coefficients saisonniers (car la série est périodique de période 12). Pour les estimer, 
on suit la méthode présentée en cours :

# 1) On range la série CO2_st dans une matrice à 12 colonnes. Attention, cela peut poser problème 
si la longueur de la série n'est pas  multiple de 12 (dans notre exemple, 300 est bien multiple de 12,
 mais ce ne sera pas toujours forcément le cas.)

# Pour gérer ça, on va compléter la série avec des NA jusqu'à ce que sa longueur soit multiple de 12 
(de p en général):

# On calcule d'abord le nombre de NA qu'il faut ajouter:

n_na = ceiling(length(CO2_st)/12)*12- length(CO2_st)# calcul de nombre de Na 

n_na#0

# Ici ça doit faire 0 car la série est déja multiple de 12

# Puis on ajoute autant de NA qu'il faut à la fin de la série:
CO2_st = c(CO2_st, rep(NA, n_na))

# On peut maintenant ranger la série dans une matrice à 12 colonnes
# La colonne 1 représente le mois de Janvier, etc... 

CO2_st_mat = matrix(CO2_st, ncol = 12, byrow = T)# ranger dans une matric

CO2_st_mat 

# On calcule la moyenne de chaque colonne (fonction colMeans): 

coeff = colMeans(CO2_st_mat, na.rm = T) # en enlevant les NA

coeff 

# Question: Que vaut la moyenne de ces coefficients saisonniers ?

mean(coeff )#0.000120081

#-3.684932e-17

# Le vecteur coeff représente les 12 coefficients saisonniers estimés.
# La série saisonnière est simplement la répétition de ces 12 coefficients pendant 25 ans (durée de la série CO2)

CO2_saison = rep(coeff, 25)## Pour les rendre de moyenne nulle, il faut centrer les coefficients 

coeff = coeff - mean(coeff) # maintenant les coeffs sont de moyenne nulle
coeff

mean(coeff) repeter les 12 coef 25 fois

CO2_saison 

# Question : 
# Sur un même graphe, afficher la série CO2_st et la série CO2_saison que l'on vient d'estimer.
# Sont-elles proches ? oui

plot(CO2_st,type="l")# serie sans tendance

lines(CO2_saison,col="blue")# saison saison fabrique


########## Estimation paramétrique de la tendance

# Maintenant, on va pouvoir estimer la tendance de la série CO2 désaisonnalisée, c'est à dire : 

CO2_des = s$CO2 - CO2_saison# serie avec tendance seulement 

# Question : Vérifiez visuellement et avec le corrélogramme que la série CO2_des ne comporte plus de saisonnalité

acf(CO2_des ,lag.max=40)

# Comme la série CO2_des ne comporte pas de saisonnalité, 
# on peut estimer la tendance de cette série en utilisant les méthodes vues dans 
# le cours et lors du TP2

# Question : 
# Commencer par estimer cette tendance par une droite sur toutes les valeurs passées. On

# pourra changer cela par la suite.
# Quelle est l'équation T(t) de la tendance obtenue ?

# prediction par modele lineaire

t = c(1:length(CO2_des))# vecteur de temps

modele1= lm(CO2_des~t)# droite

modele1$coefficients
  
t= 313.43416130  +0.09312074 t # equation de la droite

######## Prédiction des prochaines valeurs:

# Si T(t) correspond à l'équation de la tendance que l'on vient d'estimer, la prédiction
# de la valeur de la série à un instant h donné est égale à 

# T(h) + coeff[1+ (h-1) modulo 12].

# Le modulo s'écrit %% en R
# La dernière valeur de la série CO2 est à t = 300.
 La prochaine valeur  à prédire correspond donc à t = 301.

# Question : Quelle est la prédiction que vous faites à t = 301 ? 
# Aide = utilisez ce qu'on a vu dans le TP2 pour prédire des valeurs en utilisant un modèle de régression existant

#h=301

predi301=313.43416 + (0.09312*301) + coeff[1+ (300 %% 12 )] 

predi301#341.3772

# Et à t = 302 ?

predi302=313.43416 + (0.09312*302) + coeff[1+(301) %% 12] 

predi302#342.1136

# etc ...

# Pour faciliter les choses, je vous conseille d'écrire une fonction 
# prediction_decomposition (s, i, h)
# qui fait toute la méthode que l'on vient de voir et prédit les valeurs 
# i, i+1, ..., i+h de la série s en utilisant tout le passé

source("./fonctions_tp1_st.R")

prediction_decomposition=function (s, i, h,p){

  prediction=sapply(c(i:h), function(x){

	predictionLm(s,x)+coeff[1+(x-1) %% p]
})
 return (prediction)
}

# Afficher sur un même graphe la série C02 et les prédictions des 12 prochaines valeurs

predict=prediction_decomposition(CO2_des,301,312,12)

plot(CO2_des,type="l")

lines(c(301:312),predict,col="blue")
	
# Essayer maintenant avec une tendance polynomiale de degré d (faire une nouvelle fonction peut
# vous aider)

prediction_decomposition_poly=function (s, i, h,d,p){

  prediction=sapply(c(i:h), function(x){

	prediction_polynomiale(s,x,d)+coeff[1+(x-1) %% p]
})
 return (prediction)
}

# Afficher sur un même graphe la série CO2 et les prédictions des 12 prochaines valeurs
# obtenues avec différents degrés de polynôme

predict2=prediction_decomposition_poly(CO2_des,301,312,2,12)
predict5=prediction_decomposition_poly(CO2_des,301,312,5,12)
predict9=prediction_decomposition_poly(CO2_des,301,312,9,12)

plot(CO2_des,type="l")

lines(c(301:312),predict2,col="blue")
lines(c(301:312),predict5,col="red")
lines(c(301:312),predict9,col="yellow")

### Estimation des performances de cette méthode

# Comme lors des deux premiers TP, je vous demande maintenant de réaliser une procédure qui permet 
# d'estimer la performance de la méthode de prédiction par décomposition en fonction
# du degré du polynome utlisé pour estimer la tendance

# L'idée est la même que depuis le début : 

# il faut prédire des valeurs que l'on connait (en utilisant le passé) puis calculer l'erreur
# quadratique moyenne.

# Je vous conseille de faire p prédictions à la fois (où p est la période, ici 12 pour CO2), c'est à dire
# étant donné la série CO2[1:k] (les k premières valeurs de CO2), appliquer la méthode pour prédire
# les valeurs k+1, k+2, ..., k+p et comparer les à la vérité.

# Vous pourrez ensuite appliquer cette procédure pour 
# k = 2*p (on commence avec au moins 2 périodes d'historique)
# k = 3*p
# k = 4*p
# etc...

# L'idée est de créer une (ou plusieurs) fonction(s) qui à partir: 
#  - d'une série s, 
#  - d'une période p (qui correspond à la longueur de la saisonalité de s),
#  - d'un instant k (dernier instant de l'historique, i.e. on utilise s[1:k] pour prédire la suite),
#  - et du degré d du polynôme qui estime la tendance,
# prédit les p prochaines valeurs de la série, i.e. s[k+1], s[k+2], ... , s[k+p]

eqm_ploy=function(s,k,d,p)
{
	predict=prediction_decomposition_poly(s,k,k+p,d,p)#  preduire les valeurs de k jusqu k+p

	eqm=eqm(predict,s[k:(k+p)])

	return (eqm)	
}

#pour historique k=12=p

eqm_ploy(CO2_des,12,1,12)# 6.045646
eqm_ploy(CO2_des,12,5,12)# 3.460365
eqm_ploy(CO2_des,12,9,12)# 3.379607

#pour historique k=24=2*p

eqm_ploy(CO2_des,24,1,12)# 5.7726
eqm_ploy(CO2_des,24,5,12)#4.408383
eqm_ploy(CO2_des,24,9,12)#4.387664

#pour historique k=36 =p*3

eqm_ploy(CO2_des,36,1,12)# 4.776576
eqm_ploy(CO2_des,36,5,12)#4.121622
eqm_ploy(CO2_des,36,9,12)#4.111421

# Quelle est la meilleure performance que vous obtenez pour la série CO2 avec cette méthode de prédiction ?
eqm=3.4
k=p

########## Deuxième partie ######################
####        Lissage exponentiel triple       ####
#################################################


# Vous allez maintenant utiliser la méthode de lissage exponentiel triple (LET).
# Le LET s'applique à une série qui comporte une saisonnalité. Pour indiquer à R qu'une 
# série est saisonnière de période p, il faut créer un objet de type ts (time series) en 
# utilisant la commande ts:

CO2_ts = ts(s$CO2, frequency = 12) # car p = 12 pour cette série

# Ensuite, il faut appliquer la commande HoltWinters comme d'habitude, mais cette fois-ci
# en spécifiant les  valeur des 3 paramètres alpha, beta et gamma du LET:

let = HoltWinters(CO2_ts, alpha = 0.2, beta = 0.2, gamma = 0.7) # par exemple

# Dans l'objet let, vous avez accès à plusieurs valeurs : 
# - let$coefficients vous renvoie les coefficients du LET, c'est à dire a, b, et les 12 coefficients
# saisonniers.
# let$fitted vous donne le level (a), trend (b) et coefficient saisonnier (season) estimé 
# à tous les instants précédants de la série, ainsi qu'une prévision à chaque instant (xhat).
# Vérifiez comment est obtenu xhat à partir des 3 autres colonnes.

let$coefficients

let$fitted

# On peut prédire les prochaines valeurs de la série CO2_ts en utilisant la méthode
# predict:

predictions = predict(let, n.ahead = 12) # pour avoir les 12 prochaines.

# Afficher sur un même graphe la série CO2 et les 12 prédictions obtenues

plot(s$CO2,type="l",ylim=c(315,365),xlim=c(0,350))

lines(c(301:312),predictions ,col="blue")

# Question : 

# Prédire les 12 prochaines valeurs de la série en utilisant un autre triplet (alpha, beta, gamma).

# Afficher sur le même graphe ces nouvelles prédictions en utilisant une autre couleur.


# Si vous ne donnez pas de valeurs de alpha, beta et gamma dans l'appel à la fonction let, cette fonction 

# va automatiquement sélectionner le triplet pour lequel l'EQM est minimale.

let_best= HoltWinters(CO2_ts)
let_best$coefficients
let_best

# Question : 
# Quels sont les valeurs de alpha, beta , gamma obtenues ?
 alpha: 0.5286747
 beta : 0
 gamma: 0.5931482

# Prédire les 12 prochaines valeurs de la série avec ces valeurs, et afficher les prédictions.

predictions = predict(let_best, n.ahead = 12) # pour avoir les 12 prochaines

plot(s$CO2,type="l")

lines(c(301:312),predictions ,col="blue")

###### Estimation de la performance du LET sur la série CO2

# Pour pouvoir comparer équitablement le LET et la méthode par décomposition, il faut procéder de la même façon que dans la partie
# précedente, c'est à dire : 

# à partir d'un historique s[1:k] de la série (avec k multiple de p), prédire les p prochaines valeurs, et calculer l'EQM
# de ces prédictions. Et refaire ça pour différentes valeurs de k (p, 2p, 3p, etc).

# Calculer l'EQM moyen faite par le LET sur la série CO2 en suivant cette procédure (vous pouvez créer une ou plusieurs fonction(s)
# pour faire ça de façon générique)

# Attention, pour utiliser la fonction HoltWinters pour faire du LET, il faut bien transformer d'abord la série avec la 
# fonction ts(..., frequency = p) , ici p = 12.


eqm_let=function(s,k,p)
{
	CO2_ts = ts(s$CO2[1:k], frequency = p)

	let_best= HoltWinters(CO2_ts)

	predictions = predict(let_best, n.ahead = p)

	eqm=eqm(predictions ,s$CO2[k:(k+p)])

	return (eqm)	

}


eqm_let(s,24,12)#1.227065
eqm_let(s,36,12)#1.334885
eqm_let(s,48,12)# 1.288102

##### Question pour un café : 

# En fait, la procédure que vous venez de réaliser n'est pas très précise pour estimer la performance du LET sur les p prochaines prédictions
# Pourquoi, à votre avis ? (pensez aux paramètres alpha, beta et gamma. Choisis t'on vraiment ceux qui semblent être les meilleurs
# pour le problème qui nous intéresse ?)
# Si vous avez compris pourquoi on peut faire mieux, essayez de réfléchir à comment on pourrait faire mieux !


#### Conclusion : Choix du meilleur modèle pour prédire la série CO2

# Parmi toutes les méthodes que vous avez essayées, quelle est celle qui conduit à la plus petite EQM ? 

let conduit � la plus petite eqm

# Appliquer cette méthode pour prédire les 24 prochaines valeurs de la série, et afficher les prédictions.

 
predictions = predict(let_best, n.ahead = 24)
predictions 

plot(s$CO2,type="l")

lines(c(301:324),predictions ,col="blue")


predict=prediction_decomposition_poly(s$CO2,301,324,9,12)

length(predict)

lines(c(301:324),predict,col="red")


# Comparer (visuellement) avec les 24 prochaines prédictions des autres méthodes.


