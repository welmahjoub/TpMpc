# Chargement des librairies nécéssaires
library(stats)
library(expsmooth)

source("./fonctions_tp1_st.R")

# Quelques rappels de manipulation de données avec R
# Si vous avez un vecteur v qui contient des valeurs, 
#   length(v) permet de connaitre sa longueur
#   v[ i ] permet de récuperer la i-ème valeur de ce vecteur, si elle existe
#   v[ i:j ] permet de récupérer un vecteur contenant toutes les valeurs de v d'indice entre i et j 
#   Attention à mettre des parenthèses avant ou après les ":" si besoin
#   exemple : v[ i:(i+1) ] pour récupérer les i et i+1 èmes valeurs (v[i:i+1] ne fait pas pareil, vous pourrez essayer)
# Si vous avez deux vecteurs u et v, 
#    - u + v calcule la somme deux à deux des éléments de u et de v
#    - u - v calcule la différence deux à deux des éléments de u et de v
# exemple: 
u = c(4,5,2)
v = c(3,4,8)
u+v
u-v

# Pour tracer un graphe : 
# - plot(y) --> trace la courbe avec le vecteur y en ordonnée, les abscisses étant par défaut 1,2,3,..., length(y)
# - plot(x,y) --> trace la courbe avec le vecteur x en abscisse et y en ordonnée
# - l'option typ = "l" à l'intérieur de la commande plot permet de faire un tracé avec une ligne (sinon c'est des points par défaut)
# - l'option typ = "b" trace ligne ET points
# - lines(x,y) rajoute le tracé de y en fonction de x sur un graphe existant (en ligne)
# - points(x,y) idem mais avec des points



########## Première partie ######################
####   Detection tendance et/ou saisonnalité ####
#################################################

# Chargement de la série fmsales

fms = read.table("./sales.txt", header=  T)

# L'option header = T (true) signifie qu'il y a des en-têtes
# pour les noms de colonne dans le fichier txt

# Inspection du début de la série

head(fms)

# Cela vous donne les 6 premières valeurs de la série.

# Question : Comment s'appele la colonne qui contient les valeurs de la série ?
		Sales

# Taille de la série

dim(fms) : 1 seule colonne qui contient 44 ligne 

# Cette commande renvoie deux valeurs : le nombre de lignes
# et le nombre de colonnes de fms.

# Question : Combien de points comportent la série fms ?
	44

# Pour accéder aux valeurs de fms, il faut utiliser le $ et indiquer le nom
# de la colonne qui contient les valeurs de la série : Sales

fms$Sales

# Cette commande renvoie un vecteur qui contient tous les points de la série

# Pour accéder à un point en particulier, il faut utiliser les [] et indiquer l'
# indice du point qui nous intéresse.
# Par exemple, pour récupérer le 5ème point de la série, on écrit : 

fms$Sales[1]

# Affichage de la série avec la commande plot

plot(fms$Sales, typ = "b")

# Aide sur la cmmande plot.
help(plot)
# Question : 
#En regardant l'aide sur la commande plot, tracer un nouveau 
# graphique avec comme titre de l'axe X "Semaines" et de Y "Ventes".
# Modifier également la valeur du paramètre type pour qu'on voit
# les points et une ligne continue qui passe par tous les points

plot(fms$Sales,type="b",xlab="Semaines",ylab="Ventes")

# Question :
# En regardant le graphe de la série, pensez-vous qu'il y ait une tendance
# et/ou une saisonnalité?

non pas de tendance ,non plus une saison

# La commande acf permet d'afficher le corrélogramme de la série:

acf(fms, lag.max = 40)

# Question : 
# A la lecture de ce corrélogramme, la série fms comporte t'elle 
# une tendance ? une saisonnalité ?

 non pas de tendance ,non plus une saison

# Questions: 
# Charger les deux autres séries (tempdub et CO2)
tempdub = read.table... # à compléter
CO2 = read.table... # à compléter

tempdub = read.table("./TempDub.txt", header=  T)
CO2 = read.table("./CO2.txt", header=  T)



# Quelddim(le est la longueur de tempdub = 144  ? de CO2 =300 ?

dim(tempdub)
dim(CO2)

# Regarder le nom de la colonne qui contient les valeurs
# pour chaque série, vous en aurez besoin ensuite
# Afficher ces deux séries sur deux graphes différents

# Aide: la commande dev.new() permet d'ouvrir une nouvelle 
# fenetre pour tracer un graphique

plot(tempdub$Temp,type="b",xlab="Semaines",ylab="temperature")

dev.new() # ouvrir nouvelle fenetre

plot(CO2$CO2,type="b",xlab="Semaines",ylab="co2")

# Question :
# Pour ces deux nouvelles séries, tracer le corrélogramme 

acf(tempdub$Temp, lag.max = 40)

acf(CO2$CO2, lag.max = 40)

# et indiquer si elles compoortent une tendance et/ou 
# une saisonnalité (de quelle période?).

temp : saison avec p=12
co2 : saison avec p=12 et tendance

# Pour les séries dont vous avez soupconné une saisonnalité, on va 
# essayer de confirmer cela avec l'analyse de variance.

# La fonction analyse_variance qui vous est fournie dans le
# fichier fonctions_tp1_st.R va vous aider pour faire ça.

# Elle prend deux paramètres en entrée:
#  1. une série temporelle (tempdub$Temp par exemple)
#  2. un entier qui correspond à la période de la saisonnalité que vous
# soupçonnez
# Elle renvoie un vecteur contenant 2 valeurs : 

# 1. V_I / V_R (cf slide de cours numéros 36-40 )
# 2. V_P / V_R (--------------------------------)

source(file="./fonctions_tp1_st.R",encoding="UTF-8")

res=analyse_variance(tempdub$Temp,12)
res

# Question : appliquer la fonction analyse_variance à la série tempdub
# avec la période de saisonnalité que vous avez estimé.

# Quelles sont les valeurs renvoyées par cette fonction ? 
 409.510369   1.129906

# Il vous faut maintenant les comparer aux quantiles de la loi de Fischer

# La commande qf(0.95, v1,v2) donne le quantile 
# de Fisher à (v1,v2) degrès de liberté avec confiance de 95%

# rappel de cour :

# ( vi / vr) doit etre > a fichier p-1;(n-1)(p-1) : saisonalite 

# ( vp / vr) doit etre sup a fichier n-1;(n-1)(p-1) : tendance 

qf(0.95, v1,v2) 

# Question : Que donne l'analyse de variance pour tempdub ?
# Et pour CO2

--->Ananlyse de la variance pour Temp : saisonalite 

qf(0.95, 11,143*11) == 1.794724

fisher(11,143*11) : 1.794724

ona 409.510369 > 1.7 donc il ya presence d'une saison pour Temp 

---> Analyse de la variance pour Co2

analyse_variance(co2$CO2,12)  :  
vi/vr=1033.094 
 vp/vr =9822.024

qf(0.95, 11,299*11) : 1.791553

Test pour la saisonnalit� : on VI/VR =1033.094 >>>> 1.179 donc  il y'a pesence d'une saaison

Test pour la tendance :  VP/VR = 9822.024 

qf(0.95, 299,299*11) : 1.145592

on a Vp/VR >>>> a la valeur de fisher 1.14 donc preense de tendance 



########## Deuxième partie ######################
####   Prédiction de séries sans tendance    ####
##########   ni saisonnalité       ##############
#################################################


#Vous allez appliquer dans cette section les méthodes 
#vues en cours pour faire de la prédiction sans tendance ni saisonnalité, c’est à dire :

# — prévision par la moyenne de toutes les valeurs passées
# — prévision par la moyenne mobile des k dernières valeurs
# — prévision par lissage exponentiel simple
# On testera ces méthodes sur une nouvelle série LakeHuron qui ne comporte ni
# tendance ni saisonnalité


# Questions :
# Charger la série LakeHuron qui est dans le fichier huron.txt

""" # à compléter

huron= read.table("./huron.txt", header=  T)
dim(huron) 
82

# Afficher la série sur un graphique

plot(huron$Niveau, typ = "b")

# Vérifier à l'aide du corrélogramme qu'elle ne comporte ni tendance ni saison

acf(huron, lag.max = 40)

###### 1ère méthode : prédiction par moyenne de tout le passé : 

# Si v est un vecteur contenant des valeurs numériques, la commande mean(v)
# permet de calculer la moyenne de tous les éléments du vecteur v.

# Question : 
# En appliquant cette commande à toute la série huron,
# quelle est la prédiciton que vous faites pour la prochaine valeur de cette 
série par cette 1ère méthode ?

mean(huron$Niveau)#578.6526

# Conservez cette valeur (sur une feuille, ou dans une variable)
prediction83 = 578.6526

# On va maintenant afficher sur un même graphique la série huron, ainsi que
 la prédiction que 
# vous venez de faire pour se prochaine valeur. Pour cela, 

#   1) faire un plot(...) de la huron, en indiquant le nouveau range de l'axe des x par l'option 
#   xlim = c(debut, fin) dans la commande plot. Ici début vaut 1, et fin doit valoir la longueur 
de la   série huron + 1 (prédiction de la prochaine valeur à al suite de la série)

 plot(huron$Niveau, typ = "l",xlab="MOIS",ylab="Niveau",xlim=c(1,length(huron$Niveau)+1))


#   2) ajouter la prédiction sur le graphe grace à la commande points(x,y, col = "???"), où x est 
#   l'abscisse du point que vous voulez ajouter au graphique (ici l'indice temporel de cette 
prédiction)
#   et y correspond à la valeur de la prédiction, et à la place des ??? vous pouvez mettre la
 couleur
#   que vous souhaitez (en anglais, par ex blue, red, green, ou cherchez sur google d'autres couleurs
#   dispos dans R, sans y passer une heure!)

 points(83,578.6526, col = "blue")

#   Si vous voulez relier le dernier point de la série avec la prédiction, il faut utiliser la
#   commande lines
# (c(x1,x2), c(y1,y2), col = "???),
# avec x1 l'indice temporel du dernier point de huron, x2 l'indice
#   temporel de la prédiction (x1+1), y1 la valeur du dernier point de la série, 
#y2 la valeur de la   prédiction.

 lines(c(82,83),c(579.96,578.6526) ,col="blue")# relier le point avec la serie 

 Essayez
#   Vous pourrez ajouter cette commande pour enjoliver votre graphe :
#   legend("bottomleft", c("Historique", "Prédiction par moyenne"), col = c("black", "???"), lty = c(1,1))



# Pour évaluer si cette méthode de prédiction est bonne, il faut prédire des valeurs que l'on connaît
# déjà (sans les utiliser pour le modèle) et calculer une erreur quadratique (EQ).

# Par exemple, on peut prédire le dernier point de la série huron, en faisant comme si on ne
# ne le connaissait pas, et comparer cette prédiction avec la vraie valeur de ce point.
# Question :

# Que vaut la prédiction du dernier point de la série Huron par la méthode de moyenne sur toutes valeurs
# passées (attention, on ne doit pas utiliser le dernier point de la série) ?

prediction82=mean(huron$Niveau[1 :81])

prediction82 #578.6364

#verite =579.96

# Pour évaluer si cette prédiction est bonne, on va calculer l'EQ entre la vérité (dernier point
# de la série) et la prédiction que vous venez de faire (dans prediction).
# 1. on fait la différence entre la vérité et la prediction. C'est l'erreur de prédiction


d=prediction82-huron$Niveau[82]# erreur = prediction -verite 
d#-1.32358


# 2. On met au carré cette erreur. Ca donne l'erreur quadratique

d = d^2 # eqm =erreur au carre 
d# 1.751865

# Pour avoir une estimation plus fiable de la performance de cette première méthode de prédiction sur
# la série Huron, il faudrait calculer plusieurs erreurs quadratiques et en faire une moyenne.

# Pour l'instant, on a prédit le dernier point de Huron en utilisant tous les précédents.
# On peut également :

#   - prédire l'avant dernier en utilisant tous ses précédents, et calculer une EQ
#   - prédire l'avant avant dernier , en utilisant tous ses précédents, et calculer une EQ
#   - etc etc

# Globalement, pour une série S = s1, s2, ..., sL de longueur L, on peut calculer :
#   - p2 la prédiction de s2 en utilisant s1, et calculer e1 l'EQ associée
#   - p3 la prédiction de s3 en utilisant s1 et s2, et calculer e2 l'EQ associée
#   - p4 la prédiction de s4 en utilisant s1 s2 et s3, et calculer e3 l'EQ associée
#   - ....

#   - pL la prédiction de sL en utilisant s1 s2, ... sL-1, et calculer eL-1 l'EQ associée
# On peut donc faire L-1 prédictions et calculer L-1 erreurs quadratiques.

# Ces L-1 EQ nous permettent de calculer une erreur quadratique moyenne (EQM)
# associées à ces erreurs. C'est simplement la moyenne de e1, e2, ..., eL-1
# Cette EQM est une estimation plus robuste de la performance d'une méthode de prédiction
# sur une série.

# Question
# C'est ce que je vous demande de réaliser maintenant. Pour cela, je vous conseille d'écrire
# des fonctions intermédiaires qui vont faciliter la compréhension.
# Regardez la fonction analyse_variance du fichier fonctions_tp1_st.R pour comprendre
# la syntaxe de l'écriture d'une fonction en R (nom de la fonction, paramètres, return)

#   1) Ecrire (dans le fichier fonctions_tp1_st.R)
# une fonction prediction_moyenne(s, i) qui calcule la 
#   prédiction (par moyenne de tout le passé) de la i ème valeur d une série s
 # en utilsant    les i-1 premières uniquement (c'est à dire le pi ci-dessus)

# prediction du colonne i = la moyen de tous valeurs dans la serie avant i 

 prediction_moyenne=function(s, i){

   fin=i-1
   m= mean(s[1 : fin])

   return (m)
 }

#   2) Testez cette fonction : 
#     - d'abord recompiler le fichier fonctions_tp1_st.R : source("./fonctions_tp1_st.R")
#     - puis tester la fonction ici même en donnant comme paramètres votre série huron, et un indice 
#     i au choix. Vous pouvez par exemple, essayer pour i le dernier indice temporel et vérifier 
#     que le résultat est bien celui obtenu plus haut.
#  
source("./fonctions_tp1_st.R")

hurron = read.table("./huron.txt", header=  T)

prediction_moyenne(huron$Niveau,82)#578.6364


 3) Cette fonction vous permet de calculer simplement les L-1 prédictions évoquées ci-dessus 
#      (p2, p3, ..., pL,  il faut faire varier le paramètre i de 2 à L). 
#      La commande sapply() permet de faire ça facilement:
#      par exemple:

sapply(c(1:3), function(x){x^2})

 # permet d'appliquer la fonction x^2 pour tous les éléments du vecteur

# 1:3 (c'est à dire 1,2,3)
#      Utiliser sapply pour calculer un vecteur de longueur L-1 (où L est la longueur de la série Huron)
#      et qui comportent les L-1 prédictions dont on a besoin : p2, p3, ...., pL
#   

#on commence par 2 car le 1er element n' a pas de passe 

source("./fonctions_tp1_st.R")

#prediction de toute la serie sauf 1er valeur

prediction=sapply(c(2:82), function(x){
		prediction_moyenne(huron$Niveau,x)})

prediction

4) Ecrire une fonction eqm (prediction, verite) qui prend en paramètre un vecteur
#      qui contient des prédictions et un vecteur verite qui contient les vraies 
valeurs associées àces prédictions et qui calcule l'EQM de toutes ces prédictions.
#      Aidez vous du calcul de l'EQ fait juste au dessus pour calculer toutes 
les EQ (dans un vecteur) entre verite et prediciton
#      puis utiliser la commande mean() appliquée à ce vecteur d'EQ

###
eqm =function(prediction, verite){

	res=(1:length(prediction))
	
	for( i in c(1:length(prediction)) )
      {
	 res[i] =(verite[i] - prediction[i])^2;
	}
	
	 m=mean(res)
	 return (m)
 
 }

###

# 2eme version plus simple et efficace :

eqm =function(prediction, verite){

	 d=prediction-verite

	 d=d^2	

	 m=mean(d)

	 return (m)
 
 }

#   5) Appliquer cette fonction à la série Huron pour évaluer la performance de 
#     cette 1ère méthode de prédiction.
#     Aide :  - la fonction de la question 3) vous permet d'avoir le vecteur contenant les L-1 prédictions
#             - ensuite, il faut que vous récupériez les L-1 "vérités" qui sont dans la série Huron (attention aux indices correspondants)
#             - ensuite appliquer la fonction eqm à ces 2 vecteurs

length(prediction)#81 
length(huron$Niveau[2:82]) # 81

eqmMethod1=eqm (prediction,huron$Niveau[2:82]) 
eqmMethod1 #1.272805

#1.28 valeur correct
# Gardez bien en mémoire (sur un papier, ou dans votre tête ou dans une variable) cette valeur. Elle 
# sera à comparer avec les performances des autres méthodes de prédiction.

# Vous pouvez tracer sur un même graphe la série ainsi que toutes les prédictions intermédiaires que vous venez de calculer
# (attention aux abscisses des prédictions)

plot(huron$Niveau, typ = "b")

# 1er point cest 2

points(c(2:81),prediction,col="blue")# afficher tous les prediction a cote du serie sous forme de point

##sapply(, function(x){points(x+1,prediction[x], col="blue")})

###### 2ème méthode : prédiction par moyenne des k dernières valeurs

# Cette méthode comme son nom l'indique, calcule la moyenne des k dernières valeurs de la série pour
# prédire la prochaine valeur.

k = 5 # on commence par tester cette valeur de k. Vous changerez ensuite

# Question : 
#   - Ecrire une ligne de commande qui permet de récupérer les k dernières valeurs de la série huron
#     Aide : l'indice de la dernière valeur de la série est length(huron$Niveau). Il faut donc aller récupérer les
#     k précédentes (en comptant celle-là). Rappel : Si v est un vecteur, la commande v[a:b] permet de récupérer les 
#     eléments d'indice a, a+1, a+2, ...., b

#   - Calculer la moyenne de ces k dernières valeurs. Cette valeur correpond à la prédiction de la prochaine valeur de
#     la série huron.

# pour predit la valeur du 83 :

#pour k=5

debut=(83- 5) # 78
fin=83-1
valeurs=huron$Niveau[debut :fin]

prediction83=mean(valeurs) #579.484
prediction83

#   - En reprenant les commandes utilisées avec la première méthode, tracez sur un même
#     graphe, la série huron et la prédiction que vous venez de faire pour la prochaine valeur de la série.

 plot(huron$Niveau, typ = "l",xlab="MOIS",ylab="Niveau",xlim=c(1,length(huron$Niveau)+1))

 points(83,prediction83, col = "blue")

#   - Calculer ensuite une nouvelle prédiction pour k = 3 et ajouter sur le graphe (avec une couleur différente)

# meme chose avec k=3

debut=83-3 # 
fin=83-1
valeurs=huron$Niveau[debut :fin]
prediction_k3_83=mean(valeurs) #579.72
prediction_k3_83

points(83,prediction_k3_83, col = "red")

# Question:
#    - Ecrire une fonction prediction_k_valeur(s,i,k) qui calcule la prédiction de la i ème valeur d'une série
#    s en utilisant les k valeurs précédentes.
#    - Utiliser cette fonction pour calculer la prédiction de la prochaine valeur de la serie huron pour k = 5 
#      et k = 3 et vérifier que vous obtenez les mêmes valeurs qu'à la question prcédente


# prediction_k_valeur=function(s,i,k){
#	valeurs=s[(length(s)- k) :i]
#	return (mean(valeurs))}
#

 prediction_k_valeur=function(s,i,k)
{

	debut=(i - k) 

	fin = (i-1)

	valeurs=s[debut :fin]

	return (mean(valeurs))
}

source("./fonctions_tp1_st.R")

prediction_k_valeur(huron$Niveau,83,5)#579.484


# On va maintenant faire une évaluation robuste de la performance de cette méthode sur la série huron 
# Pour cela, on va faire comme avec la 1ère méthode : 
faire des prédictions pour des points que l'on 
# connait (sans les utiliser) et calculer l'erreur quadratique moyenne de ces prédictions.
# 
# Question : 
#   - Sachant que cette méthode nécessite de connaître les k derniers points pour faire une prédiction, 
# combien de prédictions peut-on faire pour une série de longueur L (sans compter la prédiction de
# la prochaine valeur) ?
lenght(s) - k 

#   - A l'aide de la commande sapply(), 
#calculer un vecteur contenant toutes ces prédictions (pour k=5 par ex)
#   - Calculer l'EQM de ces prédictions (en utilisant la fonction eqm que vous avez déjà)

length(huron$Niveau)# 82

prediction2=sapply(c(6:82), function(x){
	 prediction_k_valeur(huron$Niveau,x,5)
})

eqmMethod2=eqm (prediction2,huron$Niveau[6:82])
eqmMethod2

#1.195778 pour k=5
#1.02323 pour k =3


#   - En utilisant les commandes de ces 3 derniers points, écrire une fonction eval_k_valeur(s,k) qui 
#   calcule l'EQM de cette méthode sur une série s pour une valeur de k en paramètre

# a corriger on commence par k+1

# calculer le eqm d'une serie pour k donne 

# debut function

eval_k_valeur = function(s,k){

  debut=k+1
  # calculer prediction en fonction du k
  prediction=sapply(c(debut:length(s)), function(x){
	 prediction_k_valeur(s,x,k)})
  
  valEqm= eqm (prediction,s[debut:length(s)])# puis calculer eqm 
  return (valEqm)

}

# fin function

source("./fonctions_tp1_st.R")

eval_k_valeur(huron$Niveau,5)# 1.195778
eval_k_valeur(huron$Niveau,3)#1.012387

# Vous pouvez tracer sur un même graphe la série ainsi que toutes les prédictions intermédiaires que vous venez de calculer
# (attention aux abscisses des prédictions)

# Question
#  - Appliquer cette méthode à la série huron pour différentes valeurs de k (2, 3, 5, 10, etc) et choisir la valeur de 

eqm=sapply(c(1,2,3,5,10,11,12), function(x){
	 eval_k_valeur(huron$Niveau,x)})

eqm#0.5846272 0.8377659 1.0123873 1.1957778 1.3498153 1.4019346 1.4550609

# k qui est la meilleure pour cette série. Quelle est la prédiction faite par cette méthode pour la prochaine valeur de huron ?

min(pre) => k=21
# eqm =0.5846272

#  - Quelle méthode (moyenne de tout le passsé ou k dernières valeurs) est plus adaptée à cette série huron ?

methode moyenne des  k valuer precedent avec k=1
 

###### 3ème méthode : lissage exponentiel simple (LES)

# Le LES est très facile d'utilisation avec R, il faut utiliser la commande HoltWinters de la façon suivante:

# alphat permet de savoir la distrubtion des coeficient des annees precedent 

les = HoltWinters(huron$Niveau, alpha = 0.5, beta = F, gamma = F)

# Il faut donner comme paramètres : 
# - 1 série temporelle (ici c'est huron$Niveau)
# - une valeur pour le paramètre alpha du LES (ici on met 0.5, on le changera plus tard)
# - puis beta = F (comme false), et gamma = F (comme false), car on n'en a pas besoin en LES

les = HoltWinters(huron$Niveau, alpha = 0.5, beta = F, gamma = F)

# Question : 
# Afficher le modèle obtenu, en tapant les (le nom de la variable qui stocke le modèle) 
# Vous devez voir que la valeur de alpha est bien 0.5, que beta et gamma ne sont pas utilisés.
# Vous devez également avoir une autre valeur appelée "Coefficients".
# Cette valeur est égale à 579.735

# Elle correspond à la dernière valeur de ^x_t (x_t chapeau)  que l'on calcule en suivant
# le procedé du LES, ici c'est ^x_83 car il y a 82 valeurs dans la série
# On peut récupérer cette valeur en tapant

les$coefficients#prediction du valeur 83 : 579.735 

# Question : 
# Sachant que R initialise ^x_1 à la première valeur de la série (ici, c'est 579.14), 

# calculer (en regardant la formule du cours), les deux prochaines valeurs prédites ^x_2 et ^x_3

# Vous pourrez retrouver ces valeurs intermédiaires de ^x_t, c'est à dire ^x_2, ^x_3, ...,^x_82 par la commande

les$fitted # predcition de tous les valeurs du series

# dans la première colonne (xhat qui veut dire x chapeau)

# Vérifier que vos calculs ont bien donné les bonnes valeurs pour ^x_2 et ^x_3

# Vous pouvez tracer sur un même graphe la série ainsi que toutes ces prédictions intermédiaires 

plot(huron$Niveau, typ = "b")

sapply(c(1:82), function(x){points(x+1,les$fitted[x], col="blue")})

# (attention aux abscisses des prédictions)

# Question:

# Calculer l'EQM des 81 prédictions faites par le LES 

eqm3=eqm(les$fitted[,1],huron$Niveau[2:82]) 
eqm3#0.7647434


# Question:

# Essayer maintenant de refaire le LES avec une autre valeur de alpha (comme vous voulez), et noter l'EQM
# correspondant

# Pour trouver la valeur d'alpha la plus adaptée à la série, il faudrait essayer pleins de valeurs de alpha (entre 0 et 1)
# et choisir celle qui conduit à l'EQM la plus faible.
# Vous avez de la chance, la commande HoltWinters peut le faire à votre place.
# Il suffit de ne pas indiquer de valeurs de alpha, et il va chercher tout seul la meilleure !

les_best = HoltWinters(huron$Niveau, beta = F, gamma = F)
les_best

# Question:
# Quelle est la valeur de alpha choisie sur ces données ?
 0.9999339

cof =579.96

# Quelle est l'EQM associée ? Normalement, cette EQM doit être plus faible que les deux que vous avez 

eqm3_best=eqm(les_best$fitted[,1],huron$Niveau[2:82])
eqm3_best
 0.5846399

# essayées avant

# Vous pouvez maintenant prédire la prochaine valeur de la série:

predict(les_best, n.ahead = 1) #579.96

# n.ahead permet de dire combien de prochains points on souhaite prédire. 

# Question:
# Afficher sur un même graphe:
# - la série
# - les prédictions ^x2, ^x3, ..., ^x82 en bleu
# - la prédiction de la prochaine valeur de la série en rouge

prochaineValeur=predict(les_best, n.ahead = 1)

plot(huron$Niveau, typ = "b")

sapply(c(1:82), function(x){points(x,les_best$fitted[x], col="blue")})

points(83,prochaineValeur, col="red")


#### Conclusion : Choix du meilleur modèle pour prédire la série huron


# Parmi toutes les méthodes que vous avez essayées, quelle est celle qui conduit à la plus petite EQM ?
les avec alpha =0.09

# Quelle est donc la prédiction que vous faites pour la prochaine valeur de cette série ?


prochaineValeur=predict(les_best, n.ahead = 1)
579.96# meme valeur que celle avant


