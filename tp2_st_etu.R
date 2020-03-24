# Chargement des librairies nécéssaires
library(stats)
library(expsmooth)
library(forecast)

########## Première méthode ######################
####   Régression linéaire sur tout le passé  series avec tendance !!!####
############################################led_best#####
 
# Chargement de la série DAX

dax = read.table("./dax.txt", header=  T)

# L'option header = T (true) signifie qu'il y a des en-têtes
# pour les noms de colonne dans le fichier txt

# Inspection du début de la série

head(dax)#Valeurs

# Cela vous donne les 6 premières valeurs de la série.

# Question : Comment s'appele la colonne qui contient les valeurs de la série ?
Valeurs

# Question : Combien de points (ou de valeurs) comporte la série dax?

1841

# Question : Afficher la série à l'aide de la commande plot. Choisissez le type
# de ligne qui vous semble le plus adapté

plot(dax$Valeurs, typ = "b")

# Question :
# En regardant le graphe de la série, pensez-vous qu'il y ait une tendance
# et/ou une saisonnalité?

oui tendance mais pas de saisonnalite

# Question : Tracer le corrélogramme de la série dax.

acf(dax, lag.max = 1841)

# Quelle conclusion pouvez-vous tirer ?

tendance car long temps les valeurs sont positive


# Vous allez maintenant appliquer les méthodes 
# vues en cours pour faire de la prédiction avec tendance mais sans saisonnalité, c’est à dire :

#  — prévision par régression linéaire sur toutes les valeurs passées
#  — prévision par régression linaire sur les k dernières valeurs
#  — prévision par lissage exponentiel double

###### 1ère méthode : prédiction par régression linéaire sur toutes les valeurs passées:

### On commence par une tendance linéaire (une droite), même si cela n'a pas l'air
# adapté à notre série

# La variable prédictrice est la variable qui représente le temps : c'est un 
# vecteur qui comporte les valeurs 1,2,3, ..., jusqu'au nombre de valeurs de la série

t = c(1:length(dax$Valeurs))# vecteur de temps 

#Le modèle de régression que l'on construit tente d'expliquer la série dax$Valeurs
# en fonction de t:

modele1= lm(dax$Valeurs~t) #modele lineaire
modele1#955.085        1.675  


# Ce modèle étant une droite qui modélise dax en fonction du temps, son équation est donc:

# dax = a + b*t

# Les valeurs de a et b sont les coefficients du modèle et sont donnés par 

modele1$coefficients

# Question = Quelle est l'équation de la droite que vous venez d'estimer ?

dax=955.084893 + 1.674871 t

# Affichage de la série, ainsi que l'estimation de sa tendance 

plot(dax$Valeurs, typ = "l")

abline(modele1, col = "red")# afficher la droite du modele lineaire

# Question : Ce modèle semble t'il adapté ?

no car il passe pas par tous les points

# La dernière obsevation que l'on a de cette série est celle de l'instant t=1841.

# Si on veux prédire le prochain point de cette série, il faut prédire l'instant t = 1842.
# Pour cela, il faut créer une data frame contenant la valeur de t pour laquelle
# on souhaite prédire les valeurs de dax et indiquer le nom de la variable
# explicative (ici t)

newdata = data.frame("t"=1842) # l'instant pour lequel on veut prédire 
prediction = predict(modele1, newdata) # on fait la prédiction
prediction#4040.198 : pour l'instant 1842 ( prochaine valeur du serie ) 


# Question : D'après l'équation du modèle, quelle doit être la prédiction à l'instant t=1842 ?

v=955.084893 + (1.674871 *1842 )

4040.197

# Vérifiez que cette valeur est bien celle donnée par la commande predict

# Affichage de la série et de la prédiction que l'on vient de faire

plot(dax$Valeurs, typ = "l", xlim = c(1,1842), ylim = c(1500,6200))# affiche serie 

lines(modele1$fitted.values, col = "red")# afficher modele lineaire (droite)

points(c(1842),4040.198, col = "blue", pch = 3)# afficher la valeur de prediction du point 1842 sous forme de +

modele1$fitted.values# afficher tous les prediction de la series faite par le modele lineaire


# Pour prédire plusieurs points suivants, il faut suivre la même démarche:

newdata = data.frame("t"=c(1842:1900)) # si on veut les 59 suivants par exemple
prediction = predict(modele1, newdata) # on fait la prédiction
prediction

# Question : Afficher la série et les prédictions que l'on vient de faire

plot(dax$Valeurs, typ = "l", xlim = c(1,1900), ylim = c(1500,6200))# afficher serie
lines(modele1$fitted.values, col = "red")# afficher model
points(c(1842:1900),prediction, col = "blue", pch = 3)# afficher les prediction


# On voudrait maintenant estimer la performance de cette méthode (regréssion linéaire sur tout le passé)
# sur ces données. Pour cela, il faut faire des prédictions pour des points que l'on connait, afin
# de calculer ensuite un erreur quadratique.

# Question : en vous aidant des commandes ci-dessus, créer un modèle pour prédire la dernière valeur
# de la série dax (i.e. pour t = 1841). Attention, le point pour t = 1841 ne doit pas être donné pour
# apprendre la droite de régression.
# Prédire la dernière valeur de la série avec ce modèle et calculer l'EQ
#-2145.892 


#prediction du valeur 1841
fin=length(dax$Valeurs)-2
t = c(1:fin)# vecteur de temps 

modele1= lm(dax$Valeurs[1:fin]~t) #modele lineaire

newdata = data.frame("t"=1841) # si on veut les 59 suivants par exemple
prediction = predict(modele1, newdata) # on fait la prédiction
prediction#4038.523 

# calculer l'erreur 

diff= prediction - dax$Valeurs[1841]

diff# -2147.567 

#eqm 
diff ^2 #4612044


# On peut refaire ça pour t = 1840, 1839, 1838, ....
# De cette façon, on va avoir beaucoup d'estimation des erreurs faites par le modèle.
# On pourra donc mieux estimer sa performance globale.
# On va maintenant faire ça de façon un peu plus automatique : 
# Question : écrire (dans le fichier fonctions_tp1_st.R), une fonction 
# prediction_lineaire(s, i)
# qui prend en paramètre une série s, et prédit la ième valeur de cette série par la méthode 
# de régression linéaire sur tout le passé (c'est à dire avant i strictement)

 predictionLm=function(s,x)
{
	fin=x-1
	t = c(1:fin)# vecteur de temps 

	modele1= lm(s[1:fin]~t)# modele lineaire
	newdata = data.frame("t"=x) # l'instant pour lequel on veut prÃ©dire 
	prediction = predict(modele1, newdata) # on fait la prÃ©diction

	return (prediction)
}

 predictionLm(dax$Valeurs,1841)
#4038.523 

# Question : à l'aide de la commande sapply() (idem TP 1), créer un vecteur qui contient les 
# prédictions aux instants t = 5,6,7, ... , 1841 par cette méthode.
# Puis calculer l'EQM de ces prédictions. Attention à bien comparer une prédiction avec la valeur 
# de la série à l'instant correspondant.

source("./fonctions_tp1_st.R")

prediction=sapply(c(5:1841), function(x){
	predictionLm(dax$Valeurs,x)
})


eqmpredictionlinneaire = eqm(prediction,dax$Valeurs[5:1841])

eqmpredictionlinneaire #289944.4

# Retenez cette EQM, vous la comparerez avec la performance des autres méthodes ensuite.

##### On va maintenant estimer la tendance par un polynome de degré 2:

modele2 = lm(dax$Valeurs~poly(t,2, raw=T))# generer modele polynomiale de degree 2

# maintenant on a deux variables explicatives : t et t^2 (que l'on doit noter I(t^2))
# Le modèle estimé est donc un polynome de degré 2 : 

# dax = a + b*t + c*t^2

# Les coefficients du modèle sont donnés par 

modele2$coefficients

1.956e+03            -1.584e+00             1.769e-03 

# Question : Quelle est l'équation du modèle ?

dax = 1.956e+03 + -1.584e+00*t + 1.769e-03 +t^2

# Affichage de la série et du modèle:

plot(dax$Valeurs, typ = 'l')
lines(modele2$fitted.values, col = "red")

# Est ce que ça semble convenir ?
Oui �a semble convenir

# Pour prédire la (ou les) prochaine(s) valeur(s) de la série, il faut 
# utiliser comme tout à l'heure un data.frame contenant les valeurs des instants
# pour lesquels on souhaite prédire, et ensuite utiliser la fonction predict


# Question : Afficher sur un même graphique, la série, le polynôme que vous
# avez créé et les 19 prochaines prédictions faites par ce polynôme.

newdata = data.frame("t"=c(1842:1861)) # si on veut les 19 suivants par exemple
prediction = predict(modele2, newdata) # on fait la prédiction
prediction

plot(dax$Valeurs, typ = 'l')
lines(modele2$fitted.values, col = "red")

points(c(1842:1861),prediction, col = "blue", pch = 3)

# Question : Refaites la même procédure pour un polynôme de degré 3, puis 5, 
# puis d'autres valeurs encore plus grandes. Afficher à chaque fois
# la série, le polynôme et les prédictions.
# Qu'observez-vous ?

#Degr�s 3 :

modeled3 = lm(dax$Valeurs~poly(t,3, raw=T))

newdata = data.frame("t"=c(1842:1900)) # si on veut les 59 suivants par exemple
prediction = predict(modeled3, newdata) # on fait la prédiction
prediction

plot(dax$Valeurs, typ = 'l')
lines(modeled3$fitted.values, col = "red")
points(c(1842:1900),prediction, col = "blue", pch = 3)


#D�gr�s 5:

modeled5 = lm(dax$Valeurs~poly(t,5, raw=T))
newdata = data.frame("t"=c(1842:1900)) # si on veut les 59 suivants par exemple
prediction = predict(modeled5, newdata) # on fait la prédiction
prediction

plot(dax$Valeurs, typ = 'l')
lines(modeled5$fitted.values, col = "red")
points(c(1842:1900),prediction, col = "blue", pch = 3)

On observe que le d�gr�s 5 est plus adapt� pour explier la serie

## Maintenant, vous allez effectuer une démarche pour estimer la performance
# d'un prédiceur de type polynomial de degré d (paramètre)
# Pour cela,
# 1) écrire une fonction prediction_polynomiale(s,i,d) qui prédit la
# ième valeur de la série s avec un polynome de degré d
# 2) Calculer les errreurs de prédiction pour différentes valeurs de i (comme précédemment)
# 3) Calculer l'EQM de toutes ces erreurs

prediction_polynomiale= function(s,i,d){
	
	fin=i-1
	t = c(1:fin)
	modele = lm(s[1:fin]~ poly(t,d, raw=T))
	newdata = data.frame("t"=i) 
	prediction = predict(modele, newdata) # on fait la prÃ©diction
	return (prediction)

}

source("./fonctions_tp1_st.R")

prediction_polynomiale(dax$Valeurs,1841,8)#6212.713 : degre 8 de 1841

# predire tous les valeurs

prediction=sapply(c(5:1841), function(x){
	prediction_polynomiale(dax$Valeurs,x,5)
})

# les erreur 
erreurd5= erreur(prediction, dax$Valeurs[5:1841])

# eqm 
eqmd5 = eqm(prediction, dax$Valeurs[5:1841])
eqmd5 #24911.38

# Ensuite, tester différentes valeurs de d, et choisir celle qui vous semble la meilleure


# prediction  k=2

prediction=sapply(c(5:1841), function(x){
	prediction_polynomiale(dax$Valeurs,x,2)
})

eqmd2 = eqm(prediction, dax$Valeurs[5:1841])
eqmd2 #89584.19


# prediction  k=8

prediction=sapply(c(5:1841), function(x){
	prediction_polynomiale(dax$Valeurs,x,8)
})

eqmd8 = eqm(prediction, dax$Valeurs[5:1841])
eqmd8 # 19630.18

# prediction  k=3

prediction=sapply(c(5:1841), function(x){
	prediction_polynomiale(dax$Valeurs,x,3)
})

erreurd3= erreur(prediction, dax$Valeurs[5:1841])

eqmd3 = eqm(prediction, dax$Valeurs[5:1841])
eqmd3 #36472.16


L'eerreur du d�gr�s 8 est plus petit donc on le garde

###### 2ème méthode : prédiction par regression sur les k dernières valeurs


# Refaites la même analyse, mais cette fois-ci en utilisant la méthode
# de prédiction par régression sur les k dernières valeurs.
# C'est très similaire à ce que vous venez de faire, sauf qu'au lieu 
# de prendre tout le passé en considération, vous ne devez prendre que
# les k dernières valeurs. 

# Attention, la variable explicative est toujours le temps, mais cette fois-ci,
# on ne démarre pas à t=1. On démarre à t=length(s) - k + 1 (seulement les k derniers instants) 

# Le plus pratique est de créer une fonction 
# prediction_tendance_k_derniere(s, k, i, d)
# avec s une serie
# k le parametre pour règler le nombre de dernières valeurs qu'on veut garder
# i l'instant pour lequel on veut prédire
# d le degré du polynome pour estimer la tendance
# et qui renvoie la prédiction de la ième valeur de la série

# Ensuite, déterminer quelles valeurs de k et de d semblent les meilleures pour
# cette série

#debut=(length(s) - k ) + 1 
#debut=(i - k ) + 1 

prediction_tendance_k_derniere=function(s, k, i, d)
{
	debut=(i - k) 
	fin = (i-1)
	t = c(debut:fin)

	modele = lm(s[debut:fin]~poly(t,d, raw=T))

	newdata = data.frame("t"=i) 
	prediction = predict(modele, newdata) # on fait la prÃ©diction
	return (prediction)

}

source("./fonctions_tp1_st.R")

# pour k=5 et degre 5

prediction_tendance_k_derniere(dax$Valeurs, 1, 1840, 8)#6108  

# prediction pour tous la serie  avec k=2 et degre=8

prediction=sapply(c(5:1841), function(x){
	prediction_tendance_k_derniere(dax$Valeurs, 2, x, 8)
})

eqmK = eqm(prediction, dax$Valeurs[5:1841])
eqmK #1956.905


###### 3ème méthode : lissage exponentiel double (LED)

# Le LED est très facile d'utilisation avec R, il faut utiliser la commande HoltWinters de la façon suivante:

led = HoltWinters(dax$Valeurs, alpha = 0.5, beta = 0.5, gamma = F)
led

# a 6183.94834
# b   29.99818

# Il faut donner comme paramètres : 
# - 1 série temporelle (ici c'est dax$Valeurs)
# - une valeur pour le paramètre alpha du LED (ici on met 0.5, on essaiera de changer plus tard)
# - une valeur pour le paramètre beta du LED (ici 0.5 pour commencer) 
# - et gamma = F (comme false), car on n'en a pas besoin en LED

# Question : 

# Afficher le modèle obtenu, en tapant led (le nom de la variable qui stocke le modèle) 

led

# Vous devez voir que alpha et beta sont bien à 0.5,  et gamma n'est pas utilisé

# Vous devez également avoir une autre résultat affiché: "Coefficients".

# Ce vecteur contient 2 valeurs a et b qui sont les 2 résultats issus du LED (cf cours, 
# ce sont les derniers a et b calculés)

# On peut récupérer ces valeurs en tapant

led$coefficients

 6183.94834  29.99818 

# Question : Comment peut-on prédire (grâce à a et b) la prochaine valeur de la série (instant t=1842) ? (cf cours)
# Et la valeur suivante (t=1843) ? 

#prediction pour linstant t=1842 :
6183.94834 +29.99818 #6213.947

#prediction pour linstant t=1843 : a + (b*2) 
# 2 : nombre d'annes apres la fin du serie 
6183.94834 + (2*29.99818 )# 6243.945

# La commande predict permet faire les prédicitions des prochaines valeurs en utilisant le modèle obtenu:

predict(led, 1)# 6213.947 : ( t=1842) prochaine valeur
predict(led, 2)#6243.945  : t=1843

# - led correspond au modèle qu'on a estimé
# - et le 2 car je demande les 2 prochaines prédictions ici par exemple.

# Question:
# Vérifier que les prédictions sont bien celles attendues.

# Si vous vous souvenez du fonctionnement du LED, deux coefficients a et b sont 
# estimés à chaque instant de la série, à partir de t = 3. On les a nommés a_t et b_t en cours
# On peut retrouver toutes ces valeurs dans la matrice:

led$fitted

# Les lignes de cette matrice sont les instants temporels t = 3, t = 4, etc..., jusque t = 1841 (dernier
# instant de la série)

# La première colonne donne x^_t, c'est à dire la prédiction du t-ième élément de la série
# La deuxième colonne donne a_(t-1), c'est à dire le coefficient a (level) de l'instant précédent
# La troisième colonne donne b_(t-1), c'est à dire le coefficient b (Trend) de l'instant précédent

# Vous pouvez vérifier que x^_t = a_(t-1) + b_(t-1) comme indiqué dans le cours.

# prediction pour t=1840 ( a1839 +b1839)

x1840= 6118.962 + 2.393701e+01
x1840#6142.899

# Donc vous pouvez récupérer toutes les prédictions intermédiaires x^_t pour t = 3, 4, ...1841 
# par la commande:

led$fitted[,1] # recuere 1 colonne qui correspond au valeurs de prediction

# Question : 
# Calculer l'EQM de toutes ces prédictions.
# Attention, la première prédiction dans led$fitted correspond à t = 3. Il faut donc la 
# comparer à la 3ème valeur de la série. etc...

prediction=led$fitted[,1]# prediction de tous la serie (3 ... 181 )

eqmTed = eqm(prediction, dax$Valeurs[3:1841])
eqmTed #1485.494

# Question : 
# Refaites les mêmes opérations mais en choisissant un autre couple de valeurs alpha, beta (au hasard)
# Comparez les EQM obtenues

# Si vous ne donnez pas de valeurs de alpha et beta dans l'appel à la fonction led, cette fonction 
# va automatiquement sélectionner le couple pour lequel l'EQM est minimale.

led_best= HoltWinters(dax$Valeurs, gamma = F)

led_best

# Question : 
# Quels sont les valeurs de alpha, beta obtenues ?

 alpha: 0.9893016
 beta : 0.01158487
 
# a et b sont les derniere valeur ( a et b ) calculer => a et b du 1841

# Calculer l'EQM associée. Elle doit être inférieure à celles que vous avez obtenues avec les précédents led

prediction=led_best$fitted[,1]
eqmTed = eqm(prediction, dax$Valeurs[3:1841])
eqmTed #978.8969

#### Conclusion : Choix du meilleur modèle pour prédire la série dax
	Le meilleur mod�le est le lissage exponnentiel Double

# Parmi toutes les méthodes que vous avez essayées, quelle est celle qui conduit à la plus petite EQM ?
	Le lissage exponentielle double

# Appliquer cette méthode pour prédire les 50 prochaines valeurs de la série, et afficher les prédictions.

predict(led_best, 50)

