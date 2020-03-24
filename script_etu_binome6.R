library(forecast)
library(stats)
library(expsmooth)

# el mahjoub abdrahmane et yaya simpara : binomes 6 


###############    Exercice 1 :   ##########################
# Chargement des données
ventes = read.table("exo1_binome6.txt", header = T) # Remplacer XX par votre numéro de binome


# VOS REPONSES ICI
# Q1 : 
dim(ventes)#40  1
40points  

Q2 :
plot(ventes$CA, typ = "l",xlab="Semaines",ylab="CA")

Q3 :

acf(ventes, lag.max = 40)
elle comporte rien si saison , ni tendance

Q4 :

Q5 :methode moyenn de tous le passe

# prediction du prochaine  valeurs du series
predi41=mean(ventes$CA)#12.8095

plot(ventes$CA, typ = "l",xlab="Semaines",ylab="CA")

points(41,predi41,col="red")# afficher la prediction 

Q7:EQM

source("./fonctions_exam_tp.R")

prediction=sapply(c(2:length(ventes$CA)), function(x){

	prediction_moyenne(ventes$CA,x);

})

eqm=eqm(prediction,ventes$CA[2:length(ventes$CA)])
eqm#46.82715


Q8: moyen des k valeurs passes

#k=1
prediction=sapply(c(2:length(ventes$CA)), function(x){

	prediction_k_valeur_passe(ventes$CA,x,1);

})

eqm=eqm(prediction,ventes$CA[2:length(ventes$CA)])
eqm#93.17254

source("./fonctions_exam_tp.R")

#k=2

prediction=sapply(c(3:length(ventes$CA)), function(x){

	prediction_k_valeur_passe(ventes$CA,x,2);

})

eqm=eqm(prediction,ventes$CA[3:length(ventes$CA)])
eqm#75.87584

#k=8

prediction=sapply(c(9:length(ventes$CA)), function(x){

	prediction_k_valeur_passe(ventes$CA,x,8);

})

eqm=eqm(prediction,ventes$CA[9:length(ventes$CA)])
eqm#48.26094



Q 9: Mediane

predict41=median(ventes$CA)
predict41#13.635


source("./fonctions_exam_tp.R")

prediction=sapply(c(2:length(ventes$CA)), function(x){

	prediction_mediane(ventes$CA,x);

})

eqm=eqm(prediction,ventes$CA[2:length(ventes$CA)])
eqm#56.12573

######Q10 : LES 

les_best = HoltWinters(ventes$CA, beta = F, gamma = F)
les_best

# alpha: 0.07973347
#a 12.70838

# eqm de les 
source("./fonctions_exam_tp.R")

eqm3_best=eqm(les_best$fitted[,1],ventes$CA[2:40])
eqm3_best#48.59683


prediction=predict(les_best, n.ahead = 1)
prediction#12.70838

Q12 : conclusion

plot(ventes$CA, typ = "l",xlab="Semaines",ylab="CA")

points(41,12.8095,col="red")# afficher la prediction 


###############    Exercice 2 :   ##########################

# Chargement des données

exo2 = read.table("./exo2_binome6.txt", header = T) # Remplacer XX par votre numéro de binome


# VOS REPONSES ICI
# Q1 : 

plot(exo2$y, typ = "l",xlab="Semaines",ylab="y")

acf(exo2 , lag.max = 20)

dim(exo2)#50

# d'apres  le graphique et le corelogramme la serie presente une tendance car on constate une �volution 
#et les traits vertcaux restent longtentemps audessus de la barre horizontale
# donc on a 3 choix possible pour predire les prochaines valeurs  :
# modele lineaire 
# modele polynomiale 
# lissage exponentielle double ( le meilleur et plus simple avec R )

# led

led_best= HoltWinters(exo2$y, gamma = F)
led_best

# alpha: 0.591591
# beta : 0.3262659
# gamma: FALSE

#Coefficients:       
# a 123.531722
# b   6.004557

# eqm 

prediction=led_best$fitted[,1]# prediction de tous la serie 

eqmTed = eqm(prediction, exo2$y[3:50])
eqmTed #35.61454

# prediction de 5 prochaine valeurs 

predict(led_best, 5)

# [1,] 129.5363
# [2,] 135.5408
# [3,] 141.5454
# [4,] 147.5500
# [5,] 153.5545

############### 2 eme methode 

# ##################### modele lineaire 

dim(exo2)
source("./fonctions_exam_tp.R")

prediction=sapply(c(2:50), function(x){
	predictionLm(exo2$y,x)
})

eqmpredictionlinneaire = eqm(prediction,exo2$y[2:50])

eqmpredictionlinneaire #225.3607

##################### polynomial

# degre 2

prediction=sapply(c(2:50), function(x){
	prediction_polynomiale(exo2$y,x,2)
})

eqmd2 = eqm(prediction, exo2$y[2:50])
eqmd2 #17.09569

#degre 5

prediction=sapply(c(2:50), function(x){
	prediction_polynomiale(exo2$y,x,5)
})

eqmd5 = eqm(prediction, exo2$y[2:50])
eqmd5 #14.63464

#degre 8

prediction=sapply(c(2:50), function(x){
	prediction_polynomiale(exo2$y,x,8)
})

eqmd8 = eqm(prediction, exo2$y[2:50])
eqmd8 #14.03221


# conclusion : la bonne methode est polynomial de degre 8 qui a le plus petit eqm (14)

# prediction des 5 prochaine valeurs 

prediction=sapply(c(51:55), function(x){
	prediction_polynomiale(exo2$y,x,8)
})

prediction

#124.94239 122.93523 114.84624  98.02853 69.09372 

###############    Exercice 3 :   ##########################
# Chargement des données

exo3 = read.table("./exo3_binome6.txt", header = T) # Remplacer XX par votre numéro de binome

plot(exo3$Naissances, typ = "l",xlab="Semaines",ylab="y")

acf(exo3, lag.max = 100)
###Q1
# analyse de la variance

source("./fonctions_exam_tp.R")

res=analyse_variance(exo3$Naissances,12)
res #84.82341 124.10758

#  V_I / V_R =84.82341

#V_P / V_R =124.10758

# rappel de cour :

# ( vi / vr) doit etre > a fichier p-1;(n-1)(p-1) : saisonalite 

# ( vp / vr) doit etre sup a fichier n-1;(n-1)(p-1) : tendance 

# 
dim(exo3)#108 => p=12 et n=108

# quantile de fichier p-1;(n-1)(p-1) degree

qf(0.95, 11,107*11)#1.796771

# 84.82341 >  1.796771 donc presence de saisonalite 

###Q2
La serie pr�sentant une saisonnalit� et une tendance nous pouvons appliquer deux m�thodes : la decomposition et le
# lissage exponentiel triple (LET)

############################ Lissage exponentiel Triple
n_ts = ts(exo3$Naissances, frequency = 12) # time series

let_best= HoltWinters(n_ts)
let_best$coefficients
let_best

# alpha: 0.8774038
# beta : 0
# gamma: 1

# denriere a et b calucler 

#a   351.1483234
#b     0.2916667

#Estimation de la performance du let
source("./fonctions_exam_tp.R")


# calcule  eqm

prediction=let_best$fitted[,1]
prediction

length(prediction)#96

# eqm du 2 eme saison
 
eqm_let=eqm(prediction[1:12],exo3$Naissances[12:24])
eqm_let# 257.3185


#eqm_let(exo3,12,12)

############# prediction des 12 prochaine valeurs 

predictions = predict(let_best, n.ahead = 12) # pour avoir les 12 prochaines

predictions 
#  Jan      Feb      Mar      Apr      May
# 10 349.7782 329.0602 352.8180 327.1219 339.8433
#       Jun      Jul      Aug      Sep      Oct
# 10 346.2686 371.7273 378.1206 371.6716 366.6868
#        Nov      Dec
# 10 349.1313 358.5000
#

plot(exo3$Naissances,type="l")

lines(c(109:120),predictions ,col="blue")



########################################################### La decomposition

#Filtrage par moyenne mobile

naiss_mm = ma(exo3$Naissances,12, centre = T)# estimation de la tendance ( on supprimer la saisonalite)

naiss_mm
plot(exo3$Naissances, typ = "l", xlim = c(1,108))

lines(naiss_mm, col = "red")

##### Estimation des coefficients saisonniers sur la série sans tendance.

naiss_st = exo3$Naissances - naiss_mm # serie sans tendance
naiss_st

plot(naiss_st, typ = "l")

acf(naiss_st,lag.max =40,na.action = na.pass )

# On calcule d'abord le nombre de NA qu'il faut ajouter:

n_na = ceiling(length(naiss_st)/12)*12- length(naiss_st)# calcul de nombre de Na 
n_na

 #Puis on ajoute autant de NA qu'il faut à la fin de la série:
naiss_st = c(naiss_st, rep(NA, n_na))

# On peut maintenant ranger la série dans une matrice à 12 colonnes
# La colonne 1 représente le mois de Janvier, etc... 

naiss_st_mat = matrix(naiss_st, ncol = 12, byrow = T)# ranger dans une matric

naiss_st_mat

# On calcule la moyenne de chaque colonne (fonction colMeans): 

coeff = colMeans(naiss_st_mat, na.rm = T) # en enlevant les NA
coeff 
mean(coeff )

# Pour les rendre de moyenne nulle, il faut centrer les coefficients 

coeff = coeff - mean(coeff) # maintenant les coeffs sont de moyenne nulle
coeff

mean(coeff) #2.952102e-16


# Le vecteur coeff représente les 12 coefficients saisonniers estimés.
# La série saisonnière est simplement la répétition de ces 12 coefficients pendant 25 ans (durée de la série CO2)

naiss_saison = rep(coeff, 9)## Pour les rendre de moyenne nulle, il faut centrer les coefficients 

coeff = coeff - mean(coeff) # maintenant les coeffs sont de moyenne nulle
coeff

mean(coeff)

# Le vecteur coeff représente les 12 coefficients saisonniers estimés.
# La série saisonnière est simplement la répétition de ces 12 coefficients pendant 9 ans (durée de la série CO2)

naiss_saison = rep(coeff,9)## Pour les rendre de moyenne nulle, il faut centrer les coefficients 

coeff = coeff - mean(coeff) # maintenant les coeffs sont de moyenne nulle
coeff

mean(coeff) 
naiss_saison

########## Estimation paramétrique de la tendance

# Maintenant, on va pouvoir estimer la tendance de la série exo3 désaisonnalisée, c'est à dire : 

naiss_des = exo3$Naissances - naiss_saison# serie avec tendance seulement 

# prediction par modele lineaire

t = c(1:length(naiss_des))# vecteur de temps

modele1= lm(naiss_des~t)# droite

modele1$coefficients

#t=286.3448750  + 0.5852452t

######## Prédiction des prochaines valeurs:
# T(h) + coeff[1+ (h-1) modulo 12].

predi109=286.3448750 + (0.5852452*109) + coeff[1+ (108 %% 12 )] 
predi109

source("./fonctions_exam_tp.R")

#Prediction des 5 prochaines valeurs
predict=prediction_decomposition(naiss_des,109,113,12)
predict

#348.7121 327.1515 349.9660 323.4054 337.0166

#Prediction polynomiale
source("./fonctions_exam_tp.R")

predict2=prediction_decomposition_poly(naiss_des,109,113,2,12)
predict5=prediction_decomposition_poly(naiss_des,109,113,5,12)
predict9=prediction_decomposition_poly(naiss_des,109,113,9,12)

### Estimation des performances de cette méthode

#calcul des eqm

source("./fonctions_exam_tp.R")

#pour historique k=12=p

eqm_ploy(naiss_des,12,2,12)#330.0874
eqm_ploy(naiss_des,12,5,12)#315.2632
eqm_ploy(naiss_des,12,9,12)#285.6503

#pour historique k=24=2*p

eqm_ploy(naiss_des,24,2,12)#319.1876
eqm_ploy(naiss_des,24,5,12)#286.9324
eqm_ploy(naiss_des,24,9,12)#276.4384

#pour historique k=36 =p*3

eqm_ploy(naiss_des,36,2,12)#468.9915
eqm_ploy(naiss_des,36,5,12)#489.3385
eqm_ploy(naiss_des,36,9,12)#506.5723

# conclusion : c' est le led qui a l'eqm le plus petit 

# voici les preditction des 12 prochaines

#  Jan      Feb      Mar      Apr      May
# 10 349.7782 329.0602 352.8180 327.1219 339.8433
#       Jun      Jul      Aug      Sep      Oct
# 10 346.2686 371.7273 378.1206 371.6716 366.6868
#        Nov      Dec
# 10 349.1313 358.5000