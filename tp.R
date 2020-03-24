library(forecast)
library(stats)
library(expsmooth)

###############    Exercice 1 :   ##########################
# Chargement des donnÃ©es
ventes = read.table("exo1_binome6.txt", header = T) # Remplacer XX par votre numÃ©ro de binome


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

# Chargement des donnÃ©es

exo2 = read.table("./exo2_binome6.txt", header = T) # Remplacer XX par votre numÃ©ro de binome


# VOS REPONSES ICI
# Q1 : 

plot(exo2$y, typ = "l",xlab="Semaines",ylab="y")

acf(exo2 , lag.max = 20)

dim(exo2)#50

# d'apres  le graphique et le corelogramme la serie presente une tendance car on constate une évolution 
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



###############    Exercice 3 :   ##########################
# Chargement des donnÃ©es

exo3 = read.table("./exo3_binome6.txt", header = T) # Remplacer XX par votre numÃ©ro de binome

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
La serie présentant une saisonnalité et une tendance nous pouvons appliquer deux méthodes : la decomposition et le
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

