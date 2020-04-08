# Chargement des deux jeux de donn?es
# clear console : clr+L

source("./fonctions_elmahjoub_simpara.R")

dataset = read.table("./dataset.txt", header = T)
competition = read.table("./competition.txt", header = T)


####################################################### etape 1:#############################

# vaiables predictrices : f1 a f29 
# variable cible : f30
# liens : ce sont tous des films

#generation des index des apprenti alearatoire et les stocker dans un fichier

#ap_index=sample(nrow(dataset ),floor(nrow(dataset )*70/100))

#write.csv(data.frame("ap_index" = ap_index), file ="./ap_index.csv", row.names = F)

########################## etape 2: choix meilleur model a un seule variable#############################


ap_index=read.table("./ap_index.csv", header = T) # lecture des index desqui ont été générés
ap_index=ap_index$ap_index

apprenti=dataset[ap_index,]
test=dataset[-ap_index,]


meilleurmodele(apprenti,test,c(1:29),30)#9.000000 2.233365

# la variable f9 est la best modele qui a pour eqm= 2.233365

eqm ( apprenti,apprenti ,9, 30) # eqm apprenti 2.052324
  
generateFilePrediction(dataset,competition,9, 30)# prediction des competition

#Verifaction erreur Apprenti

model_f9 = myreg(apprenti,9, 30)

model_f9
erreur_app = mean (model_f9$residuals^2)
erreur_app #☻2.052324

eqm ( apprenti,test ,9, 30) # erreur test 2.233365

#################################### etape 3: utilisation des 29 variables #############################

# eqm du modele qui contient tous les variables

eqm (apprenti , test,c(1:29), 30)#eqm test : 1.680958
  
eqm (apprenti , apprenti ,c(1:29), 30)# eqm apprenti 1.356254

generateFilePrediction(dataset,competition,c(1:29), 30)# prediction des competition

#Verifction erreur Apprenti

model_f129  = myreg(apprenti,c(1:29), 30)

erreur_app_f129 = mean (model_f129$residuals^2)
erreur_app_f129 #☺1.356254

#################################### etape 4: selection ascendante des variables #########################


selec_asc (apprenti,test, c(1:29), 30)

#         [,1]     [,2]  [,3]     [,4]      [,5]      [,6]     [,7]      [,8]      [,9]     [,10]     [,11]
# [1,] 9.000000 21.00000 8.000 6.000000 26.000000 28.000000 2.000000 15.000000 23.000000 25.000000 12.000000
# [2,] 2.233365  1.91509 1.771 1.732233  1.700951  1.688906 1.678227  1.671587  1.658133  1.654291  1.652468
 
#         [,12]     [,13]   [,14]     [,15]
# [1,] 14.000000 27.000000 4.00000 10.000000
# [2,]  1.650554  1.649191 1.64856  1.648442 

variable_select=c(9,21,8,6,26,28,2,15,23,25,12,14,27,4,10)

eqm (apprenti , test,variable_select, 30)#eqm test :1.648442

eqm (apprenti , apprenti ,variable_select, 30)# eqm apprenti 1.41756

generateFilePrediction(dataset,competition,variable_select, 30)# prediction des competition

#Verifiction erreur Apprenti

model_asc  = myreg(apprenti,variable_select, 30)

erreur_app_asc  = mean (model_asc $residuals^2)
erreur_app_asc  #1.41756


#################################### etape 5: ajout des puissance #########################



length(new_data)

# puissance positive du meilleur variable F9

source("./fonctions_elmahjoub_simpara.R")

eqm_pos=sapply(c(2:11),function(x){
  
  new_data = puissance(dataset, 9, x)
  
  apprenti_n=new_data[ap_index,]
  test_n=new_data[-ap_index,] 
  
  eqm ( apprenti_n,test_n,c(9,31), 30)
    
})

eqm_pos

#2.283298 2.269821 2.259690 2.252032 2.246186 2.241687 2.238201 2.235485

which.min(eqm_pos)#puissance 9 
min(eqm_pos)#2.235485

####################################  puissance negative #################################



source("./fonctions_elmahjoub_simpara.R")

eqm_neg=sapply(c(-9:-1),function(x){
  
  new_data = puissance(dataset, 9, x)
  
  apprenti_n=new_data[ap_index,]
  test_n=new_data[-ap_index,] 
  
  eqm(apprenti_n,test_n,c(9,31), 30)
  
})

eqm_neg

#[1] 2.312004 2.312178 2.312522 2.313199 2.314513 2.316970 2.321152 2.326526 2.328171

which.min(eqm_neg) #puissance -9
min(eqm_neg) #2.312004



 
####################################  puissance decimal #################################


source("./fonctions_elmahjoub_simpara.R")

eqm_decimal=sapply(c(0.5,1.5,2.5,3.5,4.5),function(x){
  
  new_data = puissance(dataset, 9, x)
  
  apprenti_n=new_data[ap_index,]
  test_n=new_data[-ap_index,] 
  
  eqm(apprenti_n,test_n,c(9,31), 30)
  
})

eqm_decimal

#[1] 2.309908 2.291502 2.276086 2.264395 2.255598

which.min(eqm_decimal) #puissance 4.5
min(eqm_decimal) #2.255598

#################################### choix des meilleurs puissance #################################

meilleur_puiss=c(9,8,7)

dataset_puissance = puissance(dataset, 9, meilleur_puiss)


apprenti_puiss=dataset_puissance[ap_index,]

test_puiss=dataset_puissance[-ap_index,] 

eqm(apprenti_puiss,test_puiss,c(9,31,32,33), 30)#2.31731 : cette valeur n'est meilleur que les autres


#################################### choix des meilleurs puissance Bis #################################

# eqm des puissances 9 10 11 :2.235485 2.233357 2.231681

meilleur_puiss_Bis=c(9,10,11)

dataset_puissance_Bis = puissance(dataset, 9, meilleur_puiss_Bis)


apprenti_puiss_Bis=dataset_puissance_Bis[ap_index,]

test_puiss_Bis=dataset_puissance_Bis[-ap_index,] 

eqm(apprenti_puiss_Bis,test_puiss_Bis,c(9,31,32,33), 30)#2.316822 : cette valeur n'est meilleur que les autres non pus


################################################## Ascendante et puissance F9^9 ##################

meilleur_puiss=c(9)

dataset_puissance = puissance(dataset, 9, meilleur_puiss)
competition_puissance = puissance(competition,9,meilleur_puiss)


apprenti_n=dataset_puissance[ap_index,]
test_n=dataset_puissance[-ap_index,] 

variable_select2=c(variable_select,31)# les ancien varaiabe selectionne avec colonne 9^9
variable_select2

#variable_select_puisance=selec_asc (apprenti_n,test_n, c(1:29,31), 30)[1,]

eqm (apprenti_n , test_n,variable_select2, 30)#eqm test : 1.645868

eqm (apprenti_n , apprenti_n ,variable_select2, 30)# eqm apprenti 1.417498


generateFilePrediction(dataset_puissance,competition_puissance,variable_select2, 30)# prediction des competition


################################################## Ascendante et puissance F9^15 ##################

meilleur_puiss=c(15)

dataset_puissance = puissance(dataset, 9, meilleur_puiss)
competition_puissance = puissance(competition,9,meilleur_puiss)

apprenti_n=dataset_puissance[ap_index,]
test_n=dataset_puissance[-ap_index,] 

variable_select2=c(variable_select,31)# les ancien varaiabe selectionne avec colonne 9^15

eqm (apprenti_n , test_n,variable_select2, 30)#eqm test : 1.644674

eqm (apprenti_n , apprenti_n ,variable_select2, 30)# eqm apprenti 1.417361

generateFilePrediction(dataset_puissance,competition_puissance,variable_select2, 30)# prediction des competition
