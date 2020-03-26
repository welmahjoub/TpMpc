
source("./fonctions_tp_reg.R")
ozone = read.table("./ozone.txt", header = T)
ozone_n = read.table("./ozone_n.txt", header = T)
ozone = rbind(ozone, ozone_n) # on regroupe les 2 jeux


ap_index=sample(nrow(ozone),floor(nrow(ozone)*15/100))

######## Exercice 4: Ajout de puissances, modèle polynomial
# On va tenter dans cet exercice d'améliorer un modèle en créant des modèles polynomiaux.
# On va commencer par travailler avec une seule variable. L'objectif de cet exercice est de déterminer
# le ou les degrés (d'un polynome) qui semble(nt) adaptés à cette variable.
# On va travailler avec la meilleure variable (seule) que vous avez trouvée : x2

# Question : Recalculer l'erreur de generalisation du modèle y en fonction de x2. Cette valeur servira de base


ozone_app=ozone[ap_index,]
ozone_test=ozone[-ap_index,] 

mod_best = myreg( ozone_app, 3,1)
prediction=predict(mod_best,ozone_test)
eq=prediction-ozone_test$y
eqm=mean(eq^2)
eqm# 343.8417



# On va maintenant mettre un modèle polynomial de degré 2 avec x2. Pour cela, il faut créer une nouvelle
# colonne dans notre tableau de données. Cette colonne doit comporter les valeurs de x2 à la puissance 2.
# La fonction puissance du fichier fonctions_tp_reg.R vous permet de faire ça rapidement.
# Allez voir comment elle fonctionne. 

new_data = puissance(ozone, 3, 2)


# Quel est l'indice de la nouvelle colonne ?  Que représente la nouvelle colonne ? 
12 et les valeurs de x2^2
# Pour créer un modèle de regression polynomial de degré 2, il suffit maintenant d'appeler myreg avec les bons indices.

# x2^2
new_data = puissance(ozone, 3, 2)
ozone_app=new_data[ap_index,]
ozone_test=new_data[-ap_index,] 

mod_best = myreg( ozone_app, c(3,12),1)
prediction=predict(mod_best,ozone_test)
eq=prediction-ozone_test$y
eqm=mean(eq^2)
eqm#323.59




# J'utilise les colonnes 2 et 12 (donc x1 et x1^2) pour prédire y. On a donc bien un modèle polynomial de degré 2
# Estimer l'erreur de généralisation de ce modèle. Est - il meilleur que si l'on utilisait x2 seule ?



# Si oui, on décide que la puissance 2 est adaptée à x2 (on l'utilisera plus tard), et si non on l'oublie!
# Ensuite, on va essayer d'augmenter la puissance à 3. Pour cela, on crée : 
new_data = puissance(ozone, 3, c(3)) # Je demande de mettre la colonne 3 (x2) à
# la puissance 3

# x2^3
new_data = puissance(ozone, 3, c(3))
ozone_app=new_data[ap_index,]
ozone_test=new_data[-ap_index,] 

mod_best = myreg( ozone_app, c(3,12),1)
prediction=predict(mod_best,ozone_test)
eq=prediction-ozone_test$y
eqm=mean(eq^2)
eqm#323.8809

# puissance positive 
sapply(c(2:9),function(x){

new_data = puissance(ozone, 3, c(x))
ozone_app=new_data[ap_index,]
ozone_test=new_data[-ap_index,] 

mod_best = myreg( ozone_app, c(3,12),1)
prediction=predict(mod_best,ozone_test)
eq=prediction-ozone_test$y
mean(eq^2)


})

[1] 323.5900 323.8809 325.8597 328.8391
[5] 332.2564 335.6788 338.7972 341.4136

# Question : Estimer l'erreur de généralisation du modèle qui utilise x2, et x2^3
# Vaut-il mieux prendre la puissance 3 ou 2 pour x2 ? x2^2


# Essayer d'autres puissances et regarder la performance (erreur de généralisation)
# Vous pouvez choisir pour x2 une ou deux puissances (celles qui mènent à la meilleure 
# erreur de généralisation)

# Vous pouvez ensuite essayer des puissances négatives (-1, -2, etc)
# Attention, cela n'est uniquement possible s'il n y aucune valeur nulle pour x2

# puissance negative
sapply(c(-9:-1),function(x){

new_data = puissance(ozone, 3, c(x))
ozone_app=new_data[ap_index,]
ozone_test=new_data[-ap_index,] 

mod_best = myreg( ozone_app, c(3,12),1)
prediction=predict(mod_best,ozone_test)
eq=prediction-ozone_test$y
mean(eq^2)


})
[1] 640.6763 577.1562 522.4943 475.5800
[5] 435.7750 402.7239 376.1630 355.7620
[9] 341.0310


# Idem, vous pouvez essayer des racines (puissance 0.5, 1.5, etc)
# Attention, possible uniquement si aucune valeur négative dans x2


# puissance decimale
sapply(c(0.5,1.5,2.5),function(x){

new_data = puissance(ozone, 3, c(x))
ozone_app=new_data[ap_index,]
ozone_test=new_data[-ap_index,] 

mod_best = myreg( ozone_app, c(3,12),1)
prediction=predict(mod_best,ozone_test)
eq=prediction-ozone_test$y
mean(eq^2)

})

# 328.0692 324.3217 323.4771

# Créer un vecteur v qui contient toutes les puissances (3 ou 4 max) que vous avez gardées


# Vous pouvez maintenant ajouter les colonnes adaptées dans le tableau ozone : 
v=c(2.5,2,3)
ozone_v2 = puissance(ozone, 3, v)
# Vous pouvez vérifier que la colonne 3 de ozone a été mise à toutes les puissances qui
# sont dans v.

dim(ozone_v2)#14

# Estimer l'erreur de généralisation du modèle qui utilise toutes les variables (dont celles 
# que l'on vient de créer). Est ce mieux qu'avant ?


ozone_app=ozone_v2[ap_index,]
ozone_test=ozone_v2[-ap_index,] 

mod_best = myreg( ozone_app, c(3,12,13,14),1)
prediction=predict(mod_best,ozone_test)
eq=prediction-ozone_test$y
mean(eq^2)#895.2304





##### Exercice 5 : ajout de puissance et sélection de variables en pratique

# Comme expliqué en cours, lorsqu'on utilise un algo de sélection de variables, il faut
# mettre de coté un ensemble de test qui permettre d'évaluer les différents modèles sélectionnés.

# Question : Si ce n'est déjà fait, séparer le jeu de données ozone en un ensemble
# d'apprentissage (environ 70%) et un ensemble de test.

ap_index=sample(nrow(ozone),floor(nrow(ozone)*70/100))
ozone_app=ozone[ap_index,]
ozone_test=ozone[-ap_index,] 

# Question : Calculer l'EQM du modèle complet (10 variables) sur le jeu de test.

mod_complet= myreg(ozone_app, c(2:11),1);

prediction=predict(mod_complet,ozone_test)
eq=prediction-ozone_test$y
mean(eq^2)# 439.0908

# Cette performance nous servira de base (on va essayer d'améliorer cette perf en mettant des 
# puissances et en faisant de la sélection de variables)

#Question : Appliquer la sélection ascendante puis calculer l'EQM du modèle sélectionné 
# sur le jeu de test.

#selec_asc = function(data, idx_p, idx_c, K)

selec_asc (ozone, c(2:11), 1, 100)# 3.0000  11.0000   5.0000  10.0000

mod_asd= myreg(ozone_app, c(3,11,5,10),1);

prediction=predict(mod_asd,ozone_test)
eq=prediction-ozone_test$y
mean(eq^2)#  291.9948

# Ajouter maintenant les puissances que vous avez choisies pour la variable x2

v=c(2.5,2,3)
ozone_v2 = puissance(ozone, 3, v)

ozone_v2

# Question : Estimer la performance (sur jeu de test) du modèle qui contient toutes les variables de 
# départ ainsi que ces nouvelles puissances.

ozone_app=ozone_v2[ap_index,]
ozone_test=ozone_v2[-ap_index,] 

mod_asd_puis= myreg(ozone_app, c(3,11,5,10,12,13,14),1);

prediction=predict(mod_asd_puis,ozone_test)
eq=prediction-ozone_test$y
mean(eq^2)# 4534.648


# Puis appliquer la sélection de variables. Trouvez vous un meilleur modèle ?


# Vous pourrez ensuite ajouter une ou deux puissances (choisies) pour les autres variables 
# et ainsi essayer de trouver un modèle meilleur.


################# Pour aller plus loin : 


# Pour les plus rapides, si vous voulez en faire un peu plus : 

# 1) Implémenter la validation croisée à K plis(cf cours). Cette fonction pourra remplacer la
# fonction séparation que vous avez écrite (validation croisée est plus rapide et 
# plus fiable)

# Aide : ce n'est pas compliqué théoriquement, l'idée de base est similaire à
# la séparation : créer un modèle sur un ensemble puis calculer une erreur sur un autre
# Sauf que là on le fait K fois (on sépare les données en K groupes et chaque groupe est
# utilisé une fois en test, et K-1 fois en apprentissage)
# Si idx_test contient les indices des individus devant être mis en test, 
test = ozone[idx_test,]
app = ozone[-idx_test,]
# vous permet de créer les ensembles app et test (on l'a déjà fait)

# Le point de départ est de séparer les individus en K groupes (environ de même taille)
# Pour cela, vous pouvez utiliser la commande suivante
k = floor(runif(nrow(ozone), 1, K+1))
# Cette commande assigne à chaque individu (ici il y en a 111) un entier entre 1 et K
# au hasard uniformément. 
# Regardez ce que vaut k 
# Tous les individus pour lesquels k vaut 1 seront dans le 1er groupe, etc...
which(k==1) # vous donne ces individus (les indices)
# imaginez que vous voulez mettre les individus du groupe 1 en test et les autres groupes en 
# apprentissage. Il faut faire:
test = ozone[which(k==1), ]
app = ozone[-which(k==1), ]
# avec ça vous pouvez faire le 1 er pli 
# puis faire pour k=2 , etc... (avec une boucle)

# 2) Implémenter la sélection descendante








