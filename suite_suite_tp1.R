
source("./fonctions_tp_reg.R")
ozone = read.table("./ozone.txt", header = T)
ozone_n = read.table("./ozone_n.txt", header = T)
ozone = rbind(ozone, ozone_n) # on regroupe les 2 jeux


ap_index=sample(nrow(ozone),floor(nrow(ozone)*85/100))

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
eqm#349.4211



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
eqm#331.1599




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
eqm#332.4369

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

 331.1599 332.4369 334.0434 335.8503 337.7428 339.6186 341.3912 342.9950

#plus petit puissance 2 puis 3 puis 4

# Question : Estimer l'erreur de généralisation du modèle qui utilise x2, et x2^3
# Vaut-il mieux prendre la puissance 3 ou 2 pour x2 ? x2^2
 eqmX2 = eqm#349.4211
eqmX2^2 = 331.1599 
eqmX2^3 =332.4369

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

#plus on diminue la puissance plus eqm diminue (-1 est la meilleure)

 344.5238 343.2030 341.5953 339.7147 337.6285 335.4705 333.4363 331.7508 330.6136



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

#plus on diminue la puissance plus eqm diminue (0.5 est la meilleure)

# 330.1662 330.6886 331.7486

# Créer un vecteur v qui contient toutes les puissances (3 ou 4 max) que vous avez gardées


# Vous pouvez maintenant ajouter les colonnes adaptées dans le tableau ozone : 
v=c(0.5,-1,2)
ozone_v2 = puissance(ozone, 3, v)
# Vous pouvez vérifier que la colonne 3 de ozone a été mise à toutes les puissances qui
# sont dans v.

dim(ozone_v2)#14
ozone_v2

# Estimer l'erreur de généralisation du modèle qui utilise toutes les variables (dont celles 
# que l'on vient de créer). Est ce mieux qu'avant ?


ozone_app=ozone_v2[ap_index,]
ozone_test=ozone_v2[-ap_index,] 

mod_best = myreg( ozone_app, c(3,12,13,14),1)
prediction=predict(mod_best,ozone_test)
eq=prediction-ozone_test$y
mean(eq^2)#335.7019

non c'est pas meilleur que les autres





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
mean(eq^2)#352.4608

# Cette performance nous servira de base (on va essayer d'améliorer cette perf en mettant des 
# puissances et en faisant de la sélection de variables)

#Question : Appliquer la sélection ascendante puis calculer l'EQM du modèle sélectionné 
# sur le jeu de test.

#selec_asc = function(data, idx_p, idx_c, K)

selec_asc (ozone, c(2:11), 1, 100)# 3.0000  11.0000   5.0000  9.0000

mod_asd= myreg(ozone_app, c(3,11,5,9),1);

prediction=predict(mod_asd,ozone_test)
eq=prediction-ozone_test$y
mean(eq^2)#  280.5422

# Ajouter maintenant les puissances que vous avez choisies pour la variable x2

v=c(0.5,-1,2)
ozone_v2 = puissance(ozone, 3, v)

ozone_v2

# Question : Estimer la performance (sur jeu de test) du modèle qui contient toutes les variables de 
# départ ainsi que ces nouvelles puissances.

ozone_app=ozone_v2[ap_index,]
ozone_test=ozone_v2[-ap_index,] 

mod_asd_puis= myreg(ozone_app, c(3,11,5,9,12,13,14),1);

prediction=predict(mod_asd_puis,ozone_test)
eq=prediction-ozone_test$y
mean(eq^2)#249.7062


# Puis appliquer la sélection de variables. Trouvez vous un meilleur modèle ?

selec_asc (ozone_v2, c(2:14), 1, 100)

# 14.0000  11.0000   5.0000
# 306.0249 232.7981 194.6794


# 14.000  11.0000   5.0000   8.0000   9.000
# 299.787 239.9129 207.6448 204.5929 199.294

mod_asd_puis= myreg(ozone_app, c(14,11,5),1);

prediction=predict(mod_asd_puis,ozone_test)
eq=prediction-ozone_test$y
mean(eq^2)#277.8333

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

#Voir la fonction

# 2) Implémenter la sélection descendante
		
   		eqm=combn(c(1,2,3,4),3, function(x){

   			eqm_stable(ozone,c( x),1 , 1)
			print(x)
   			
   		})
eqm


selec_desc = function(data, idx_p, idx_c, K){

  Vs = idx_p
  nb=length(Vs)

  Cb=eqm_stable(data,idx_p,idx_c , K)

  test=TRUE
  out = matrix(0, nrow = 2)
  
 
   while (test & nb>0){

   		res=matrix(0, nrow = 2)
   		
   		combn(Vs, nb-1, function(x){

   			eq=eqm_stable(data, x,idx_c , K)
   			res = cbind(res, c(x, eq))
   		})

		C_X_best=min(res[1,])# le performance du cb  : eqm du combinaison la plus performante
		indice_min_eqm=which.min(res[1,])# xb
		
		combin=res[0,indice_min_eqm]# recuper la bonne combinaison
		
		if (C_X_best < Cb)
		{
			Vs=combin
			Cb=C_X_best
			out = cbind(out, c(combin, C_X_best))
			
		}else
		{
			test=FALSE
		}
	}
	
	return (out[,-1])
}

selec_desc(ozone, c(2:11),1, 100)






