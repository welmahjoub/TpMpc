# La fonction myreg ci-dessous permet une manipulation plus simple de la commande
# lm pour faire de la régression. Elle prend en paramètre trois choses : 
# - data : un tableau de données avec individus en lignes, variables en colonnes
# - idxp : un vecteur contenant les indices des colonnes de la (ou des) variable(s)
# prédictrices dans data
# - idxc : l'indice de la colonne de la variable à prédire dans data
# Le résultat de cette fonction est un modèle de régression (comme ceux renvoyés par 
# la fonction lm).
# Exemple : supposons que nous ayons un tableau tab de données avec 3 colonnes : 
# - les 2 premières colonnes x1 et x2 sont des variables prédictrices
# - la 3 ème colonne est la variable y à prédire
# Pour créer un modèle de régression expliquant y à partir de x2 seulement : 
# mod = myreg(tab, 2, 3)
# Pour créer un modèle de régression expliquant y à partir de x1 et x2:
# mod = myreg(tab, c(1,2), 3)


myreg = function(data, idxp, idxc){
  fff = as.formula(paste(paste(colnames(data)[idxc],"~"),paste(colnames(data)[idxp],
                                                               collapse = "+")))
  return(lm(fff, data = data))
}





# fonction puissance_v2
# Cette fonction crée un nouveau tableau de donneée à 3 colonnes : 
# - la colonne correspondant à la variable cible d'indice idx_c
# - la colonne correspondant à la variable prédictrice d'indice idx_p
# - la variable d'indice idx_p mise à la puissance p
puissance_v2 = function(data, idx_p, idx_c, p){
  
  out = data[,c(idx_c, idx_p)]
  out = cbind(out, data[,idx_p]^p)
  if(p>0){
    fff = paste(colnames(data)[idx_p],"_P",p, sep = "")
  }
  else{fff = paste(colnames(data)[idx_p],"_Pm",-p, sep = "")}
  colnames(out)[3] = fff
  
  return(out)
  
}

# fonction puissance
puissance = function(data, idx_col, p){
  
  out = data
  
  for(j in 1:length(idx_col)){
    
    
    for(i in 1:length(p)){
      
      out = cbind(out,data[,idx_col[j]]^p[i])
      if(p[i]>0){
        fff = paste(colnames(out)[idx_col[j]],"_P",p[i], sep = "")
      }
      else{fff = paste(colnames(out)[idx_col[j]],"_Pm",-p[i], sep = "")}
      colnames(out)[ncol(data)+i+(j-1)*length(p)] = fff
    }
    
  }
  
  return(out)
}




eqm = function( apprenti,test,idxp, idxc){
  
  
  mod= myreg(apprenti, idxp,idxc);
  
  prediction=predict( mod,test);
  
  eq=prediction-test[,idxc]
  
  eqm=mean(eq^2);
  
  return (eqm)
  
}


# renvoie indice du meilleur model qui as le plus petit eqm
meilleurmodele=function( apprenti,test,idxp, idxc){

  
  eqm=sapply(idxp,function(x){

	    eqm( apprenti,test,x,idxc);
	  
	})

	indice_min=which.min(eqm)
	min=min(eqm)

	return (c(indice_min,min))

}
 
# faire un preduction et les stocker dans un fichier 
generateFilePrediction=function(apprenti,test,idx_p, idx_c){
  

  mod = myreg(apprenti, idx_p , idx_c )
  
  p = predict(mod, test)
  
  write.csv(data.frame("Id" = c(1:654), "F30" = p), file ="./prediction.csv", row.names = F)
  
}

# fonction selec_asc 
selec_asc = function(apprenti,test, idx_p, idx_c){
  
  Vs = c()
  
  Vr = idx_p
  
  Cb = 10^32
 
  out = matrix(0, nrow = 2)
  
  bool = TRUE
  
   while ( bool & length(Vr) > 0){
     
		eqm=sapply(Vr,function(x){

		   eqm(apprenti,test, c(Vs, x),idx_c )
		})
		
		
		C_X_best = min(eqm)# le performance du cb 
	
		indice_min_eqm = which.min(eqm)# xb
		
		X_best = Vr[indice_min_eqm]
		
		if (C_X_best < Cb)
		{
			Vs = c(Vs,X_best)
			
			Vr = Vr[-indice_min_eqm]
			
			Cb = C_X_best
			
			out = cbind(out, c(X_best, C_X_best))
			
		}else
		{
		  bool=FALSE
		}
		
	}
	
	return (out[,-1])
}
