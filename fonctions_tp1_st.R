analyse_variance = function(s,p){
  
  m = matrix(s, ncol = p, byrow = TRUE)
  s_p = p*sum((apply(m, 1,mean) - mean(m))^2)
  v_p = s_p/(nrow(m)-1)
  s_i= nrow(m)*sum((apply(m,2,mean)-mean(m))^2)
  v_i = s_i/(p-1)
  s_t = sum((m-mean(m))^2)
  s_r = s_t - s_p - s_i
  v_r = s_r / ((nrow(m)-1)*(p-1))
  return(c(v_i/v_r, v_p/v_r))
  
  
}

 prediction_moyenne=function(s, i){
 j=i-1
 m= mean(s[0 : j])
 return (c(m))

 }

 erreur =function(prediction, verite){
	res=(1:length(prediction))
	
	for( i in 1:length(prediction))
      {
	 res[i] =(verite[i] - prediction[i])^2;
	}
	
	
	 return (res)
 
 }

eqm =function(prediction, verite){
	res=(1:length(prediction))
	
	for( i in 1:length(prediction))
      {
	 res[i] =(verite[i] - prediction[i])^2;
	}
	
	 m=mean(res)
	 return (m)
 
 }


 prediction_k_valeur=function(s,i,k)
{
	i
	debut=(i - k) 
	fin = (i-1)
	valeurs=s[debut :fin]
	return (mean(valeurs))
}

eval_k_valeur = function(s,k){

debut=k+1
prediction=sapply(c(debut:length(s)), function(x){
	 prediction_k_valeur(s,x,k)})

valEqm= eqm (prediction,s[debut:length(s)])

return (valEqm)

}

# tp2

 predictionLm=function(s,x)
{
	t = c(1:length(s))
	modele1= lm(s~t)
	newdata = data.frame("t"=x) # l'instant pour lequel on veut prÃ©dire 
	prediction = predict(modele1, newdata) # on fait la prÃ©diction
	return (prediction)
}

prediction_polynomiale= function(s,i,d){
	t = c(1:length(s))
	modele = lm(s~poly(t,d, raw=T))
	newdata = data.frame("t"=i) 
	prediction = predict(modele, newdata) # on fait la prÃ©diction
	return (prediction)

}


prediction_tendance_k_derniere=function(s, k, i, d)
{
	debut=(i - k) 
	fin = (i-1)
	t = c(debut:fin)

	#modele = lm(s[debut:fin]~ poly(t,d, raw=T))
	modele = lm(s ~ poly(t,d, raw=T))

	newdata = data.frame("t"=i) 
	prediction = predict(modele, newdata) # on fait la prÃ©diction
	return (prediction)

}