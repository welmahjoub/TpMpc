# el mahjoub abdrahmane et yaya simpara : binomes 6 


prediction_moyenne=function(s, i){
   
   fin=i-1
   m= mean(s[1 : fin])
   return (c(m))

 }

eqm =function(prediction, verite){

	 d=prediction-verite

	 d=d^2	

	 m=mean(d)

	 return (m)
 
 }

 prediction_k_valeur_passe=function(s,i,k)
{
	
	debut=(i - k) 
	fin  = (i - 1)
	valeurs=s[debut :fin]
	return (mean(valeurs))
}

prediction_mediane=function(s, i){
   
   fin=i-1
   m= median(s[1 : fin])
   return (c(m))

 }


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

predictionLm=function(s,x)
{
  t = c(1:length(s))
  modele1= lm(s~t)
  newdata = data.frame("t"=x) # l'instant pour lequel on veut prÃ©dire 
  prediction = predict(modele1, newdata) # on fait la prÃ©diction
  return (prediction)
}

prediction_decomposition=function (s, i, h,p){

  prediction=sapply(c(i:h), function(x){

  predictionLm(s,x)+coeff[1+(x-1) %% p]
})
 return (prediction)
}

prediction_polynomiale= function(s,i,d){
  t = c(1:length(s))
  modele = lm(s~poly(t,d, raw=T))
  newdata = data.frame("t"=i) 
  prediction = predict(modele, newdata) # on fait la prÃ©diction
  return (prediction)

}

prediction_decomposition_poly=function (s, i, h,d,p){

  prediction=sapply(c(i:h), function(x){

  prediction_polynomiale(s,x,d)+coeff[1+(x-1) %% p]
})
 return (prediction)
}

eqm_ploy=function(s,k,d,p)
{
  predict=prediction_decomposition_poly(s,k,k+p,d,p)#  preduire les valeurs de k jusqu k+p

  eqm=eqm(predict,s[k:(k+p)])

  return (eqm)  
}

eqm_let=function(s,k,p)
{
  N_ts = ts(s$Naissances, frequency = p)

  let_best= HoltWinters(N_ts)

  predictions = predict(let_best, n.ahead = p)

  eqm=eqm(predictions ,s$Naissances[k:(k+p)])

  return (eqm)  

}
