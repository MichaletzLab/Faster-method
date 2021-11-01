quadratic.model = function(temp, a, b, c) {
  
  return(a*temp*temp + b*temp + c)
  
}

fit.quadratic = function(data.AT) {
  
  
  output.params = c()
  
  for (i in unique(data.AT$curveID)) {
    print(i)
    d_1 = subset(data.AT, curveID == i)
    
    fit = nls_multstart(A ~ quadratic.model(temp = Tleaf, a1,b1,c1),
                        data = d_1,
                        iter = 1000,
                        start_lower =  c(a1 = -5, b1 = 0, c1 = -10),
                        start_upper = c(a1 = 0, b1 = 10, c1 = 10),
                        supp_errors = 'Y',
                        na.action = na.omit)
    
    
    #fit = lm(A ~ poly(Tleaf, degree = 2, raw = T), data = d_1)
    
    # If the fit failed, set failure status and all params to NA
    if(typeof(fit) == "NULL" || coef(fit)["a1"] >= 0) {
      Topt = Amax = Tbreadth = CTmin = CTmax = a = b = c = r_sq = NA
      print(coef(fit)["a1"])

    } else {
      
      a <- coef(fit)["a1"]
      b <- coef(fit)["b1"]
      c <- coef(fit)["c1"]
    
      Topt = -b/(2*a)
      Amax = quadratic.model(Topt, a,b,c)
      d = c-0.5*Amax
      Tlow = (-b - sqrt(b*b-4*a*d))/(2*a)
      Thigh = (-b + sqrt(b*b-4*a*d))/(2*a)
      Tbreadth = abs(Thigh-Tlow)
      CTmin = (-b + sqrt(b*b-4*a*c))/(2*a)
      CTmax = (-b - sqrt(b*b-4*a*c))/(2*a)
      
      # Compute pseudo-r2
      r_sq = 1-sum(resid(fit)^2)/sum((d_1$A-mean(d_1$A))^2)
    }
    
    
    # Bind dataframe
    output.params = bind_rows(output.params, 
                              data.frame(curveID = i, method = d_1$method[1], species = d_1$species[1], 
                                         rep = d_1$rep[1], Topt, Amax, Tbreadth, 
                                         CTmin, CTmax, a, b, c, r_sq))
    
  }
  
  return(output.params)
}
