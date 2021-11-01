fit.pawar = function(data.AT) {
  
  
  output.params = c()
  
  for (i in unique(data.AT$curveID)) {
    print(i)
    d_1 = subset(data.AT, curveID == i)
    
    fit = nls_multstart(A ~ pawar_2018(temp = Tleaf, r_tref, e1, eh, topt, tref = 10),
                        data = d_1,
                        iter = 1000,
                        start_lower =  c(r_tref = 0.1, e1 = 0.01, eh = 0.01, topt = 5),
                        start_upper = c(r_tref = 50, e1 = 2, eh = 5, topt = 50),
                        supp_errors = 'Y',
                        lower = c(r_tref = 0, e1 = 0, eh = 0, topt = 0),
                        na.action = na.omit)
    
    
    #fit = lm(A ~ poly(Tleaf, degree = 2, raw = T), data = d_1)
    
    # If the fit failed, set failure status and all params to NA
    if(typeof(fit) == "NULL") {
      print("no")
      r_tref = e = eh = topt = tref = r_sq = NA
      
    } else {
      
      r_tref <- coef(fit)["r_tref"]
      e <- coef(fit)["e1"]
      eh <- coef(fit)["eh"]
      topt <- coef(fit)["topt"]
      tref = 10
      
      # Compute pseudo-r2
      r_sq = 1-sum(resid(fit)^2)/sum((d_1$A-mean(d_1$A))^2)
    }
    
    
    # Bind dataframe
    output.params = bind_rows(output.params, 
                              data.frame(curveID = i, method = d_1$method[1], species = d_1$species[1], 
                                         rep = d_1$rep[1], r_tref, e, eh, topt, tref, r_sq))
    
  }
  
  return(output.params)
  
  
}
