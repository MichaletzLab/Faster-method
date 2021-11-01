medlyn_2002 = function(temp, r_tref, e, eh, ds, tref = 25) {
  
  Tk = temp + 273
  Trefk = tref + 273
  R = 8.314 # J/molK
  
  term.a = r_tref*exp(e*(Tk-Trefk)/(Trefk*R*Tk))
  term.b = 1+exp((Trefk*ds-eh) / (Trefk*R))
  term.c = 1+exp((Tk*ds-eh) / (Tk*R))
  
  return(term.a*term.b/term.c)                    
  
}



fit.medlyn = function(data.AT) {
  
  
  output.params = c()
  
  for (i in unique(data.AT$curveID)) {
    print(i)
    d_1 = subset(data.AT, curveID == i)
    
    fit = nls_multstart(A ~ medlyn_2002(temp = Tleaf, r_tref, e1, eh, ds),
                        data = d_1,
                        iter = 1000,
                        start_lower =  c(r_tref = 0.1, e1 = 0.01, eh = 0.01, ds = 0),
                        start_upper = c(r_tref = 300, e1 = 1e5, eh = 1e6, ds = 3000),
                        supp_errors = 'Y',
                        lower = c(r_tref = 0, e1 = 0, eh = 0, ds = 0),
                        na.action = na.omit)
    
    
    #fit = lm(A ~ poly(Tleaf, degree = 2, raw = T), data = d_1)
    
    # If the fit failed, set failure status and all params to NA
    if(typeof(fit) == "NULL") {
      print("no")
      r_tref = e = eh = ds = tref = r_sq = NA
      
    } else {
      
      r_tref <- coef(fit)["r_tref"]
      e <- coef(fit)["e1"]
      eh <- coef(fit)["eh"]
      ds <- coef(fit)["ds"]
      tref = 25
      
      # Compute pseudo-r2
      r_sq = 1-sum(resid(fit)^2)/sum((d_1$A-mean(d_1$A))^2)
    }
    
    
    # Bind dataframe
    output.params = bind_rows(output.params, 
                              data.frame(curveID = i, species = d_1$species[1], 
                                         rep = d_1$rep[1], r_tref, e, eh, ds, tref, r_sq))
    
  }
  
  return(output.params)
  
  
}
