# Function for fitting Pawar 2018 model using parallel processing
fit_curves_parallel = function(dat) {
  
  # Fit curve with NLS multstart
  fit <- nls_multstart(Photo ~ pawar_2018(temp = Tleaf, r_tref, e, eh, topt, tref=10),
                       data = dat,
                       iter = 1000,
                       start_lower = c(r_tref = 0, e = 0, eh = 0.2, topt = 0),
                       start_upper = c(r_tref = 20, e = 2, eh = 5, topt = 50),
                       supp_errors = 'Y',
                       na.action = na.omit,
                       lower = c(r_tref = -10, e = 0, eh = 0, topt = 0))
  
  # If the fit was successful, extract parameters estimates and SE
  if(typeof(fit) != "NULL") { 
    r_tref <- coef(fit)["r_tref"]
    r_tref_SE <- summary(fit)$coefficients[,'Std. Error']['r_tref']
    e <- coef(fit)["e"]
    e_SE <- summary(fit)$coefficients[,'Std. Error']['e']
    eh <- coef(fit)["eh"]
    eh_SE <- summary(fit)$coefficients[,'Std. Error']['eh']
    topt <- coef(fit)["topt"]
    topt_SE <- summary(fit)$coefficients[,'Std. Error']['topt']
    AIC <- AIC(fit)
    r_sq = 1-sum(resid(fit)^2)/sum((dat$Photo-mean(dat$Photo))^2)
    
  # Otherwise, likely a convergence failure. All parameters set to NA
  } else { 
    r_tref = r_tref_SE = e = e_SE = eh = eh_SE = topt = topt_SE = AIC = r_sq = NA
    dat$failure_status = "Convergence failure"
  }
  
  # Make data frame with results and return
  results = data.frame(select(dat[1,],-Tleaf, -Photo), r_tref, r_tref_SE, e, e_SE, 
                       eh, eh_SE, topt, topt_SE, AIC, r_sq)
  return(results)
  
}
