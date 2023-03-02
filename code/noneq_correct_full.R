# Function for computing dynamic assimilation using the full dynamic equation
# explained in the Saathof and Welles paper, and in the new LI-6800 manual

# Currently implemented only to compute Adyn; Edyn not included

# Arguments:
# Data is a dataframe with LI-6800 data
# dt1 is the calibration "tuning" constant from licor for REF
# dt2 is the calibration "tuning" constant from licor for SAMPLE
# aV is the effective volume tuning constant
noneq_correct_full = function(data, dt1, dt2, aV) {
  
  # 
  return_data = c()
  
  # Iterate over curves in dataset
  for (i in unique(data$curveID)) {
    
    # Grab first curve
    non.eq.AT = subset(data, curveID == i)
    
    len = dim(non.eq.AT)[1]
    
    co = non.eq.AT$CO2_s/1e6 #[2:n] #umol/mol
    ce = non.eq.AT$CO2_r/1e6 #[2:n] #umol/mol
    
    wo = non.eq.AT$H2O_s/1e3 #[2:n] #mmol/mol
    we = non.eq.AT$H2O_r/1e3 #[2:n] #mmol/mol
    
    # 1. Get Crd and Csd at each t
    Crd = ce/(1-we)*1e6
    Csd = co/(1-wo)*1e6
    
    # 2. Interpolate Crd at -dt1
      # Find the two points which straddle time-dt1 in time
      # If one of them matches exactly, use that one
      # Otherwise, interpolate between values
    
    time = non.eq.AT$time
    past_time = time-dt1
    
    Crd_dt1 = rep(NA, len)
    
    for (i in 1:len) {
      # check if past time < beginning of time; if so, Crd_dt1 = NA
      if (past_time[i] < time[1]) {
        Crd_dt1[i] = NA
        next
      }
      
      # check if past time matches exactly one time; if so, set Crd_dt1 to that value
      if (sum(past_time[i] == time) == 1) {
        Crd_dt1[i] = Crd[which(past_time[i] == time)]
        next
      }
      
      # otherwise, find the two straddling points in time
      diff = time-past_time[i]
      next_obs = which(min(diff[diff > 0]) == diff)
      last_obs = which(max(diff[diff < 0]) == diff)
      
      time_diff = past_time[i]-time[last_obs]
      slope = (Crd[next_obs]-Crd[last_obs])/(time[next_obs]-time[last_obs])
      Crd_last = Crd[last_obs]
      
      Crd_dt1[i] = Crd_last + slope*time_diff
      
    }
    
    # 3. Compute dCsd/dt at each t
    dCsc_dt = (Csd[2:len]-Csd[1:len-1])/(time[2:len]-time[1:len-1])
    dCsc_dt = c(NA, dCsc_dt)
    
    dCsc_dt_dt2 = rep(NA, len)
    
    # 4. Interpolate dCsd/dt at -dt2
    for (i in 1:len) {
      # check if past time < beginning of time; if so, Crd_dt1 = NA
      if (past_time[i] < time[1]) {
        dCsc_dt_dt2[i] = NA
        next
      }
      
      # check if past time matches exactly one time; if so, set Crd_dt1 to that value
      if (sum(past_time[i] == time) == 1) {
        dCsc_dt_dt2[i] = dCsc_dt[which(past_time[i] == time)]
        next
      }
      
      # otherwise, find the two straddling points in time
      diff = time-past_time[i]
      next_obs = which(min(diff[diff > 0]) == diff)
      last_obs = which(max(diff[diff < 0]) == diff)
      
      time_diff = past_time[i]-time[last_obs]
      slope = (dCsc_dt[next_obs]-dCsc_dt[last_obs])/(time[next_obs]-time[last_obs])
      dCsc_dt_last = dCsc_dt[last_obs]
      
      dCsc_dt_dt2[i] = dCsc_dt_last + slope*time_diff
      
    }
    
    # 5. Finally, compute Adyn
    
    S = non.eq.AT$S #[2:n] # cm^2
    Tair = non.eq.AT$Tair
    Flow = non.eq.AT$Flow
    Pa = non.eq.AT$Pa
    H2OR = non.eq.AT$H2O_r
    
    Adyn = (Crd_dt1 - Csd - Pa*1000/(8.314*(Tair+273.15)) * aV/Flow * dCsc_dt_dt2) * Flow/(100*S) * (1000-H2OR)/1000
    
    non.eq.AT$A = Adyn
    
    return_data = bind_rows(return_data, non.eq.AT)
    
    
  }
  
  # Drop any with A = NA
  return_data = subset(return_data, !is.na(A))
  
  return(return_data)
  
}
