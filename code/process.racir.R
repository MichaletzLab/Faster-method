process.racir = function(racir.data) {
  

  # This section automatically corrects the ACi curves
  racir_corr = c()
  for(fn in unique(racir.data$filename)) {
    
    if(str_detect(fn, "blank")) {
      # set this one as the current blank cal data 
      caldat = subset(racir.data, filename == fn)[-1:-40,]
    } else {
      # set this one as the current racir data
      racdat = subset(racir.data, filename == fn)[-1:-40,]
      corrdat = racircal(data = racdat, caldata = caldat)
      
      racir_corr = bind_rows(racir_corr, corrdat)
    }
  }
  # 
  # # Fit aci curves
  # extracted = fitacis(data = racir_corr, 
  #                     group = "curveID",
  #                     id = c("filename", "curveID"),
  #                     varnames = list(ALEAF = "Acor", Tleaf = "Tleaf", Ci = "Cicor", PPFD = "Qin", Rd = "Rd"),
  #                     Tcorrect = F,
  #                     fitmethod = "default")
  
  kinetics_data = c()
  for (i in unique(racir_corr$curveID)) {
    dat_cur = subset(racir_corr, curveID == i)
    kin_cur = try(
      fitaci(data = dat_cur, 
             varnames = list(ALEAF = "Acor", Tleaf = "Tleaf", Ci = "Cicor", PPFD = "Qin", Rd = "Rd"),
             Tcorrect = F,
             fitmethod = "default"))
    
    if(class(kin_cur) == "try-error") {
      kin_cur = fitaci(data = dat_cur, 
             varnames = list(ALEAF = "Acor", Tleaf = "Tleaf", Ci = "Cicor", PPFD = "Qin", Rd = "Rd"),
             Tcorrect = F,
             fitmethod = "bilinear")
    }
    
    vcmax = kin_cur$pars[1]
    jmax = kin_cur$pars[2]
    rd = kin_cur$pars[3]
    tleaf = mean(kin_cur$df$Tleaf)
    filename = dat_cur$filename[1]
    fitmethod = kin_cur$fitmethod
    kinetics_data = bind_rows(kinetics_data,
                              data.frame(filename, curveID = i, fitmethod, vcmax, jmax, rd, tleaf) )
  }
  # 
  # # Pull kinetics data
  # kinetics_data = c()
  # for (i in 1:length(extracted)) {
  #   if(extracted[[i]]$fitmethod == "bilinear") { next }
  #   #print(i)
  #   vcmax = extracted[[i]]$pars[1]
  #   jmax = extracted[[i]]$pars[2]
  #   rd = extracted[[i]]$pars[3]
  #   tleaf = mean(extracted[[i]]$df$Tleaf)
  #   filename = extracted[[i]]$df$filename[1]
  #   kinetics_data = bind_rows(kinetics_data,
  #                             data.frame(filename, vcmax, jmax, rd, tleaf) )
  # }
  # 
  # Now we need to break down the filename into taxon an replicate
  kinetics_data$taxon = NA
  kinetics_data[kinetics_data$filename %>% grep("rasa", .),]$taxon = "RASA"
  kinetics_data[kinetics_data$filename %>% grep("tito", .),]$taxon = "TITO"
  
  kinetics_data$replicate = kinetics_data$filename %>% str_match("r[0-9]+") %>% str_extract("[0-9]+")
  
  kinetics_data = kinetics_data %>% select(-filename)
  
  kinetics_data = kinetics_data %>% group_by(taxon, replicate) %>% mutate(curveID = cur_group_id())
  
  # Fit something - pawar, or something else. Gotta get some values here
  #vcmax.param = fit_curves_michaletz_2021(kinetics_data, x = "tleaf", y = "vcmax")
  #jmax.param = fit_curves_michaletz_2021(kinetics_data, x = "tleaf", y = "jmax")
  # Not all were fit, we may need to do some fiddling
  #vcmax.param = kinetics_data %>% mutate(A = vcmax, Tleaf = tleaf, species = taxon, rep = replicate) %>% fit.pawar()
  #jmax.param = kinetics_data %>% mutate(A = jmax, Tleaf = tleaf, species = taxon, rep = replicate) %>% fit.pawar()

  vcmax.param = kinetics_data %>% mutate(A = vcmax, Tleaf = tleaf, species = taxon, rep = replicate) %>% fit.medlyn()
  jmax.param = kinetics_data %>% mutate(A = jmax, Tleaf = tleaf, species = taxon, rep = replicate) %>% fit.medlyn()
  
  # Maybe it would actually make more sense to just fit the Medlyn model directly,
  # because it is parameterized the way I want?
  
  
  # # Quick visual check here:
  # i = 1
  # k1 = subset(kinetics_data, curveID == i)
  # v1 = subset(vcmax.param, curveID == i)
  # j1 = subset(jmax.param, curveID == i)
  # plot(k1$tleaf, k1$vcmax)
  # lines(1:50, pawar_2018(1:50, v1$r_tref, v1$e, v1$eh, v1$topt, tref = 10))
  # plot(k1$tleaf, k1$jmax)
  # lines(1:50, pawar_2018(1:50, j1$r_tref, j1$e, j1$eh, j1$topt, tref = 10))
  # i = i+1
  
  # Despite some weird values, all the fits look good
  
  # Now we have to convert to using the Medlyn 2002 form
  # 
  # R = 8.314 # J/molK
  # k = 8.617e-5 #eV/K
  
  AT_FvCB = c()
  for (i in unique(vcmax.param$curveID)) {
    #i = 4
    # dSC = vcmax.param$eh[i]*R/k/(vcmax.param$topt[i]+273) + R*log(vcmax.param$e[i]/(vcmax.param$eh[i]-vcmax.param$e[i]))
    # dSJ = jmax.param$eh[i]*R/(jmax.param$topt[i]+273)/k + R*log(jmax.param$e[i]/(jmax.param$eh[i]-jmax.param$e[i]))
    # 
    
    photo.sim = Photosyn(Tleaf = 15:40,
                         Jmax = jmax.param$r_tref[i],
                         Vcmax = vcmax.param$r_tref[i],
                         EaV = vcmax.param$e[i],
                         EdVC = vcmax.param$eh[i],
                         delsC = vcmax.param$ds[i],
                         EaJ = jmax.param$e[i],
                         EdVJ = jmax.param$eh[i],
                         delsJ = jmax.param$ds[i],
                         PPFD = 600,
                         Tcorrect = T)
    
    #plot(photo.sim$Tleaf, photo.sim$ALEAF)
    
    
    AT_FvCB = bind_rows(AT_FvCB,
    data.frame(curveID = vcmax.param$curveID[i], species = vcmax.param$species[i], rep = vcmax.param$rep[i], A = photo.sim$ALEAF, Tleaf = photo.sim$Tleaf, method = "FvCB"))
    
    # R = 8.314 # J/molK
    # k = 8.617e-5 #eV/K
    # vcmax.param$E[i]*R/k
    
    # I need to try to force the curve fitting algorithm to get fits here to as many as possible
    # Missing curve fits to most of them.
    
    # Also need to select a T range (15 - 38 seems consistent with the step method)
    
    # Then, once this works for all, loop over each dataset to make simulated AT curves
    
    
  
  }
  
  
  # Probably need to bind together the simulated AT and return it
  return(AT_FvCB)
  
} 
  
  
  
  