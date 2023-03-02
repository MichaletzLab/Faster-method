# Methods comparison ANOVA table
make_tab1 = function(data.AT) {
  
  # Do nonequilibrium and IRGA corrections
  faster900 = subset(data.AT$AT_faster, Flow > 700) %>% 
    match_correct() %>% noneq_correct_full(dt1 = 0.99, dt2 = 0.91, aV = 66.65)
  faster600 = subset(data.AT$AT_faster, Flow <= 700) %>% 
    match_correct() %>% noneq_correct_full(dt1 = 2.31, dt2 = 1.51, aV = 66.34)
  
  # Make AT dataframe
  data.AT = bind_rows(data.AT$AT_step, 
                      data.AT$AT_chamber, 
                      faster900,
                      faster600)
  data.AT$curveID = data.AT %>%  group_by(species, rep, method) %>% group_indices()
  data.AT = data.AT %>% select(curveID, species, rep, method, A, Tleaf)
  
  data.AT$method = as.factor(data.AT$method)
  
  # Fit NLME model
  photo.nlme.1 = nlme(A ~ pawar_2018(Tleaf, r_tref, e, eh, topt, tref = 10),
                      data = data.AT,
                      fixed = r_tref + e + eh + topt ~ 1,
                      random = pdDiag(r_tref + e + eh + topt ~ 1),
                      groups = ~ curveID,
                      start = c(r_tref = 15, e = 0.71, eh = 1.46, topt = 25),
                      na.action = na.omit,
                      method = "ML",
                      verbose = F)
  
  photo.nlme.2 = update(photo.nlme.1, 
                        fixed = r_tref + e + eh + topt ~ method,
                        start = c(22.3, 0,0, 0.46,0,0, 1.3,0,0, 24.8,0,0),
                        verbose = F,
                        method = "REML")

  # Write results to file
  write.table(anova(photo.nlme.2), "stats.csv", sep=",", row.names = F)
 
}
