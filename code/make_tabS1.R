# Supplementary ramp speed table
make_tabS1 = function() {
  
  # Set random number seed
  set.seed(0)
  
  # Get data
  all_files = list.files("data/ramp_speed_tests", recursive = T, full.names = T) %>% 
    grep("\\.", ., value = T, invert = T)
  
  # Read licor files
  data_raw = lapply(all_files, read_6800)
  
  # Unpack the data
  data_unpack = c()
  for (i in 1:length(data_raw)) {
    data_unpack = bind_rows(data_unpack, 
                            data.frame(data_raw[i], curveID = i) %>% select(-averaging))
  }
  data_unpack$curveID = as.factor(data_unpack$curveID)
  dat = data_unpack
  
  # Now I need to match up the leaves
  dat$leafID = dat$filename %>% str_match("P[0-9]+") %>% str_extract("[0-9]+")
  dat$method = dat$filename %>% str_match("[0-9]+R")
  
  # Match correct
  dat = dat %>% match_correct()
  
  # Nonequilibrium correct
  dat = dat %>% noneq_correct_full(dt1_c = 1.58, dt2_c = 1.21, aV_c = 67.26, dt1_h = 2.23, dt2_h = 2.79, aV_h = 78.55)
  
  # Fit SS curve to each curve
  print("Fitting curves, please wait...")
  dat = dat %>% rename(Photo = A)
  dat2 = list()
  j = 1
  for (i in unique(dat$curveID)) {
    cur = subset(dat, curveID == i)
    dat2[[j]] = as.data.frame(cur)
    j = j + 1
    
  }
  system.time({
    numcores = detectCores()
    clust <- makeCluster(numcores)
    clusterExport(clust, "nls_multstart")
    clusterExport(clust, "pawar_2018")
    clusterExport(clust, "select")
    results = parLapply(clust, dat2, fit_curves_parallel)
  })
  pawar.params = as.data.frame(do.call(rbind, results))
  print("Curve fitting complete")
  
  pawar.barplot.2 = gather(pawar.params, "parameter", "value", c(r_tref, e, eh, topt)) %>%
    group_by(method, parameter) %>%
    summarise(mean_param = mean(value), se = sd(value)/sqrt(length(value)),na.rm=T) #%>%
  
  
  # Fix names
  pawar.params$method[pawar.params$method == "step"] = "SEM"
  pawar.params$method[pawar.params$method == "chamber"] = "SEM-ATC"
  pawar.params$method[pawar.params$method == "faster"] = "FAsTeR"
  
  pawar.barplot.2$method[pawar.barplot.2$method == "step"] = "SEM"
  pawar.barplot.2$method[pawar.barplot.2$method == "chamber"] = "SEM-ATC"
  pawar.barplot.2$method[pawar.barplot.2$method == "faster"] = "FAsTeR"
  
  
  e_gathered = pawar.params %>%
    select(r_tref, e, eh, topt, curveID, leafID, method)

  res.aov.rtref <- anova_test(data = e_gathered, dv = r_tref, wid = leafID, within = method)
  res.aov.e <- anova_test(data = e_gathered, dv = e, wid = leafID, within = method)
  res.aov.eh <- anova_test(data = e_gathered, dv = eh, wid = leafID, within = method)
  res.aov.topt <- anova_test(data = e_gathered, dv = topt, wid = leafID, within = method)

  # Assemble table
  
  res = e_gathered %>% group_by(method) %>% summarize(A_max = mean(r_tref),
                                                      E = mean(e),
                                                      E_D = mean(eh),
                                                      T_opt = mean(topt))

  res_se = e_gathered %>% group_by(method) %>% summarize(
    A_max = sd(r_tref)/sqrt(length(r_tref)),
    E = sd(e)/sqrt(length(e)),
    E_D = sd(eh)/sqrt(length(eh)),
    T_opt = sd(topt)/sqrt(length(topt))) 
  
  res = res[,2:5] %>% t() %>% as.data.frame()
  colnames(res) = c("1.0 C/min", "1.5 C/min", "2.0 C/min")
  res_se = res_se[,2:5] %>% t() %>% as.data.frame()
  colnames(res_se) = c("1.0 C/min StdErr", "1.5 C/min StdErr", "2.0 C/min StdErr")
  res = merge(res, res_se, by="row.names")
  res$p = NA
  
  res$p[1] = get_anova_table(res.aov.rtref)$p
  res$p[2] = get_anova_table(res.aov.e)$p
  res$p[3] = get_anova_table(res.aov.eh)$p
  res$p[4] = get_anova_table(res.aov.topt)$p
  
  sink("stats.txt", append = T)
  cat("========\n")
  cat("Table S1:\n")
  cat("========\n\n")
  
  print(res)
  # Then print tukey results -  E,E_D, Topt is signficant
  cat("\nTukey test for E:\n")
  print(e_gathered %>% pairwise_t_test(e ~ method, paired = TRUE))
  cat("\nTukey test for E_D:\n")
  print(e_gathered %>% pairwise_t_test(eh ~ method, paired = TRUE))
  cat("\nTukey test for T_opt:\n")
  print(e_gathered %>% pairwise_t_test(topt ~ method, paired = TRUE))
  
  cat("\n\n")
  sink()
  
}