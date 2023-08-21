make_tabS2 = function(data) {
  
  data.AT = data
  
  
  # Do nonequilibrium and IRGA corrections
  faster = data.AT$AT_faster %>% 
    match_correct() %>% 
    noneq_correct_full(dt1_c = 1.58, dt2_c = 1.21, aV_c = 67.26, dt1_h = 2.23, dt2_h = 2.79, aV_h = 78.55)

  # Make AT dataframe
  data.AT = bind_rows(data.AT$AT_step, 
                      data.AT$AT_chamber, 
                      faster)
  data.AT$curveID = data.AT %>%  group_by(rep, method) %>% group_indices()
  data.AT = data.AT %>% select(curveID, rep, method, A, Tleaf, gsw)
  
  # Remove extreme outlier
  data.AT = subset(data.AT, rep != 141)
  
  gsw_data = data.AT %>% group_by(rep, method) %>% 
    summarize(gsw = mean(gsw))  %>% as.data.frame()
  
  # Then do the repeat measures ANOVA on these
  res.aov.gsw <- anova_test(data = gsw_data, dv = gsw, wid = rep, within = method)
  #print(gsw_data %>% pairwise_t_test(gsw ~ method, paired = TRUE))
  
  gsw_res = gsw_data %>% group_by(method) %>% summarize(mean(gsw)) %>% as.data.frame()
  gsw_se = gsw_data %>% group_by(method) %>% summarize(sd(gsw)/sqrt(length(gsw))) %>% as.data.frame()
  
  colnames(gsw_res)[2] = "Mean gsw"
  colnames(gsw_se)[2] = "StdErr" 
  
  gsw_res = merge(gsw_res, gsw_se)
  
  gsw_res$method = c("SEM-ATC", "Faster", "SEM")
  
  #print(gsw_data %>% pairwise_t_test(gsw ~ method, paired = TRUE))
  
  sink("stats.txt", append = T)
  cat("========\n")
  cat("Table S2:\n")
  cat("========\n\n")
  
  print(gsw_res)
  print(res.aov.gsw)
  
  # Then print tukey results -  E,E_D, Topt is signficant
  cat("\nTukey test for gsw:\n")
  print(gsw_data %>% pairwise_t_test(gsw ~ method, paired = TRUE))
  
  cat("\n\n")
  sink()
  
  
  
}
