make_fig4 = function(data.AT) {
  
  # Set random number seed
  set.seed(0)
  
  # Do nonequilibrium and IRGA corrections
  faster = subset(data.AT$AT_faster) %>% 
    match_correct() %>% 
    noneq_correct_full(dt1_c = 1.58, dt2_c = 1.21, aV_c = 67.26, dt1_h = 2.23, dt2_h = 2.79, aV_h = 78.55)

  # Make AT dataframe
  data.AT = bind_rows(data.AT$AT_step, 
                      data.AT$AT_chamber, 
                      faster)
  data.AT$curveID = data.AT %>%  group_by(rep, method) %>% group_indices()
  data.AT = data.AT %>% select(curveID, rep, method, A, Tleaf)
  
  # Remove extreme outlier
  data.AT = subset(data.AT, rep != 141) #%>% 

  # Do the curve fitting necessary to extract parameters of interest
  print("Fitting curves, please wait...")
  dat = data.AT %>% rename(Photo = A)
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
  
  # Color palette
  pal6 = c("#44AA99", "#882255", "#E69F00")
  
  # Panel 1: A_max
  a = subset(pawar.barplot.2, parameter == "r_tref")
  p1 = ggplot(a, aes(x=method, y = mean_param, fill = method)) +
    geom_bar(stat="identity", color = "black")+
    geom_errorbar(aes(ymin=mean_param-se, ymax=mean_param+se), width=0.2) +
    scale_fill_manual(values=pal6) +
    my_theme +
    theme(legend.position = c(0.55,0.85)) +
    theme(legend.title = element_blank()) +
    theme(axis.ticks.x = element_blank()) +
    theme(axis.text.x = element_blank()) +
    theme(legend.background = element_blank(),
          legend.box.background = element_blank(),
          legend.key = element_blank()) +
    theme(legend.key.size = unit(0.5, 'cm')) +
    xlab(NULL) +
    ylab(bquote(A[max]~"(µmol/m²s)")) +
    annotate("text", x = 2, y = 9, label = "n.s.") +
    annotate("text", x = 0.75, y = max(a$mean_param+a$se)+6, label="(a)") #+

  # Panel 2: E_A
  a = subset(pawar.barplot.2, parameter == "e")
  p2 = ggplot(a, aes(x=method, y = mean_param, fill = method)) +
    geom_bar(stat="identity", color="black")+
    geom_errorbar(aes(ymin=mean_param-se, ymax=mean_param+se), width=0.2) +
    scale_fill_manual(values=pal6) +
    my_theme +
    theme(axis.ticks.x = element_blank()) +
    theme(axis.text.x = element_blank()) +
    xlab(NULL) +
    ylab(bquote(E[A]~"(eV)")) +
    annotate("text", x = 2, y = 1.8, label = "n.s.") +
    annotate("text", x = 0.75, y = max(a$mean_param+a$se)+.1, label="(b)") #+

  # Panel 3: E_D
  a = subset(pawar.barplot.2, parameter == "eh")
  p3 = ggplot(a, aes(x=method, y = mean_param, fill = method)) +
    geom_bar(stat="identity", color="black")+
    geom_errorbar(aes(ymin=mean_param-se, ymax=mean_param+se), width=0.2) +
    scale_fill_manual(values=pal6) +
    my_theme +
    theme(axis.ticks.x = element_blank()) +
    theme(axis.text.x = element_blank()) +
    xlab(NULL) +
    ylab(bquote(E[D]~"(eV)")) +
    annotate("text", x = 1, y = 1.25, label = "a") +
    annotate("text", x = 2, y = 2.05, label = "b") +
    annotate("text", x = 3, y = 1.7, label = "ab") +
    annotate("text", x = 0.75, y = max(a$mean_param+a$se)+0.2, label="(c)") #+

  # Panel 4: Topt
  a = subset(pawar.barplot.2, parameter == "topt")
  p4 = ggplot(a, aes(x=method, y = mean_param, fill = method)) +
    geom_bar(stat="identity", color="black")+
    geom_errorbar(aes(ymin=mean_param-se, ymax=mean_param+se), width=0.2) +
    scale_fill_manual(values=pal6) +
    my_theme +
    theme(axis.ticks.x = element_blank()) +
    theme(axis.text.x = element_blank()) +
    xlab(NULL) +
    ylab(bquote(T[opt]~"(°C)")) +
    annotate("text", x = 2, y = 21, label = "n.s.") +
    annotate("text", x = 0.75, y = max(a$mean_param+a$se)+3, label="(d)") #+

  svg("figures/fig4.svg",width = 3.5, height=5)
  grid.arrange(p1,p2,p3,p4)
  dev.off()
}
