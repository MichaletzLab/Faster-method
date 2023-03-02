# Figure 4: comparison of parameter values
make_fig4 = function(data.AT) {

  # Set random number seed
  set.seed(0)
  
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
  
  # Do the curve fitting necessary to extract  parameters of interest
  #pawar.params = fit.pawar(data.AT)
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
  pawar.barplot.2$method[pawar.barplot.2$method == "step"] = "SEM"
  pawar.barplot.2$method[pawar.barplot.2$method == "chamber"] = "SEM-ATC"
  pawar.barplot.2$method[pawar.barplot.2$method == "faster"] = "FAsTeR"
  
  # Color palette
  pal6 = c("#44AA99", "#882255", "#E69F00")
  
  z1 = lm(r_tref ~ method, data = pawar.params)
  z2 = lm(e ~ method, data = pawar.params)
  z3 = lm(eh ~ method, data = pawar.params)
  z4 = lm(topt ~ method, data = pawar.params)
  
  # Panel 1: A_10
  a = subset(pawar.barplot.2, parameter == "r_tref")
  p1 = ggplot(a, aes(x=method, y = mean_param, fill = method)) +
    geom_bar(stat="identity", color = "black")+
    geom_errorbar(aes(ymin=mean_param-se, ymax=mean_param+se), width=0.2) +
    scale_fill_manual(values=pal6) +
    my_theme +
    theme(legend.position = c(0.4,0.8)) +
    theme(legend.title = element_blank()) +
    theme(axis.ticks.x = element_blank()) +
    theme(axis.text.x = element_blank()) +
    theme(legend.background = element_blank(),
  legend.box.background = element_blank(),
  legend.key = element_blank()) +
    theme(legend.key.size = unit(0.5, 'cm')) +
    xlab(NULL) +
    ylab(bquote(A[10]~"(µmol/m²s)")) +
    annotate("text", x = 0.75, y = max(a$mean_param+a$se)+10, label="(a)") +
    annotate("text", x = 2.5, y = max(a$mean_param+a$se)+10, label=paste("p =", round(anova(z1)$`Pr(>F)`[1],2)))
  
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
    annotate("text", x = 0.75, y = max(a$mean_param+a$se)+.1, label="(b)") +
    annotate("text", x = 2.5, max(a$mean_param+a$se)+.1, label=paste("p =", round(anova(z2)$`Pr(>F)`[1],2)))
  
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
    annotate("text", x = 0.75, y = max(a$mean_param+a$se)+0.2, label="(c)") +
    annotate("text", x = 2.5, y = max(a$mean_param+a$se)+0.2, label=paste("p =", round(anova(z3)$`Pr(>F)`[1],2)))
  
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
    annotate("text", x = 0.75, y = max(a$mean_param+a$se)+3, label="(d)") +
    annotate("text", x = 2.5, y = max(a$mean_param+a$se)+3, label=paste("p =", round(anova(z4)$`Pr(>F)`[1],2)))
  
  # Save plot to file
  svg("figures/fig4.svg",width = 3.5, height=5)
  grid.arrange(p1, p2, p3, p4, ncol=2)
  dev.off()
}
