# Figure 5: Effects of data density on parameter estimation
make_fig5 = function(data) {
  
  # Set random number seed
  set.seed(0)
  
  # Color palettes
  pal1 = c("#88CCEE", "#cc6677", "#999933", "#117733")
  pal2 = c("#0072b2", "#E69f00")
  
  # Make panel 1
  ## Grab one faster curve, correct the curve
  curdat = subset(data$AT_faster, rep == 141 & method == "faster") %>% 
    match_correct() %>% 
    noneq_correct_full(dt1_c = 1.58, dt2_c = 1.21, aV_c = 67.26, dt1_h = 2.23, dt2_h = 2.79, aV_h = 78.55)
    
  ## Subsample it at different densities
  lengths = c(5:25,seq(30,100,5),200,300,400,450)
  
  all.sub=c()
  # When subsmapling, take first and last point, then distribute the rest evenly
  for(i in lengths) {
  
    len = dim(curdat)[1]
    sub = curdat[seq(from=1,to=len,length.out=i),]
    sub$no_pts = i
    all.sub = bind_rows(all.sub,sub)
  }
  dat = all.sub %>% select(curveID = no_pts, Tleaf, Photo = A)
  
  ## Fit model to each curve - parallel version
  print("Fitting curves, please wait...")
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
  results = as.data.frame(do.call(rbind, results))
  print("Curve fitting complete")
  
  ## Get parameter estimates and STDERR
  results$r_tref_SE_pct = results$r_tref_SE/results$r_tref
  results$e_SE_pct = results$e_SE/results$e
  results$eh_SE_pct = results$eh_SE/results$eh
  results$topt_SE_pct = results$topt_SE/results$topt
  
  # Reshape data
  results_long = results %>% gather(key = "variable", "SE_pct", r_tref_SE_pct, e_SE_pct, eh_SE_pct, topt_SE_pct)
  
  ## Panel 1: number of points effects on parameter SE
  bks = c(5,15,50,150,450)
  p1 = ggplot(data = results_long, aes(x = curveID, y = 100*SE_pct, color = variable)) +
    geom_line() +
    scale_x_log10(breaks=bks,labels=bks) +
    scale_color_manual(values=pal1,breaks=c("r_tref_SE_pct", "e_SE_pct", "eh_SE_pct", "topt_SE_pct"),   labels=c(expression(A[max]), expression(E[A]), expression(E[D]), expression(T[opt]))) +
    my_theme +
    theme(legend.position = c(0.8,0.75),
          legend.title = element_blank()) +
    xlab("Measurements per curve") +
    ylab("Parameter estimate standard error (%)") +
    annotate("text", x = 5, y = 35, label="(a)")
  
  #
  #
  #
  # Make panel 2
  
  ## Grab one faster curve and one normal curve
  faster.curve = curdat %>% mutate(Anorm = A/max(A))
  normal.curve = faster.curve[seq(from=1,to=len,length.out=10),]

  ## Introduce gaussian noise at progressively larger levels and fit curves
  nrep = 100 # Number of repetitions at each noise level
  noise.levels = seq(0.001,0.03, 0.001) # SD of gaussian noise
  ntot = nrep*length(noise.levels)
  
  len = dim(curdat)[1]
  
  # We need to copy each curve a number of times equal to noise levels times nrep
  faster.curves = faster.curve[rep(1:len,ntot),]
  normal.curves = normal.curve[rep(1:10,ntot),]
  
  faster.curves$noise.level = rep(noise.levels, each=(nrep*len))
  normal.curves$noise.level = rep(noise.levels, each=(nrep*10))
  
  faster.curves$curveID = rep(1:ntot, each=len)
  normal.curves$curveID = rep(1:ntot, each=10)
  
  # Add noise
  func1 <- function(x, y) {rnorm(1, mean = x, sd = y) }
  list1 <- Map(func1, 0, faster.curves$noise.level)
  
  faster.curves$A_noise = faster.curves$Anorm + as.numeric(Map(func1, 0, faster.curves$noise.level))
  normal.curves$A_noise = normal.curves$Anorm + as.numeric(Map(func1, 0, normal.curves$noise.level))
  
  faster.curves = bind_rows(faster.curve %>% mutate(A_noise = Anorm, noise.level = 0, curveID = 1), 
                            faster.curve %>% mutate(A_noise = Anorm, noise.level = 0, curveID = 2),
                            faster.curves)
  normal.curves = bind_rows(normal.curve %>% mutate(A_noise = Anorm, noise.level = 0, curveID = 1),
                            normal.curve %>% mutate(A_noise = Anorm, noise.level = 0, curveID = 2),
                            normal.curves)
  
  faster.dat = faster.curves %>% select(curveID, Tleaf, Photo = A_noise, noise.level) %>% mutate(type = "High")
  normal.dat = normal.curves %>% select(curveID, Tleaf, Photo = A_noise, noise.level) %>% mutate(type = "Low")
  
  dat = bind_rows(faster.dat, normal.dat)
  dat$curveID = dat %>% group_by(type, curveID) %>% group_indices()
  
  ## Fit curves with parallel processing function
  print("Fitting curves, please wait...")
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
  results = as.data.frame(do.call(rbind, results))
  
  print("Curve fitting complete")
  
  # Collect results and compute SD
  results.summary = results %>% 
    group_by(noise.level, type) %>% 
    summarize(mean_r_tref = mean(r_tref), sd_r_tref = sd(r_tref),
              mean_e = mean(e), sd_e = sd(e),
              mean_eh = mean(eh), sd_eh = sd(eh),
              mean_topt = mean(topt), sd_topt = sd(topt))
  
  # Reshape data
  results.plot.data = results.summary %>% 
    gather(key = "parameter", value = "value", mean_r_tref, mean_e, mean_eh, mean_topt)
  
  results.sd.data = results.plot.data
  results.sd.data$sd = NA
  results.sd.data[results.sd.data$parameter == "mean_r_tref",]$sd = results.sd.data[results.sd.data$parameter == "mean_r_tref",]$sd_r_tref
  results.sd.data[results.sd.data$parameter == "mean_e",]$sd = results.sd.data[results.sd.data$parameter == "mean_e",]$sd_e
  results.sd.data[results.sd.data$parameter == "mean_eh",]$sd = results.sd.data[results.sd.data$parameter == "mean_eh",]$sd_eh
  results.sd.data[results.sd.data$parameter == "mean_topt",]$sd = results.sd.data[results.sd.data$parameter == "mean_topt",]$sd_topt
  
  # Make shaded interval regions
  results.sd.data = results.sd.data %>% mutate(upper = value+sd, lower = value-sd) 
  
  # Fix labels
  plot.data.2 = results.plot.data %>% rename(Density = type)
  sd.data.2 = results.sd.data %>% rename(Density = type)

 # Panel 2: A_max
 a = subset(plot.data.2, parameter == "mean_r_tref")
 b = subset(sd.data.2, parameter == "mean_r_tref")
 p2 = ggplot(a, aes(x=100*noise.level, y = 100*value, color = Density)) +
   geom_line() +  
   geom_ribbon(data=b, aes(x =100*noise.level, ymin = 100*lower, ymax = 100*upper, fill=Density), color=NA,alpha = 0.2)+
   my_theme +
   scale_color_manual(values=pal2) +
   scale_fill_manual(values=pal2) +
   xlab(NULL) +
   ylim(96,103) +
   ylab(bquote(A[max]~"(%)")) +
   annotate("text", x = 0.2, y = 103, label="(b)") +
   theme(legend.position = c(0.25,0.25)) +
   theme(legend.background = element_blank(),
         legend.box.background = element_blank(),
         legend.key = element_blank()) +
   theme(legend.key.size = unit(0.5, 'cm'))
 
 # Panel 3: E_A
 a = subset(plot.data.2, parameter == "mean_e")
 b = subset(sd.data.2, parameter == "mean_e")
 p3 = ggplot(a, aes(x=100*noise.level, y = value, color = Density)) +
   geom_line() +  
   geom_ribbon(data=b, aes(x =100*noise.level, ymin = lower, ymax = upper, fill=Density), color=NA,alpha = 0.2)+
   my_theme +
   scale_color_manual(values=pal2) +
   scale_fill_manual(values=pal2) +
   xlab(NULL) +
   ylab(bquote(E[A]~"(eV)")) +
   annotate("text", x = 0.2, y = max(b$upper), label="(c)")
 
 # Panel 4: eh
 a = subset(plot.data.2, parameter == "mean_eh")
 b = subset(sd.data.2, parameter == "mean_eh")
 p4 = ggplot(a, aes(x=100*noise.level, y = value, color = Density)) +
   geom_line() +  
   geom_ribbon(data=b, aes(x =100*noise.level, ymin = lower, ymax = upper, fill=Density), color=NA,alpha = 0.2)+
   my_theme +
   scale_color_manual(values=pal2) +
   scale_fill_manual(values=pal2) +
   xlab("Std. dev. of noise (%)") +
   ylab(bquote(E[D]~"(eV)")) +
   annotate("text", x = 0.2, y = max(b$upper), label="(d)") 
  
 # Panel 5: topt
 a = subset(plot.data.2, parameter == "mean_topt")
 b = subset(sd.data.2, parameter == "mean_topt")
 p5 = ggplot(a, aes(x=100*noise.level, y = value, color = Density)) +
   geom_line() +  
   geom_ribbon(data=b, aes(x =100*noise.level, ymin = lower, ymax = upper, fill=Density), color=NA,alpha = 0.2)+
   my_theme +
   scale_color_manual(values=pal2) +
   scale_fill_manual(values=pal2) +
   xlab("Std. dev. of noise (%)") +
   ylab(bquote(T[opt]~"(°C)")) +
   annotate("text", x = 0.2, y = max(b$upper), label="(e)") 
  
  # Save plot to file
  pdf("figures/fig5.pdf",width=4,height=6.5)
  grid.arrange(p1,p2,p3,p4,p5,
               heights = c(2,1,1),
               layout_matrix = rbind(c(1, 1),
                                     c(2, 3),
                                     c(4, 5)))
  dev.off()
}
