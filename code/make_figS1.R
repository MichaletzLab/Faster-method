# Figure 1: Linearity of match offset
make_figS1 = function() {
  
  # Read match test files
  all_files = list.files("data/match_tests", recursive = T, full.names = T) %>% 
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
  
  # Only using exchange temperatures below 40 now in main data
  data_unpack = subset(data_unpack, Txchg <= 40)
 
  # Panel 1 co2 match value
  p1 = ggplot(data_unpack, aes(x=Txchg, y = co2_adj, color = curveID)) +
    geom_point(alpha=0.7) +
    geom_smooth(method="lm",aes(color=NULL), color="black") +
    my_theme +
    annotate("text", label = "(a)", x = 8,y=0.14) +
    xlab(NULL) +
    ylab(bquote(CO[2]~"match offset (µmol/mol)"))
  
  # Panel 2: h2o match value
  p2 = ggplot(data_unpack, aes(x=Txchg, y = h2o_adj, color = curveID)) +
    geom_point(alpha=0.7) +
    geom_smooth(method="lm",aes(color=NULL), color="black") +
    my_theme +
    annotate("text", label = "(b)", x = 8,y=0.02) +
    xlab(NULL) +
    ylab(bquote(H[2]*"O match offset (µmol/mol)"))
  
  # Panel 3: residuals co2
  z3 = lm(co2_adj ~ Txchg, data = data_unpack)
  p3.dat = data_unpack %>% mutate(resid = resid(z3))
  p3 = ggplot(p3.dat, aes(x = Txchg, y = resid, color = curveID)) +
    geom_point(alpha=0.7) +
    geom_abline(slope = 0, intercept = 0, lty = 2) +
    my_theme +
    annotate("text", label = "(c)", x = 8,y=0.14) +
    xlab("Heat exchanger temperature (°C)") +
    ylab(bquote("Residuals (CO"[2]*") (µmol/mol)"))
  
  # Panel 4 - residuals h2o
  z4 = lm(h2o_adj ~ Txchg, data = data_unpack)
  p4.dat = data_unpack %>% mutate(resid = resid(z4))
  p4 = ggplot(p4.dat, aes(x = Txchg, y = resid, color = curveID)) +
    geom_point(alpha=0.7) +
    geom_abline(slope = 0, intercept = 0, lty = 2) +
    my_theme +
    annotate("text", label = "(d)", x = 8,y=0.02) +
    xlab("Heat exchanger temperature (°C)") +
    ylab(bquote("Residuals (H"[2]*"O) (µmol/mol)"))
  
  # Save plot to file
  svg("figures/figS1.svg", width = 6,height = 5.5)
  grid.arrange(p1,p2,p3,p4,ncol=2)
  dev.off()  
  }
