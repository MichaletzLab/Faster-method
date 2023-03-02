# Figure S2: Raw AT curves
make_figS2 = function(data.AT) {
  
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
  
  # Assign new curveIDs
  data.AT$curveID = data.AT %>%  group_by(species, rep, method) %>% group_indices()
  data.AT = data.AT %>% select(curveID, species, rep, method, A, Tleaf)
  
  # Rename labels
  data.AT$method[data.AT$method == "step"] = "SEM"
  data.AT$method[data.AT$method == "chamber"] = "SEM-ATC"
  data.AT$method[data.AT$method == "faster"] = "FAsTeR"
  
  # Color palette
  pals2 =  c("#44AA99", "#882255", "#E69F00")
  
  # Make plot
  p=ggplot(data.AT, aes(x = Tleaf, y = A, color = method)) +
    geom_point(size=0.5)+
    geom_line(aes(group=curveID)) +
    scale_color_manual(values = pals2) +
    my_theme +
    xlab("Leaf temperature (°C)") +
    ylab("Assimilation rate (µmol/m²s)") +
    theme(legend.title = element_blank(),legend.position = c(0.5,0.2))
  
  # Save plot to file
  svg("figures/figS2.svg",4,4)
  grid.arrange(p)
  dev.off()
  
}