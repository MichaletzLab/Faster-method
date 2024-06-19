# Raw data supplementary figure
make_figS2 = function(data.AT) {
  

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
  
  # Rename labels
  data.AT$method[data.AT$method == "step"] = "SEM"
  data.AT$method[data.AT$method == "chamber"] = "SEM-ATC"
  data.AT$method[data.AT$method == "faster"] = "FAsTeR"
  
  pal6 = c("#44AA99", "#882255", "#E69F00")
  
  
  # Raw data
  p = ggplot(data.AT, aes(x = Tleaf, y = A, color = method)) +
    geom_point(size = 0.5) +
    geom_line(aes(group=curveID)) +
    my_theme +
    xlab("Leaf temperature (°C)") +
    ylab("Assimilation rate (µmol/m²s)") +
    theme(strip.background = element_blank(),
          strip.text.x = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank()) +
    scale_color_manual(values=pal6) +
    facet_wrap(~rep, ncol = 3)
  
  pdf("figures/figS2.pdf",width = 4, height=5)
  grid.arrange(p)
  dev.off()
  
}
