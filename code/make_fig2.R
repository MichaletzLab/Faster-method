# Figure 2: IRGA drift correction
make_fig2 = function(data) {
  
  # Select a representative AT curve
  curdat = subset(data$AT_faster, rep == 4)#[1:750,]
  
  # Make DF including just the match test points
  test.points.df = data.frame(
    Tleaf = curdat$Tleaf[c(1, 751)],
    CO2_offset = curdat$co2_adj[c(1,751)],
    H2O_offset = curdat$h2o_adj[c(1,751)],
    A = curdat$A[c(1, 751)]
  )
  
  # Make DF illustrating match offset interpolation
  match.offset.df = data.frame(
    Txchg = curdat$Txchg[c(1, 751)],
    offset = c(curdat$co2_adj[c(1,751)],curdat$h2o_adj[c(1,751)]),
    label = c("CO2", "CO2", "H2O", "H2O")
  )
  
  # Make DF illustrating AT curve before and after IRGA correction
  comparison.df = bind_rows(
    curdat[1:750,] %>% mutate(label = "Uncorrected"),
    curdat %>% match_correct() %>% mutate(label = "Corrected")
  )
  
  # Color palette
  pal2 = c("#CC79A7", "#009E73")
  
  # Panel 1: illustrate the match test points
  p1 = ggplot(curdat, aes(x = Tleaf, y = A)) + 
    geom_line(color = pal2[1]) +
    geom_point(data = test.points.df, fill = pal2[1], pch = 21) +
    annotate("segment", x = 20, xend = 15.5, y = 26.5, yend = 26,
             size = 1, arrow = arrow(length = unit(.2,"cm"))) +
    annotate("segment", x = 31, xend = 35, y = 28.7, yend = 29.5,
             size = 1, arrow = arrow(length = unit(.2,"cm"))) +
    annotate("text", x = 27,y = 26.5,
             size = 4,label = "Initial match point") +
    annotate("text", x = 24,y = 28.5,
             size = 4,label = "Final match point") +
    annotate("text", x = 14, y = 33.5, label="(a)") +
    xlab("Leaf temperature (°C)") +
    ylab("Assimilation rate (µmol/m²s)") +
    my_theme

  # Panel 2: illustrate interpolating the match values 
  p2 = ggplot(match.offset.df, aes (x = Txchg, y = offset, pch = label)) +
    geom_path(aes(lty = label)) +
    geom_point() +
    scale_shape_manual(values = c(19,17), labels = c(expression(CO[2]), expression(H[2]*O))) +
    scale_linetype_manual(values = c(1,2),labels = c(expression(CO[2]), expression(H[2]*O))) +
    my_theme +
    theme(legend.position = c(0.2,0.2)) +
    theme(legend.title = element_blank()) +
    annotate("text", x = 7, y = 0.27, label="(b)") +
    xlab("Heat exchanger temperature (°C)") +
    scale_y_continuous(name = bquote(CO[2]~"match offset (µmol/mol)"),
                       sec.axis = sec_axis(~., name = bquote(H[2]*"O match offset (mmol/mol)"))) 
  
  # Panel 3: AT curve before and after IRGA correction
  p3 = ggplot(comparison.df, aes(x = Tleaf, y = A, color = label)) +
    geom_line() +
    scale_color_manual(values=pal2[c(2,1)]) +
    my_theme +
    xlab("Leaf temperature (°C)") +
    ylab("Assimilation rate (µmol/m²s)") +
    annotate("text", x = 14, y = 34, label="(c)") +
    theme(legend.position = c(0.7,0.2)) +
    theme(legend.title = element_blank()) +
    theme(legend.background = element_rect(fill='transparent'))

  # Save plot to file
  svg('figures/fig2.svg', width = 3.5, height = 7.5)
  grid.arrange(plot_grid(p1, p2, p3, ncol = 1, align = "v"))
  dev.off()  
}
