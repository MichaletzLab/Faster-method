# Figure 3: Non-equilibrium corrections
make_fig3 = function(data) {

  # Select an AT curve
  curdat = subset(data$AT_faster, rep == 4)

  # Create dataframe with corrected and uncorrected data
  data.plot = bind_rows(
    curdat %>% match_correct() %>% mutate(Condition = "Uncorrected"),
    curdat %>% match_correct() %>% noneq_correct_full(dt1 = 0.99, dt2 = 0.91, aV = 66.65) 
    %>% mutate(Condition = "Corrected")
  )
  
  # Color palette
  pal3 = c("#CC79A7", "#009E73", "#999999")
  
  # Reorder factor
  data.plot$Condition = factor(data.plot$Condition, levels = c("Uncorrected", "Corrected"))
  
  # Panel 1: comparison of correct and uncorrected AT curves
  p1=ggplot(data = data.plot, aes(x=Tleaf, y=A, color=Condition)) +
    geom_line() +
    scale_color_manual(values=pal3[c(1,2)]) +
    my_theme +
    xlab("Leaf temperature (ºC)") +
    ylab("Assimilation rate (µmol/m²s)") +
    theme(legend.title = element_blank()) +
    theme(legend.position = c(0.22,0.88)) +
    ylim(26,35) +
    theme(legend.background = element_rect(fill='transparent'))

  # Remake DF in wide format
  data.wide = spread(data.plot, key = "Condition", value = "A")
  
  # Panel 2: Corrected vs. uncorrected values
  p2=ggplot(data = data.wide, aes(x = Uncorrected, y = Corrected)) +
    my_theme +
    geom_point(color = pal3[3], size = 0.3, alpha = 0.5) +
    geom_abline(slope = 1, intercept = 0, lty = 2) +
    xlim(26,34) +
    ylim(26,34) +
    xlab("Uncorrected") +
    ylab("Corrected") +
    theme(axis.text=element_text(size=6),
          axis.title=element_text(size=8))

  # Remake panel 2 as an inset plot
  inset_plot_2 = ggplotGrob(p2)

  # Combine main plot and inset  
  p3 = p1 + annotation_custom(grob = inset_plot_2, xmin = 22, xmax = 35, ymin = 25.5, ymax = 30.5)
  
  # Save plot to file
  svg("figures/fig3.svg", width = 3.5, height = 3.5)
  grid.arrange(plot_grid(p3, ncol = 1, align = "v"))    
  dev.off()
}
