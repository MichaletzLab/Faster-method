# Figure 1: Illustration of the Faster method
make_fig1 = function(data) {
  
  # Select an exemplary curve
  curdat = subset(data$AT_faster, rep == 4)[1:750,]
  
  # Make dataframe in nice format for plotting
  plot.df = data.frame(
    y = c(curdat$Txchg, curdat$Tleaf, 6*(curdat$A-26)),
    x = curdat$elapsed/60,
    label = c(rep("Txchg", 750), rep("Tleaf", 750), rep("A", 750))
  )
  
  # Create color palette
  pal1 = c("#CC79A7", "#999999", "black")
  
  # Make plot: Time series of different variables
  p1 = ggplot(plot.df, aes(x = x, y = y, color = label)) +
    geom_line() +
    scale_color_manual(values=pal1,labels=c(expression(A), expression(T[leaf]), expression(T[xchg]))) +
    my_theme +
    theme(legend.position = c(0.8,0.25),
          legend.text.align = 0) +
    theme(legend.title = element_blank()) +
    theme(legend.background = element_rect(fill='transparent')) +
    xlab("Time (min)") +
    scale_y_continuous(name = "Temperature (ºC)", 
                       sec.axis = sec_axis(~./6+26, name = "Assimilation rate (µmol/m²s)")) 

  # Save plot to file
  svg("figures/fig1.svg", width = 3, height = 2.5)
  grid.arrange(plot_grid(p1, ncol = 1, align = "v"))
  dev.off()
}
