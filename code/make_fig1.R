# Figure 1: Illustration of the Faster method
make_fig1 = function(data) {
  
  # Select an exemplary curve
  curdat = subset(data$AT_faster, rep == 137 & method == "faster") 
  
  len = dim(curdat)[1]
  curdat = curdat[1:len,]
  
  # Make dataframe in nice format for plotting
  plot.df = data.frame(
    y = c(curdat$Txchg, curdat$Tleaf, 11*(curdat$A-8)),
    x = curdat$elapsed/60,
    label = c(rep("Txchg", len), rep("Tleaf", len), rep("A", len))
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
                       sec.axis = sec_axis(~./10+8, name = "Assimilation rate (µmol/m²s)")) 

  # Save plot to file
  pdf("figures/fig1.pdf", width = 3, height = 2.5)
  grid.arrange(plot_grid(p1, ncol = 1, align = "v"))
  dev.off()
}
