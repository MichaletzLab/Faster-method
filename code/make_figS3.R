# Figure S3: Adding random noise to curves
make_figS3 = function(data) {
  
  # Set random number seed
  set.seed(0)
  
  ## Grab one faster curve, Correct the curve
  curdat = subset(data$AT_faster, rep == 1) %>% 
    match_correct() %>% noneq_correct_full(dt1 = 0.99, dt2 = 0.91, aV = 66.65)
  len = dim(curdat)[1]
  
  ## Grab one faster curve and one normal curve
  faster.curve = curdat %>% mutate(Anorm = A/max(A))
  normal.curve = faster.curve[seq(from=1,to=len,length.out=10),]
  
  ## Introduce gaussian noise at progressively larger levels and fit curves
  nrep = 1 # Number of repetitions at each noise level
  noise.levels = seq(0.01,0.03, 0.01) # SD of gaussian noise
  ntot = nrep*length(noise.levels)
  
  # We need to copy each curve a number of times equal to noise levels times nrep
  faster.curves = faster.curve[rep(1:749,ntot),]
  normal.curves = normal.curve[rep(1:10,ntot),]
  
  # Assign noise levels
  faster.curves$noise.level = rep(noise.levels, each=(nrep*749))
  normal.curves$noise.level = rep(noise.levels, each=(nrep*10))
  
  # Give a temporary curveID
  faster.curves$curveID = rep(1:ntot, each=749)
  normal.curves$curveID = rep(1:ntot, each=10)
  
  # Make function for adding noise
  func1 <- function(x, y) {rnorm(1, mean = x, sd = y) }
  list1 <- Map(func1, 0, faster.curves$noise.level)
  
  # Apply noise to curves
  faster.curves$A_noise = faster.curves$Anorm + as.numeric(Map(func1, 0, faster.curves$noise.level))
  normal.curves$A_noise = normal.curves$Anorm + as.numeric(Map(func1, 0, normal.curves$noise.level))
  
  # Add in curves with zero noise
  faster.curves = bind_rows(faster.curve %>% mutate(A_noise = Anorm, noise.level = 0, curveID = 1), 
                            faster.curve %>% mutate(A_noise = Anorm, noise.level = 0, curveID = 2),
                            faster.curves)
  normal.curves = bind_rows(normal.curve %>% mutate(A_noise = Anorm, noise.level = 0, curveID = 1),
                            normal.curve %>% mutate(A_noise = Anorm, noise.level = 0, curveID = 2),
                            normal.curves)
  
  # Label high and low density, throw out junk
  faster.dat = faster.curves %>% select(curveID, Tleaf, Photo = A_noise, noise.level) %>% mutate(type = "High")
  normal.dat = normal.curves %>% select(curveID, Tleaf, Photo = A_noise, noise.level) %>% mutate(type = "Low")
  
  # Bind together and give new curveIDs
  dat = bind_rows(faster.dat, normal.dat)
  dat$curveID = dat %>% group_by(type, curveID) %>% group_indices()
  
  # Adjust labels
  dat$type = paste(dat$type, "density")
  dat$noise.level = paste("SD =", dat$noise.level)
  
  # Build plot
  p = ggplot(data = dat, aes(x = Tleaf, y=Photo, color=type)) + 
    geom_point(size=0.6) +
    my_theme +
    xlab("Leaf temperature (°C)") +
    ylab("Assimilation rate (relative units)") +
    facet_grid(noise.level ~ type)
  
  # Save plot to file
  svg("figures/figS3.svg", width = 4, height = 6) 
  grid.arrange(p)
  dev.off()
}