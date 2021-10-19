# Curve fitting master code for activation energy project. This file includes functions describing
# Arrhenius and Sharpe-Schoolfield temperature response functions, a function which assigns each
# curveID to be fit with either Arrhenius or SS (or neither) based on specified criteria, and a
# curve-fitting function which fits the assigned curve type to each dataset using nls.multstart,
# then plots the results if desired.

# Last modified Josef Garen 2 March 2021

library(nls.multstart)
library(ggplot2)
library(broom)
library(purrr)
library(dplyr)
library(tidyr)
library(nlstools)
library(scales)
library(gridExtra)



# Returns Sharpe-Schoolfield function for the given parameters
Schoolfield <- function(lnB0, E, E_D, T_h, temp, SchoolTpk=TRUE) { 
  # temp   : Temperature values to evaluate at (C)
  # lnB0   : Normalisation constant (units depend on rate; log transformed)
  # E      : Activation energy (eV; E > 0)
  # E_D    : High temperature de-activation energy (eV, E_D > 0) 
  # T_h    : Temperature at which trait reaches peak value (K, Tpk)   

  k <- 8.62e-5        # Boltzmann's constant (eV K-1)
  temp = temp+273.15  # Convert T to Kelvin
  
  return(lnB0 + log(exp((-E/k) * ((1/temp)))/(1 + (E/(E_D - E)) * exp(E_D/k * (1/T_h - 1/temp)))))
}



# Returns Arrhenius function for the given parameters
Arrhenius <- function(lnB0, E, temp) { 
  # lnB0   : Normalisation constant (units depend on rate; log transformed)
  # E      : Activation energy (eV; E > 0)
  # temp   : Temperature values to evaluate function at (C)
  
  k <- 8.62e-5       # Boltzmann's constant (eV K-1)
  temp = temp+273.15 # Convert to Kelvin
  
  return(lnB0 + (-E/(k*temp))) # Evaluate and return
}



# In the given dataset, assigns each curveID to be fit with Arrhenius, Schoolfield, or neither
assign_curve_type <- function(data) {
  
  filter_criteria = c() # Holder
  
  #new_dat = c() ###
  # Find metrics for filtering for each curve
  for (i in unique(data$curveID)) {
    
    cur_dat = subset(data, curveID == i) # Grab the current data set
    Tpk = cur_dat$Tleaf[which( cur_dat$Photo == max(cur_dat$Photo) )[1]] # Find peak
    #cur_dat = subset(cur_dat, Tleaf <= Tpk) ###
    
    nbefore = sum( cur_dat$Tleaf < Tpk ) # Find number of points before peak
    nafter = sum( cur_dat$Tleaf > Tpk )  # Find number of points before peak
    Tbefore = Tpk-min(cur_dat$Tleaf)     # Find temperature span before peak
    
    filter_criteria = rbind(filter_criteria, data.frame(curveID = i, nbefore, nafter, Tbefore)) 
    
    #new_dat = rbind(new_dat, cur_dat) ###
  }
  
  # Merge filtering metrics with dataset for ease of filtering
  data = merge(data, filter_criteria, by="curveID")
  
  data$curve_type = NA
  data$failure_status = NA
  
  # Reject curves if the are decreasing only, have too little T span, or too few points
  data$failure_status = ifelse(data$nbefore < 2, "Rejected: fewer than two points before peak", data$failure_status)
  data$failure_status = ifelse(data$Tbefore < 4, "Rejected: ascending region too small", data$failure_status)
  data$failure_status = ifelse(data$nbefore+data$nafter < 4, "Rejected: fewer than 5 data points", data$failure_status)
  
  # Assign curve type based on whether or not they have an identifiable peak, or NA if failed above
  data$curve_type = ifelse(data$nbefore > 0 & data$nafter > 0, "Schoolfield", data$curve_type)
  data$curve_type = ifelse(data$nafter < 1, "Arrhenius", data$curve_type)
  data$curve_type = ifelse(!is.na(data$failure_status), NA, data$curve_type)
  
  data = data %>% select(-nbefore, -nafter, -Tbefore)  # Remove junk
  return(data)
}



# Fit curves, plot, and return dataframe with best fit parameters
fit_curves <- function(Data, x = "Tleaf", y = "Photo", PLOT=TRUE) { 
  
  set.seed(1)
  
  # Standardize names of x and y for use throughout
  if(x != "Tleaf") {
    Data$Tleaf = Data[[x]] # Rename X as Tleaf
    Data[[x]] = NULL      # Get rid of originals
  }
  if(y != "Photo") {
    Data$Photo = Data[[y]] # Rename Y as Photo
    Data[[y]] = NULL      # Get rid of originals
  }
  
  # Open output file if plotting desired
  if(PLOT==TRUE)
    grDevices::pdf("plots_all.pdf", width = 10, height = 5)
  
  Data = assign_curve_type(Data)
  Data = Data %>% arrange(by_group = curveID)
  results = c()
  
  ######################
  # Curve fitting loop #
  ######################
  for (i in unique(Data$curveID)) {
    
    print(paste(i, "/", as.character(max(Data$curveID)), "complete"))
    
    d_1 <- subset(Data, curveID == i)    # Grab only current curveID
    d_1 <-subset(d_1, Photo > 0)         # Drop all values with Y < 0 
    
    if (is.na(d_1$curve_type)) {     # If no curve should be fit, all NAs
    lnB0 = lnB0_SE = E = E_SE = E_D = E_D_SE = T_h = T_h_SE = AIC = r_sq = NA
    
    } else if(dim(d_1)[1] < 5) {     # If not enough positive points, all NAs and set failure status
      lnB0 = lnB0_SE = E = E_SE = E_D = E_D_SE = T_h = T_h_SE = AIC = r_sq = NA
      d_1$failure_status = "Rejected: fewer than 5 data points with Y > 0"
      d_1$curve_type = NA
        
    } else if(d_1$curve_type == "Schoolfield") { # If curve type is Schoolfield, fit one
    
      # Fit Schoolfield function using nls_multstart
      fit <- nls_multstart(Photo ~ exp(Schoolfield(lnB0, E, E_D, T_h, temp = Tleaf)),
                          data = d_1,
                          iter = 1000,
                          start_lower = c(lnB0 = 0, E = 0, E_D = 0.2, T_h = 285),
                          start_upper = c(lnB0 = 100, E = 4, E_D = 8, T_h = 330),
                          supp_errors = 'Y',
                          na.action = na.omit,
                          lower = c(lnB0 = -10, E = 0, E_D = 0, T_h = 0))
    
      if(typeof(fit) != "NULL") { # If the fit was successful, extract parameters estimates and SE
        lnB0 <- coef(fit)["lnB0"]
        lnB0_SE <- summary(fit)$coefficients[,'Std. Error']['lnB0']
        E <- coef(fit)["E"]
        E_SE <- summary(fit)$coefficients[,'Std. Error']['E']
        E_D <- coef(fit)["E_D"]
        E_D_SE <- summary(fit)$coefficients[,'Std. Error']['E_D']
        T_h <- coef(fit)["T_h"]
        T_h_SE <- summary(fit)$coefficients[,'Std. Error']['T_h']
        AIC <- AIC(fit)
        r_sq = 1-sum(resid(fit)^2)/sum((d_1$Photo-mean(d_1$Photo))^2)
        
        if (T_h < min(d_1$Tleaf)+273.15) {
          d_1$failure_status = "Poor fit: T_opt out of bounds"
        }
        
      } else { # Otherwise, likely a convergence failure. All set to NA
        lnB0 = lnB0_SE = E = E_SE = E_D = E_D_SE = T_h = T_h_SE = AIC = r_sq = NA
        d_1$failure_status = "Convergence failure"
      }
      
    } else if(d_1$curve_type == "Arrhenius") { # If curve type is Arrhenius, fit one
      
      # Fit Arrhenius function using nls_multstart
      fit <- nls_multstart(Photo ~ exp(Arrhenius(lnB0, E, temp = Tleaf)),
                           data = d_1,
                           iter = 1000,
                           start_lower = c(lnB0 = 0, E = 0),
                           start_upper = c(lnB0 = 100, E = 4),
                           supp_errors = 'Y',
                           na.action = na.omit,
                           lower = c(lnB0 = -10, E = 0))
      
      
      if(typeof(fit) != "NULL") { # If the fit was successful, extract parameters estimates and SE
        lnB0 <- coef(fit)["lnB0"]
        lnB0_SE <- summary(fit)$coefficients[,'Std. Error']['lnB0']
        E <- coef(fit)["E"]
        E_SE <- summary(fit)$coefficients[,'Std. Error']['E']
        E_D = E_D_SE = T_h = T_h_SE = NA
        AIC <- AIC(fit)
        r_sq = 1-sum(resid(fit)^2)/sum((d_1$Photo-mean(d_1$Photo))^2)
        
      } else { # Otherwise, likely a convergence failure. All set to NA
        lnB0 = lnB0_SE = E = E_SE = E_D = E_D_SE = T_h = T_h_SE = AIC = r_sq = NA
        d_1$failure_status = "Convergence failure"
      }
    }
      
      
      ##########################
      # Plot curves if desired #
      ##########################
      if (PLOT==TRUE) {
        
        # Pick a set of temperature values to compute model over
        tmp_temps <- seq(min(floor(d_1$Tleaf)), ceiling(max(d_1$Tleaf)), length = 200)
        
        # Compute model predictions depending on model type and select curve color
        if(is.na(d_1$curve_type[1])) {
          tmp_model = mean(d_1$Photo)
          color_model = "white"
        } else if(d_1$curve_type[1] == "Schoolfield") {
          tmp_model <- exp(Schoolfield(lnB0, E, E_D, T_h, tmp_temps))
          color_model = "red"
        } else if(d_1$curve_type[1] == "Arrhenius") {
          tmp_model <- exp(Arrhenius(lnB0, E, tmp_temps))
          color_model = "blue"
        }
        
        # Assemble model predicitons and raw data to plot
        ModelToPlotS <- data.frame(Temperature = tmp_temps, TraitValue = tmp_model)
        DataToPlot <- na.omit(data.frame(Temperature = d_1$Tleaf, TraitValue = d_1$Photo))
        
        # Build plots - normal
        p <- ggplot() + 
          geom_point(data = DataToPlot, aes(x = Temperature, y = TraitValue), 
                     shape = 21, size = 2.75, col = "black", fill = "white") + 
          geom_line(data = ModelToPlotS,aes(x = Temperature, y = TraitValue), colour = color_model) +                           
          ggtitle(paste("curveID=", i, ", ", d_1$Taxon[1], sep="")) +
          xlab(expression(paste("Leaf temperature (", degree, C, ")"))) + 
          ylab(expression("Assimilation rate (" * mu ~ "mol" ~m^-2 ~s^-1 * ")")) +
          theme_bw(base_size=12) +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                axis.line = element_line(colour = "black")) +
          annotate("text", size = 3, 
                   label=paste("R^2 = ", sprintf("%.2f", r_sq),"\nE =", format(E, digits = 3),"\nAIC =",format(AIC,digits=3), "\nCurve type = ", d_1$curve_type[1]), 
                   x = min(DataToPlot[, "Temperature"]),
                   y = mean(DataToPlot[, "TraitValue"]),
                   hjust=0,
                   fontface = 3)
        
        # Build plots - Arrhenius plot
        p3 <- ggplot() + 
          geom_point(data = DataToPlot, aes(x = 1/(0.00008617*(Temperature + 273.15)), y = TraitValue), 
                     shape = 21, size = 2.75, col = "black", fill = "white") + 
          geom_line(data = ModelToPlotS, aes(x = 1/(0.00008617*(Temperature + 273.15)), y = TraitValue), 
                    colour = color_model) +                           
          ggtitle(paste("curveID=", i, ", ", d_1$Taxon[1], sep="")) +
          xlab(expression(paste('Leaf temperature ', '1/',italic('kT'), ' (',  eV^{-1}, ')'))) +
          ylab(expression("Assimilation rate (" * mu ~ "mol" ~m^-2 ~s^-1 * ")")) +
          scale_x_continuous(sec.axis = sec_axis(trans = ~ (1/(.*0.00008617))-273.15 , name = expression(paste("Leaf temperature (", degree, C, ")")))) +
          scale_y_continuous(trans="log", breaks = trans_breaks("log", function(x) exp(x), n=3), 
                             labels = trans_format("log", math_format(e^.x))) +
          theme_bw(base_size=12) +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                axis.line = element_line(colour = "black"))
      
        grid.arrange(p,p3,nrow=1) # Arrange plots next to each other
      } # End plotting
      
    # Build dataframe with results
    results = rbind(results, data.frame(d_1[1,] %>% select(-Tleaf, -Photo), E, E_SE, lnB0, 
                                 lnB0_SE, E_D, E_D_SE, T_h, T_h_SE, AIC, r_sq))
  }
  
  # Close output file
  if(PLOT==TRUE)
    grDevices::dev.off()
  
  return(results)
}
