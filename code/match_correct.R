# Function to perform match offset correction
match_correct = function(data) {

  return_data = c()

  for (i in unique(data$curveID)) {

    # Grab each curve in our dataset, get length
    a = subset(data, curveID == i)
    len = dim(a)[1]

    # Check that the match test point exists
    if(a$co2_adj[1] == a$co2_adj[len]) {
      warning(paste("Warning: curveID", i, "appears to have no match test point."))
    }

    # Linearly interpolate match offset by Txchg value
    correction_co2 = (a$Txchg-a$Txchg[1])*(a$co2_adj[len]-a$co2_adj[len-1])/(a$Txchg[len]-a$Txchg[1])
    correction_h2o = (a$Txchg-a$Txchg[1])*(a$h2o_adj[len]-a$h2o_adj[len-1])/(a$Txchg[len]-a$Txchg[1])
    
    # Add to data frame
    a$correction_co2 = correction_co2
    a$correction_h2o = correction_h2o

    # Change sample CO2 and H2O values using new correction
    b=a
    b$CO2_s = b$CO2_s + b$correction_co2
    b$H2O_s = b$H2O_s + b$correction_h2o

    # Recalculate gas exchange variables and drop test point
    b = b %>% calc_licor6800()
    b = b[1:len-1,]

    # Bind together
    return_data = bind_rows(return_data, b)

  }

  return(return_data)
}
