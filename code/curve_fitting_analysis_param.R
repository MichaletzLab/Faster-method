

# Let's start by binding everything together

AT_all = bind_rows(AT_chamber, AT_faster, AT_insitu, AT_step)

library(rTPC)
library(nlme)
library(nls.multstart)

a = nls_multstart(A ~ pawar_2018(Tleaf, r_tref, e, eh, topt, tref=10),
            data = subset(AT_all, curveID == 39),
            iter=1000,
            start_lower = c(r_tref = 4, e = 0.1, eh = 0.5, topt = 20),
            start_upper = c(r_tref = 10, e = 1.5, eh = 2, topt = 30),
            supp_errors = "Y"
            )

# Let's fit pawar_2018 to extract E and topt
# Let's fit gaussian_1987 for width and Amax
# Let's try boarman_2017 for Ctmin and Ctmax

# Iterate through all curves
# Fit each model to the curve
# Extract parameters of interest
# build a dataframe

tpc.params = c()

for (i in unique(AT_all$curveID)) {
  print(i)
  curdat = subset(AT_all, curveID == i)
  
 # if(i == 56) {next}
  # Fit pawar_2018
  print("pawar")
  start_vals = get_start_vals(curdat$Tleaf, curdat$A, model_name = 'pawar_2018')
  start_vals_low = c(r_tref = 5, e = 0.1, eh = 0.1, topt = 20)
  start_vals_high = c(r_tref = 30, e = 2, eh = 2, topt = 30)
  mod.pawar = nls_multstart(A ~ pawar_2018(Tleaf, r_tref, e, eh, topt, tref = 10),
                            data = curdat,
                            iter = 100,
                            start_lower = start_vals_low,
                            start_upper = start_vals_high,
                            supp_errors = "Y")
  
  # Fit gaussian_1987
  print("gauss")
  start_vals <- get_start_vals(curdat$Tleaf, curdat$A, model_name = 'gaussian_1987')
  start_vals_low = c(rmax = 5, topt = 20, a = 5)
  start_vals_high = c(rmax = 30, topt = 30, a = 30)
  mod.gaussian = nls_multstart(A ~ gaussian_1987(Tleaf, rmax, topt, a),
                               data = curdat,
                               iter = 100,
                               start_lower = start_vals-10,
                               start_upper = start_vals+10,
                               supp_errors = "Y")
  
  # Fit briere2_1999
  print("briere")
  start_vals <- get_start_vals(curdat$Tleaf, curdat$A, model_name = 'briere2_1999')
  mod.briere = nls_multstart(A ~ briere2_1999(Tleaf, tmin, tmax, a, b),
                              data = curdat,
                              iter = 100,
                              start_lower = start_vals-10,
                              start_upper = start_vals+10,
                              supp_errors = "Y")
  
  tpc.params = bind_rows(tpc.params,
            data.frame(curveID = curdat$curveID[1],
                       taxon = curdat$taxon[1],
                       replicate = curdat$replicate[1],
                       method = curdat$method[1],
                       e = coef(mod.pawar)["e"],
                       topt = coef(mod.pawar)["topt"],
                       amax = coef(mod.gaussian)["rmax"],
                       breadth = coef(mod.gaussian)["a"],
                       ctmin = coef(mod.briere)["tmin"],
                       ctmax = coef(mod.briere)["tmax"])
            )
}

AT_all$E = NULL
results = fit_curves_michaletz_2021(AT_all, x = "Tleaf", y = "A", T_ref = 10, PLOT = T)
