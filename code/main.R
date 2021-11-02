# main process flow for faster method validation


# libraries
library(tidyverse)
library(racir)
library(plantecophys)
library(nls.multstart)
library(rTPC)

source("code/read_6800.R")
source("code/build.database.R")
source("code/process.racir.R")
source("code/fit.pawar.R")
source("code/fit.quadratic.R")
source("code/fit.medlyn.R")


# load all data
data.all  = build.database()

# Generate simulated AT data from racir curves
data.all$AT_FvCB = process.racir(data.all$RACiR)

# Make AT dataframe
data.AT = bind_rows(data.all$AT_step, data.all$AT_chamber, data.all$AT_insitu, data.all$AT_faster, data.all$AT_FvCB)
data.AT$curveID = data.AT %>%  group_by(species, rep, method) %>% group_indices()
data.AT = data.AT %>% select(curveID, species, rep, method, A, Tleaf)

# Let's also do one with insitu combined
data.AT = bind_rows(data.AT, subset(data.AT, method == "insitu" & species == "RASA") %>% mutate(curveID = 101, method = "insitu_combined", rep = "1"))
data.AT = bind_rows(data.AT, subset(data.AT, method == "insitu" & species == "TITO") %>% mutate(curveID = 101, method = "insitu_combined", rep = "1"))

# Drop TITO for now because the data is all shitty
data.cut = subset(data.AT, species == "RASA") 

# Do the curve fitting necessary to extract  parameters of interest
pawar.params = fit.pawar(data.cut)
quad.params = fit.quadratic(data.cut)

# Post-processing/post-filtering

# Quick visual check for QC
pdf("test.pdf")
for ( i in unique(data.cut$curveID)) {
  #i=1
  curdat = subset(data.cut, curveID == i)
  curpawar = subset(pawar.params, curveID == i)
  curquad = subset(quad.params, curveID == i)
  plot(curdat$Tleaf, curdat$A, main = i)
  lines(1:50, pawar_2018(1:50, curpawar$r_tref, curpawar$e, curpawar$eh, curpawar$topt, curpawar$tref), col="red")
  lines(1:50, quadratic.model(1:50, curquad$a, curquad$b, curquad$c), col="blue")
  
  
}
dev.off()

write.csv(data.cut, "data_cut.csv", row.names = F)
write.csv(pawar.params, "pawar_params.csv", row.names = F)
write.csv(quad.params, "quad_params.csv", row.names = F)

data.cut = read.csv("data_cut.csv")
pawar.params = read.csv("pawar_params.csv")
quad.params = read.csv("quad_params.csv")

# Analysis, stats, and figures

# Perhaps here we can do some NLME business to determine if there are differences between methods
library(nlme)
data.cut$method = as.factor(data.cut$method)
data.cut.cut = subset(data.cut, method == "step" | method == "chamber" | method == "faster")

data.small = subset(data.cut.cut, method == "faster")
data.small = data.small[seq(1,dim(data.small)[1], 10),]
data.small = bind_rows(data.small, subset(data.cut.cut, method != "faster"))

photo.nlme.1 = nlme(A ~ pawar_2018(Tleaf, r_tref, e, eh, topt, tref = 10),
                    data = data.small,
                    fixed = r_tref + e + eh + topt ~ 1,
                    groups = ~ curveID,
                    start = c(r_tref = 25, e = 0.71, eh = 1.46, topt = 25),
                    na.action = na.omit,
                    method = "ML",
                    verbose = T)

photo.nlme.2 = update(photo.nlme.1, 
                      fixed = r_tref + e + eh + topt ~ method - 1,
                      start = c(30,30,30, 0.63,0.63,0.63, 1.1,1.1,1.1, 25.5,25.5,25.5),
                      verbose = T)

photo.nlme.3 = update(photo.nlme.1,
                      fixed = list(r_tref + e + eh ~ 1, topt ~ method),
                      start = c(29,0.63, 1.1,25.5,25.5,25.5))


# Compute normalized A
data.cut = data.cut %>% group_by(species, method, rep) %>% mutate(Arel = A/max(A))

ggplot(data = data.cut, aes(x=Tleaf,y=A,color=method)) +
         geom_point()

ggplot(data = data.cut, aes(x=Tleaf,y=Arel,color=method)) +
  geom_point()

ggplot(data = data.cut.cut, aes(x=Tleaf,y=Arel,color=method, group = curveID)) +
  geom_line()

# Let's do a sample non-equilibrium correction
non.eq.AT = subset(data.all$AT_faster, curveID == 24)


V = 85*(1e6/1)*(1/1000)*(1/22.4) # 85 cm ^ 3 converted to umol
n = dim(non.eq.AT)[1] #number of obs
S = non.eq.AT$S[2:n] # cm^2
uo = non.eq.AT$Flow[2:n] # umol/s
wo = non.eq.AT$H2O_s[2:n] #mmol/mol
we = non.eq.AT$H2O_r[2:n] #mmol/mol
dwdt = ( non.eq.AT$H2O_r[2:n] - non.eq.AT$H2O_r[1:(n-1)] ) / ( non.eq.AT$time[2:n] - non.eq.AT$time[1:(n-1)] )

# This is the expression for E in umol/cm2s
E_corr_umol_cm2s = (uo/S)*(wo-we)/(1000-wo) + V*dwdt/(S*(1000-wo))
# Convert to mol/m2s (used in licor)
E_corr = E_corr_umol_cm2s*(1e4)*(1/1e6)

# Now use this to correct A
co = non.eq.AT$CO2_s[2:n] #umol/mol
ce = non.eq.AT$CO2_r[2:n] #umol/mol
dcdt = ( non.eq.AT$CO2_r[2:n] - non.eq.AT$CO2_r[1:(n-1)] ) / ( non.eq.AT$time[2:n] - non.eq.AT$time[1:(n-1)] )

A_corr_umol2_cm2smol = (uo/S)*(ce-co) - E_corr_umol_cm2s*co - V*dcdt/S
# Convert to umol/m2s (standard)
A_corr = A_corr_umol2_cm2smol*(1/1e6)*(1e4)
  

#non.eq.AT$A_corr = A_corr

data.plot = data.frame(A = c(non.eq.AT$A, A_corr),
                       Tleaf = c(non.eq.AT$Tleaf, non.eq.AT$Tleaf[2:n]),
                       Condition = c(rep("Uncorrected", n), rep("Corrected", n-1)))
  

library(ggplot2)

ggplot(data = data.plot, aes(x=Tleaf, y=A, color=Condition)) +
  geom_point()
  
  

                          