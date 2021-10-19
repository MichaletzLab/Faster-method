library(tidyverse)
library(ggplot2)

source("read_6800.R")

source("curve_fitting_michaletz_2021.R")



all_files = list.files("humidity conrol tests", recursive = T, full.names = T) %>% 
  grep(".xlsx", ., value = T, invert = T)
data_raw = lapply(all_files, read_6800)


data_unpack = c()
for (i in 1:length(data_raw)) {
  #data_unpack = rbind.fill(data_unpack, data.frame(rasa_data_raw[i], curveID = i))
  data_unpack = bind_rows(data_unpack, data.frame(data_raw[i], curveID = i))
}

data_unpack$control_method = NA
data_unpack[grep("const_vpd", data_unpack$filename),]$control_method = "VPD"
data_unpack[grep("const_rh", data_unpack$filename),]$control_method = "RH"
data_unpack[grep("const_h2or", data_unpack$filename),]$control_method = "H2O_R"




# Raw AT curve comparison
ggplot(data = data_unpack, aes(x = Tleaf, y = A, group = curveID, color = control_method)) +
  geom_point() + geom_path()


  
data_unpack = data_unpack %>% group_by(curveID) %>% mutate(A_norm = A/max(A))

# Normalized AT curve comparison
ggplot(data = data_unpack, aes(x = Tleaf, y = A_norm, group = curveID, color = control_method)) +
  geom_point() + geom_path()


# gs by T comparison
ggplot(data = data_unpack, aes(x = Tleaf, y = gsw, group = curveID, color = control_method)) +
  geom_point() + geom_path()

data_unpack = data_unpack %>% group_by(curveID) %>% mutate(gsw_norm = gsw/max(gsw))

# Normalized gs by T comparison
ggplot(data = data_unpack, aes(x = Tleaf, y = gsw_norm, group = curveID, color = control_method)) +
  geom_point() + geom_path()


# Ci by T comparison
ggplot(data = data_unpack, aes(x = Tleaf, y = Ci, group = curveID, color = control_method)) +
  geom_point() + geom_path()

data_unpack = data_unpack %>% group_by(curveID) %>% mutate(Ci_norm = Ci/max(Ci))

ggplot(data = data_unpack, aes(x = Tleaf, y = Ci_norm, group = curveID, color = control_method)) +
  geom_point() + geom_path()


# VPD by T comparison
ggplot(data = data_unpack, aes(x = Tleaf, y = VPDleaf, group = curveID, color = control_method)) +
  geom_point() + geom_path()


data_short = data_unpack %>% select(curveID, control_method, Tleaf, A)

fits = fit_curves_michaletz_2021(data_short, x = "Tleaf", y = "A", T_ref = 10, PLOT=T)

fits %>% group_by(control_method) %>% summarize(mean(E_D))

fits = subset(fits, curveID !=13)
std <- function(x) sd(x)/sqrt(length(x))
ci = function(x) 1.96*sd(x)/sqrt(length(x))
