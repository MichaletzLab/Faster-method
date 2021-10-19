# Exploratory analysis of Faster data

library(tidyverse)

source("code/read_6800.R")

all_files = list.files("data/data_validation", recursive = T, full.names = T) %>% 
  grep("\\.", ., value = T, invert = T)

# Problem with file 86 - the raw file is bad, but the excel looks ok
# maybe try downloading it again
data_raw = lapply(all_files[c(1:85,87:109)], read_6800)


data_unpack = c()
for (i in 1:length(data_raw)) {
  #data_unpack = rbind.fill(data_unpack, data.frame(rasa_data_raw[i], curveID = i))
  data_unpack = bind_rows(data_unpack, data.frame(data_raw[i], curveID = i))
}

# So we have 5 methods here, and they all maybe need to be treated somewhat
# differently.
#
# lrc - light response
# step - "standard" method
# chamber - standard method in chamber
# faster - new ramping method (each of these three can be treated roughly equal)
# racir - this is the acixt method, which will require some modelling
# insitu - as it says

# Now let's separate out the data so we can apply different schemes as needed.
data_unpack$taxon = NA
data_unpack$replicate = data_unpack$filename %>% str_match("r[0-9]+") %>% str_extract("[0-9]+")
data_unpack[grep("rasa", data_unpack$filename),]$taxon = "RASA"
data_unpack[grep("tito", data_unpack$filename),]$taxon = "TITO"

LRC = data_unpack[grep("lrc", data_unpack$filename),]

AT_step = data_unpack[grep("step", data_unpack$filename),]
AT_chamber = data_unpack[grep("chamber", data_unpack$filename),]
AT_faster = data_unpack[grep("faster_r", data_unpack$filename),] 

AT_insitu = data_unpack[grep("insitu", data_unpack$filename),]

RACiR = data_unpack[grep("racir", data_unpack$filename),]

AT_insitu$replicate = as.character(AT_insitu$leafRep)

# Things to do:
# Add columns for species, method, rep
# Try fitting curves and comparing parameter estimates
# You'll probably want to do some NLMEs at some point
# LRC safe to ignore for now, just nice to have
# RACiR curves need a fair amount of processing - is there a package?

AT_chamber$method = "chamber" # OK
AT_step$method    = "step"    # OK
AT_faster$method  = "faster"  # OK
AT_insitu$method  = "insitu"  # OK

source("code/curve_fitting_michaletz_2021.R")

fits_step = fit_curves_michaletz_2021(Data = AT_step %>% select(-E), x = "Tleaf", y = "A", T_ref = 10, PLOT = T)
fits_faster = fit_curves_michaletz_2021(Data = AT_faster %>% select(-E), x = "Tleaf", y = "A", T_ref = 10, PLOT = T)


# Let's learn how to use the racir package
library(racir)


# This section automatically corrects the ACi curves
racir_corr = c()
for(fn in unique(RACiR$filename)) {
  
  if(str_detect(fn, "blank")) {
    # set this one as the current blank cal data 
    caldat = subset(RACiR, filename == fn)[-1:-40,]
  } else {
    # set this one as the current racir data
    racdat = subset(RACiR, filename == fn)[-1:-40,]
    corrdat = racircal(data = racdat, caldata = caldat)
    
    racir_corr = bind_rows(racir_corr, corrdat)
  }
}

# Now we need to extract the kinetics parameters
library(plantecophys)

extracted = fitacis(data = racir_corr, 
           group = "curveID",
           id = "filename",
           varnames = list(ALEAF = "Acor", Tleaf = "Tleaf", Ci = "Cicor", PPFD = "Qin", Rd = "Rd"),
           Tcorrect = F,
           fitmethod = "default")


kinetics_data = c()
for (i in 1:length(extracted)) {
  if(i==32) { next }
  vcmax = extracted[[i]]$pars[1]
  jmax = extracted[[i]]$pars[2]
  rd = extracted[[i]]$pars[3]
  tleaf = mean(extracted[[i]]$df$Tleaf)
  filename = extracted[[i]]$df$filename[1]
  kinetics_data = bind_rows(kinetics_data,
                  data.frame(filename, vcmax, jmax, rd, tleaf) )
}




# OK so I've noticed that I'm missing some RASA RACiR curves.... I wonder where they have gone


# Now we need to break down the filename into taxon an replicate
kinetics_data$taxon = NA
kinetics_data[kinetics_data$filename %>% grep("rasa", .),]$taxon = "RASA"
kinetics_data[kinetics_data$filename %>% grep("tito", .),]$taxon = "TITO"

kinetics_data$replicate = kinetics_data$filename %>% str_match("r[0-9]+") %>% str_extract("[0-9]+")

kinetics_data = kinetics_data %>% select(-filename)

