# Header for Faster script

library(parallel) 
library(tidyverse)
library(nls.multstart)
library(gridExtra)
library(ggplot2)
library(cowplot)
library(nlme)
library(rstatix)

source("code/build_database.R") 
source("code/read_6800.R") 
source("code/calc_licor6800.R") 
source("code/match_correct.R") 
source("code/noneq_correct_full.R") 
source("code/fit_curves_parallel.R") 
source("code/pawar_modified.R")

source("code/make_tab1.R")   # Table showing statistical test results

source("code/make_fig1.R")   # Illustrates the basic method
source("code/make_fig2.R")   # Illustrates IRGA drift correction 
source("code/make_fig3.R")   # Illustrates non-equilibrium correction
source("code/make_fig4.R")   # Comparison of parameter estimates
source("code/make_fig5.R")   # Effect of noise and data density of parameters

source("code/make_tabS1.R")   # Ramp speed supplemental test
source("code/make_tabS2.R")   # Gsw comparison

source("code/make_figS1.R")  # Linearity of match offset accumulation with T
source("code/make_figS2.R")  # Raw data
source("code/make_figS3.R")  # Noise illustration

# Basic plotting theme
my_theme = theme_bw() + 
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
