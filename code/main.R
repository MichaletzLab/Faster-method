# Script to generate figures and statistics for methods paper describing
# Fast Assimilation-Temperature Response (FAsTeR) curves.

# Last modified 2 March 2023, Josef Garen

# Load libraries, etc.
source("code/header.R")

# Create directory for figures, clear stats
if(!dir.exists("figures")) { dir.create("figures") }
if(file.exists("stats.txt")) { file.remove("stats.txt") }

# Load data
data.all = build.database()

# Make table
make_tab1(data.all) # Anova table with parameter estimates

# Generate figures
make_fig1(data.all) # Illustration of the method
make_fig2(data.all) # Correcting for IRGA drift
make_fig3(data.all) # Correcting for non-equilibrium effects
make_fig4(data.all) # Comparison of parameter estimates
make_fig5(data.all) # Effects of noise and data density on parameter estimates

# Supplement
make_figS1()         # Linearity of match offset accumulation
make_figS2(data.all) # Raw data
make_figS3(data.all) # Noise illustration

#######
# END #
#######
           