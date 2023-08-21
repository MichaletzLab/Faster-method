# New formulation of Sharpe-Schoolfield model with explicit Amax parameter

# Here, I have written this to mask the pawar_2018 function from rTPC, so that
# I can easily slot this into existing code without rewriting.
# When using this function, tref no longer does anything, and rtref is now
# interpreted as r_max or Amax. See Supplementary information Notes S1.

pawar_2018 = function (temp, r_tref, e, eh, topt, tref) {
  tref <- 273.15 + tref
  k <- 8.62e-05
  boltzmann.term <- r_tref * (1 + (e/(eh-e))) * exp(e/k * (1/(topt+273.15) - 1/(temp + 273.15)))
  inactivation.term <- 1/(1 + (e/(eh - e)) * exp(eh/k * (1/(topt + 
                                                            273.15) - 1/(temp + 273.15))))
  return(boltzmann.term * inactivation.term)
}
