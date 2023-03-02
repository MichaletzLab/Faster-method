# Function to recalculate LI-6800 values using standard models
# modified to exclude recalculation of boundary layer conductance
calc_licor6800 = function(licor) {
  
  x = licor$Fan_speed*licor$Pa / (1000*licor$blc_Po)
  y = pmax(pmin(licor$S, licor$blc_maxS) , licor$blc_minS)
  
  licor %>% 
    mutate(
      # gbw = ifelse(Geometry == "0: Broadleaf",
      #              # Then apply broadleaf formula.
      #              blc_a + blc_b*x + blc_c*x*y*y + blc_d*x*y + blc_e*x*x,
      #              # Otherwise, check if needle geometry is specified...
      #              ifelse(Geometry == "1: Needle",
      #                     # Then apply needle value.
      #                     3,
      #                     # Otherwise, apply custom BLC value
      #                     Custom
      #              )
      # ),
      E = Flow * CorrFact * (H2O_s-H2O_r)/(100*S*(1000-CorrFact*H2O_s)),
      A = Flow * CorrFact * (CO2_r-CO2_s*(1000-CorrFact*H2O_r)/(1000-CorrFact*H2O_s))/(100*S),
      Ca = CO2_s - ifelse(CorrFact>1, A*S*100 , 0),
      Rabs = Qin * convert,
      VPcham = H2O_s * (Pa+`ΔPcham`)/1000,
      SVPcham = 0.61365 * exp(17.502*Tair/(240.97+Tair)),
      RHcham = VPcham/SVPcham*100,
      #TleafEB = (Tair+(Rabs+2*0.95*0.0000000567*(((Tair+deltaTw)+273)^4 - (Tair+273)^4)-44100*E)/(1.84*29.3*gbw+8*0.95*0.0000000567*(Tair+273)^3)),
      #TleafCnd = fT1*Tleaf + fT2*Tleaf2 + fTeb*TleafEB,
      TleadCnd = Tleaf,
      SVPleaf = 0.61365*exp(17.502*TleafCnd/(240.97+TleafCnd)),
      VPDleaf = (SVPleaf-H2O_s*(Pa+`ΔPcham`)/1000),
      LatHFlux = -E*44100,
      SenHFlux = 2*29.3*gbw*0.92*(Tair-TleafCnd),
      #NetTherm = 2*0.95*0.0000000567*(((Tair+deltaTw)+273)^4-(TleafCnd+273)^4),
      #EBSum = Rabs+NetTherm+LatHFlux+SenHFlux,
      gtw = E*(1000-(1000*0.61365*exp(17.502*TleafCnd/(240.97+TleafCnd))/(Pa+`ΔPcham`)+H2O_s)/2)/(1000*0.61365*exp(17.502*TleafCnd/(240.97+TleafCnd))/(Pa+`ΔPcham`)-H2O_s),
      gsw = 2 / ((1/gtw - 1/gbw) + sign(gtw)*sqrt((1/gtw - 1/gbw)^2 + 4*K/((K+1)^2)*(2/(gtw*gbw) - 1/(gbw^2)))),
      gtc = 1 / ((K+1)/(gsw/1.6)+1/(gbw/1.37)) + K/((K+1)/(gsw/1.6) + K/(gbw/1.37)),
      Ci = ((gtc-E/2)*Ca-A)/(gtc+E/2),
      Pci = Ci*(Pa+`ΔPcham`)/1000,
      Pca = (CO2_s - ifelse(CorrFact>1, A*S*100/(Fan*Fan_speed), 0)) * (Pa+`ΔPcham`)/1000
    )
  
}

