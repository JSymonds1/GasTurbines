# Program to run an analysis of a gas turbine engine, producing a full set of static
# pressures, static temperatures, fuel-air ratios

engineCondi <- function (M0=2.0, p0=10, T0=228, yc=1.4, Cpc=1004, PRd=0.88, PRc=12,
                         ec=0.9, TR1=8.0, QR=42000000, AEb=0.98, PRb=0.95, yt=1.33,
                         Cpt=1156, et=0.82, AEm=0.995, TR1AB=11, QRAB = 42000000,
                         AEAB=0.98, PRAB=0.93, yAB=1.30, CpAB=1243, PRn=0.93, p9="p0",Afterburn=TRUE){
  if(p9=="p0"){p9=p0}
  
  
  R=287.05          #Defining gas constant R, assuming gas is air, in J/kgK
  
  #speed of sound at stage 0 and air speed velocity
  a0 = sqrt((yc-1)*Cpc*T0)
  V0 = M0*a0
  
  #Stage 0 "atmosphere" and 2 "Diffuser exit" conditions
  Tt0 = T0*(1+(yc-1)*M0^2/2)
  pt0 = p0*(1+(yc-1)*M0^2/2)^(yc/(yc-1))
  s0= 0 #Arbitrary starting entropy value of 0.

  Tt2 = Tt0
  pt2 = pt0*PRd
  s2=s0+Cpc*log(Tt2/Tt0)-R*log(pt2/pt0)
  
  # Total temperature ratio for the compressor
  TRc = PRc^((yc-1)/(ec*yc))
  
  # Stage 3 "compressor exit" conditions
  Tt3 = Tt2*TRc 
  pt3 = pt2*PRc
  s3=s2+Cpc*log(Tt3/Tt2)-R*log(pt3/pt2)
  
  # Stage 4 "burner exit" conditions
  Tt4 = Cpc*T0*TR1/Cpt
  pt4 = pt3*PRb
  s4=s3+Cpt*log(Tt4/Tt3)-R*log(pt4/pt3)
  f = (Cpt*Tt4-Cpc*Tt3)/(QR*AEb-Cpt*Tt4)
  
  # Stage 5 "turbine exit" conditions
  Tt5 = Tt4 - Cpc*(Tt3-Tt2)/(Cpt*AEm*(1+f))
  
  TRt=Tt5/Tt4
  PRt=TRt^(yt/((yt-1)*et))
  
  pt5 = pt4*PRt
  s5=s4+Cpt*log(Tt5/Tt4)-R*log(pt5/pt4)
  
  #Predefining Tt7, pt7 and fAB
  Tt7=0
  pt7=0
  fAB=0
  s7=0
  
  if(Afterburn){
  
    # Stage 7 "afterburner exit" conditions
    Tt7 = Cpc*T0*TR1AB/CpAB
    pt7 = pt5*PRAB

    fAB = ((1+f)*(CpAB*Tt7 - Cpt*Tt5))/(QRAB*AEAB-CpAB*Tt7)
    s7=s5+Cpt*log(Tt7/Tt5)-R*log(pt7/pt5)
  
    # Stage 9 "nozzle exit" conditions
    Tt9 = Tt7
    pt9 = pt7*PRn
    s9=s7+Cpt*log(Tt9/Tt7)-R*log(pt9/pt7)
    
    M9 = sqrt(2/(yAB-1)*((pt9/p9)^((yAB-1)/yAB)-1))
    T9 = Tt9/(1+(yAB-1)*M9^2/2)
    a9 = sqrt((yAB-1)*CpAB*T9)
    V9 = M9*a9
  
    #Specific thrust and thrust specific fuel consumption.  
    FnSpecM = (1+f+fAB)*V9-V0
    
    FnSpecN =(1+f+fAB)*V9/a0-M0
    
    TSFC = (f+fAB)/(FnSpecM)*10^6       #The *10^6 converts the kg value to mg per assignment
    
    #Efficiency, thermal and proulsive
    Eth = ((1+f+fAB)*V9^2/2-V0^2/2)/(f*QR+fAB*QRAB)
    
    Epr = (FnSpecM*V0)/((1+f+fAB)*V9^2/2-V0^2/2)
  
  } else {
    # Stage 9 "nozzle exit" conditions
    Tt9 = Tt5
    pt9 = pt5*PRn
    s9=s9+Cpt*log(Tt9/Tt5)-R*log(pt9/pt5)
    
    M9 = sqrt(2/(yt-1)*((pt9/p9)^((yt-1)/yt)-1))
    T9 = Tt9/(1+(yt-1)*M9^2/2)
    a9 = sqrt((yt-1)*Cpt*T9)
    V9 = M9*a9
    
    #Specific thrust and thrust specific fuel consumption.  
    FnSpecM = (1+f)*V9-V0
    
    FnSpecN =(1+f)*V9/a0-M0
    
    TSFC = (f)/(FnSpecM)
    
    #Efficiency, thermal and proulsive
    Eth = ((1+f)*V9^2/2-V0^2/2)/(f*QR)
    
    Epr = (FnSpecM*V0)/((1+f)*V9^2/2-V0^2/2)
  }
  #Creating the data frame
  
 Conditions<- data.frame(pt0,Tt0,s0,pt2,Tt2,s2,pt3,Tt3,s3,pt4,Tt4,s4,pt5,Tt5,s5,pt7,Tt7,s7,pt9,Tt9,
                         s9,f,fAB,FnSpecN,TSFC,Eth, Epr)
 colnames(Conditions) <- c('pt0','Tt0','s0','pt2','Tt2','s2','pt3','Tt3','s3','pt4','Tt4','s4',
                           'pt5','Tt5','s5','pt7','Tt7','s7','pt9','Tt9','s9','FuelAir',
                           'FuelAirAfterburn', 'NondimensionalSpecificThrust','TSFC',
                           'ThermalEfficiency', 'PropulsiveEfficiency')
 Conditions
  ##################################End of engineCondi####################################
}

